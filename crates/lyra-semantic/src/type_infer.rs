use lyra_lexer::SyntaxKind;
use lyra_parser::SyntaxNode;
use smol_str::SmolStr;

use crate::coerce::{
    IntegralCtx, OpCategory, apply_outer_context, coerce_integral, comparison_context, op_spec,
    operator_result_self,
};
use crate::enum_def::EnumId;
use crate::expr_helpers::{find_binary_op, find_operator_token, is_expression_kind};
use crate::literal::parse_literal_shape;
use crate::member::{BuiltinMethodKind, MemberInfo, MemberKind, MemberLookupError};
use crate::symbols::{GlobalSymbolId, SymbolKind};
use crate::syntax_helpers::system_tf_name;
use crate::types::{
    ConstEvalError, ConstInt, Integral, IntegralKw, PackedDim, PackedDims, RealKw, SymbolType, Ty,
};

/// Signedness for bit-vector expression typing.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Signedness {
    Signed,
    Unsigned,
}

/// Bit width: known, unknown, context-dependent, or error.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BitWidth {
    Known(u32),
    Unknown,
    /// Width adapts to assignment context (LRM 11.6). No payload -- the
    /// width is unknown until context supplies it. The "default 1-bit in
    /// self-determined context" rule (LRM 5.7.1) is handled by
    /// `self_determined()`, not by storing a fallback here.
    ContextDependent,
    /// Width computation failed (distinct from Unknown, which means
    /// "not yet evaluated").
    Error,
}

impl BitWidth {
    /// Resolve to a concrete width in self-determined context.
    ///
    /// `ContextDependent` -> 1 bit per LRM 5.7.1.
    /// Call this only when the expression is genuinely in a self-determined
    /// position (concat/replication operands, shift RHS, logical operands).
    pub fn self_determined(&self) -> Option<u32> {
        match self {
            BitWidth::Known(w) => Some(*w),
            BitWidth::ContextDependent => Some(1),
            BitWidth::Unknown | BitWidth::Error => None,
        }
    }
}

/// A bit-vector value type (integral expressions).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BitVecType {
    pub width: BitWidth,
    pub signed: Signedness,
    /// True for 4-state types (logic/reg/integer/time or x/z-containing literals).
    pub four_state: bool,
}

impl BitVecType {
    /// Human-readable representation. Uses `logic` (4-state) or `bit`
    /// (2-state) depending on `four_state`.
    pub fn pretty(&self) -> SmolStr {
        let mut s = String::from(if self.four_state { "logic" } else { "bit" });
        if self.signed == Signedness::Signed {
            s.push_str(" signed");
        }
        match self.width {
            BitWidth::Known(1) | BitWidth::ContextDependent => {}
            BitWidth::Known(w) => {
                use core::fmt::Write;
                let _ = write!(s, " [{}:0]", w - 1);
            }
            BitWidth::Unknown | BitWidth::Error => {
                s.push_str(" [?:?]");
            }
        }
        SmolStr::new(s)
    }
}

/// Reasons an expression's type could not be determined.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExprTypeErrorKind {
    Unresolved,
    NameRefIsTypeNotValue,
    UnsupportedExprKind,
    UnsupportedLiteralKind,
    UnsupportedBinaryOp,
    UnsupportedSystemCall,
    NonBitOperand,
    ConcatNonBitOperand,
    CondBranchTypeMismatch,
    InvalidReplicationCount,
    ReplicationConstEvalFailed(ConstEvalError),
    IndexNonIndexable,
    PartSelectNonIntegral,
    PartSelectBoundsNonConst,
    PartSelectWidthNonConst,
    NoMembersOnReceiver,
    UnknownMember,
    MemberNotInModport,
    MethodRequiresCall,
    MethodArityMismatch,
    MethodArgNotIntegral,
    UserCallUnsupported,
    UnsupportedCalleeForm(CalleeFormKind),
    UnresolvedCall,
    NotACallable(SymbolKind),
    TaskInExprContext,
}

/// How an expression's type is viewed for operations.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprView {
    /// Integral bit-vector view (logic/int/bit/etc).
    BitVec(BitVecType),
    /// Non-bit-vector type (real, string, struct, etc.).
    Plain,
    /// Type could not be determined.
    Error(ExprTypeErrorKind),
}

/// Result of typing an expression (self-determined).
///
/// Every typed expression always carries a `Ty`. `view` provides the
/// operational interpretation: bit-vector for integral types, plain for
/// non-integral types, or error.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExprType {
    pub ty: Ty,
    pub view: ExprView,
}

impl ExprType {
    /// Convert a `Ty` to `ExprType`. The only `Ty` -> `ExprType` conversion path.
    pub fn from_ty(ty: &Ty) -> ExprType {
        match ty {
            Ty::Integral(i) => {
                let width = match i.try_packed_width() {
                    Some(w) => BitWidth::Known(w),
                    None => BitWidth::Unknown,
                };
                let signed = if i.signed {
                    Signedness::Signed
                } else {
                    Signedness::Unsigned
                };
                let four_state = i.keyword.four_state();
                ExprType {
                    ty: ty.clone(),
                    view: ExprView::BitVec(BitVecType {
                        width,
                        signed,
                        four_state,
                    }),
                }
            }
            Ty::Real(_)
            | Ty::String
            | Ty::Chandle
            | Ty::Event
            | Ty::Void
            | Ty::Enum(_)
            | Ty::Record(_)
            | Ty::Interface(_)
            | Ty::Array { .. } => ExprType {
                ty: ty.clone(),
                view: ExprView::Plain,
            },
            Ty::Error => ExprType::error(ExprTypeErrorKind::Unresolved),
        }
    }

    /// Map `SymbolType` to `ExprType`.
    pub fn from_symbol_type(st: &SymbolType) -> ExprType {
        match st {
            SymbolType::Value(ty) => ExprType::from_ty(ty),
            SymbolType::Net(net) => ExprType::from_ty(&net.data),
            SymbolType::TypeAlias(_) => ExprType::error(ExprTypeErrorKind::NameRefIsTypeNotValue),
            SymbolType::Error(_) => ExprType::error(ExprTypeErrorKind::Unresolved),
        }
    }

    /// Construct an error `ExprType`.
    pub fn error(kind: ExprTypeErrorKind) -> ExprType {
        ExprType {
            ty: Ty::Error,
            view: ExprView::Error(kind),
        }
    }

    /// Construct a bit-vector `ExprType` from raw `BitVecType`.
    pub fn bitvec(bv: BitVecType) -> ExprType {
        ExprType {
            ty: Ty::Integral(Integral {
                keyword: if bv.four_state {
                    IntegralKw::Logic
                } else {
                    IntegralKw::Bit
                },
                signed: bv.signed == Signedness::Signed,
                packed: match bv.width {
                    BitWidth::Known(w) if w > 1 => PackedDims::from(vec![PackedDim {
                        msb: ConstInt::Known(i64::from(w) - 1),
                        lsb: ConstInt::Known(0),
                    }]),
                    _ => PackedDims::empty(),
                },
            }),
            view: ExprView::BitVec(bv),
        }
    }

    /// Unsized decimal literal default: `BitVec(32, Signed, 2-state)`.
    pub fn int_literal() -> ExprType {
        ExprType::bitvec(BitVecType {
            width: BitWidth::Known(32),
            signed: Signedness::Signed,
            four_state: false,
        })
    }

    /// Sized bit-vector (2-state by default).
    pub fn bits(width: u32, signed: Signedness) -> ExprType {
        ExprType::bitvec(BitVecType {
            width: BitWidth::Known(width),
            signed,
            four_state: false,
        })
    }

    /// Single-bit unsigned (reduction/logical results).
    pub fn one_bit() -> ExprType {
        ExprType::bits(1, Signedness::Unsigned)
    }

    /// Human-readable type representation.
    pub fn pretty(&self) -> SmolStr {
        match &self.view {
            ExprView::BitVec(bv) => bv.pretty(),
            ExprView::Plain => self.ty.pretty(),
            ExprView::Error(_) => SmolStr::new_static("<error>"),
        }
    }
}

/// Category of unsupported callee syntax (for future diagnostic messages).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CalleeFormKind {
    MethodCall,
    Other,
}

/// Error resolving a callable name.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ResolveCallableError {
    NotFound,
    NotACallable(SymbolKind),
}

/// Signature of a callable, passed through inference by the DB layer.
///
/// This is the source of truth for call-site type checking. The DB layer
/// normalizes the raw `CallableSig` types (evaluating `ConstInt::Unevaluated`
/// dims) and builds this lightweight projection that `lyra-semantic` can
/// consume without depending on `lyra-db`.
pub struct CallableSigRef {
    pub kind: CallableKind,
    pub return_ty: Ty,
    pub ports: Box<[CallablePort]>,
}

/// Whether a callable is a function or task.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallableKind {
    Function,
    Task,
}

/// Port info needed for call checking. Contains only what inference uses.
#[derive(Debug, Clone)]
pub struct CallablePort {
    pub name: SmolStr,
    pub ty: Ty,
    pub has_default: bool,
}

/// Callbacks for the inference engine. No DB access -- pure.
pub trait InferCtx {
    /// Resolve a `NameRef`/`QualifiedName` to its type.
    fn type_of_name(&self, name_node: &SyntaxNode) -> ExprType;
    /// Evaluate a constant expression (for replication count).
    fn const_eval(&self, expr_node: &SyntaxNode) -> ConstInt;
    /// Resolve a callee name node to a callable symbol.
    fn resolve_callable(
        &self,
        callee_node: &SyntaxNode,
    ) -> Result<GlobalSymbolId, ResolveCallableError>;
    /// Get the signature of a callable symbol.
    fn callable_sig(&self, sym: GlobalSymbolId) -> Option<CallableSigRef>;
    /// Look up a member (field) on a type.
    fn member_lookup(&self, ty: &Ty, member_name: &str) -> Result<MemberInfo, MemberLookupError>;
    /// Get the integral view of an enum's base type.
    fn enum_integral_view(&self, id: &EnumId) -> Option<BitVecType>;
    /// Resolve a `NameRef` node as a type (typedef/enum/struct name).
    /// Used by system functions like `$bits` that accept type arguments.
    fn resolve_type_arg(&self, name_node: &SyntaxNode) -> Option<Ty>;
}

/// Extract an integral view from an `ExprType`, auto-casting enums to
/// their base integral type when needed for arithmetic/bitwise operations.
fn try_integral_view(et: &ExprType, ctx: &dyn InferCtx) -> Option<BitVecType> {
    match &et.view {
        ExprView::BitVec(bv) => Some(*bv),
        ExprView::Plain => {
            if let Ty::Enum(id) = &et.ty {
                ctx.enum_integral_view(id)
            } else {
                None
            }
        }
        ExprView::Error(_) => None,
    }
}

/// Infer the type of an expression node, optionally in a context.
///
/// When `expected` is `None`, returns the self-determined type.
/// When `expected` is `Some(ctx)`, propagates context sizing per LRM 11.6.
pub fn infer_expr_type(
    expr: &SyntaxNode,
    ctx: &dyn InferCtx,
    expected: Option<&IntegralCtx>,
) -> ExprType {
    match expr.kind() {
        SyntaxKind::NameRef | SyntaxKind::QualifiedName => ctx.type_of_name(expr),
        SyntaxKind::Literal => infer_literal(expr, expected),
        SyntaxKind::Expression | SyntaxKind::ParenExpr => match expr.first_child() {
            Some(child) => infer_expr_type(&child, ctx, expected),
            None => ExprType::error(ExprTypeErrorKind::UnsupportedExprKind),
        },
        SyntaxKind::PrefixExpr => infer_prefix(expr, ctx, expected),
        SyntaxKind::BinExpr => infer_binary(expr, ctx, expected),
        SyntaxKind::CondExpr => infer_cond(expr, ctx, expected),
        SyntaxKind::ConcatExpr => infer_concat(expr, ctx),
        SyntaxKind::ReplicExpr => infer_replic(expr, ctx),
        SyntaxKind::IndexExpr => infer_index(expr, ctx),
        SyntaxKind::RangeExpr => infer_range(expr, ctx),
        SyntaxKind::FieldExpr => infer_field_access(expr, ctx),
        SyntaxKind::CallExpr | SyntaxKind::SystemTfCall => infer_call(expr, ctx),
        SyntaxKind::StreamExpr => infer_stream(expr, ctx),
        _ => ExprType::error(ExprTypeErrorKind::UnsupportedExprKind),
    }
}

fn infer_literal(node: &SyntaxNode, expected: Option<&IntegralCtx>) -> ExprType {
    let lit_kind = node
        .children_with_tokens()
        .filter_map(|el| el.into_token())
        .map(|t| t.kind())
        .find(|k| {
            matches!(
                k,
                SyntaxKind::TimeLiteral
                    | SyntaxKind::RealLiteral
                    | SyntaxKind::StringLiteral
                    | SyntaxKind::IntLiteral
                    | SyntaxKind::BasedLiteral
                    | SyntaxKind::UnbasedUnsizedLiteral
            )
        });

    match lit_kind {
        Some(SyntaxKind::TimeLiteral) => ExprType::from_ty(&Ty::Real(RealKw::Time)),
        Some(SyntaxKind::RealLiteral) => ExprType::from_ty(&Ty::Real(RealKw::Real)),
        Some(SyntaxKind::StringLiteral) => ExprType::from_ty(&Ty::String),
        Some(SyntaxKind::UnbasedUnsizedLiteral) => {
            let has_xz = node
                .children_with_tokens()
                .filter_map(|el| el.into_token())
                .find(|t| t.kind() == SyntaxKind::UnbasedUnsizedLiteral)
                .is_some_and(|t| t.text().chars().any(|c| matches!(c, 'x' | 'X' | 'z' | 'Z')));
            let self_ty = BitVecType {
                width: BitWidth::ContextDependent,
                signed: Signedness::Unsigned,
                four_state: has_xz,
            };
            if let Some(ectx) = expected {
                ExprType::bitvec(coerce_integral(&self_ty, ectx))
            } else {
                ExprType::bitvec(self_ty)
            }
        }
        _ => match parse_literal_shape(node) {
            Some(shape) => {
                let signed = if shape.signed {
                    Signedness::Signed
                } else {
                    Signedness::Unsigned
                };
                ExprType::bitvec(BitVecType {
                    width: BitWidth::Known(shape.width),
                    signed,
                    four_state: shape.has_xz,
                })
            }
            None => ExprType::error(ExprTypeErrorKind::UnsupportedLiteralKind),
        },
    }
}

fn infer_prefix(node: &SyntaxNode, ctx: &dyn InferCtx, expected: Option<&IntegralCtx>) -> ExprType {
    let Some(op) = find_operator_token(node) else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };

    let Some(operand_node) = node.children().find(|c| is_expression_kind(c.kind())) else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };

    match op {
        // Logical negation: always 1-bit unsigned, operand self-determined
        SyntaxKind::Bang
        // Reduction operators: always 1-bit unsigned, operand self-determined
        | SyntaxKind::Amp
        | SyntaxKind::Pipe
        | SyntaxKind::Caret
        | SyntaxKind::TildeAmp
        | SyntaxKind::TildePipe
        | SyntaxKind::TildeCaret
        | SyntaxKind::CaretTilde => {
            let _operand = infer_expr_type(&operand_node, ctx, None);
            ExprType::one_bit()
        }
        // Bitwise NOT or unary +/-: pass context to operand
        SyntaxKind::Tilde | SyntaxKind::Plus | SyntaxKind::Minus => {
            let operand = infer_expr_type(&operand_node, ctx, expected);
            match &operand.view {
                ExprView::BitVec(_) | ExprView::Error(_) => operand,
                ExprView::Plain => {
                    if let Some(bv) = try_integral_view(&operand, ctx) {
                        ExprType::bitvec(bv)
                    } else {
                        ExprType::error(ExprTypeErrorKind::NonBitOperand)
                    }
                }
            }
        }
        _ => ExprType::error(ExprTypeErrorKind::UnsupportedExprKind),
    }
}

fn infer_binary(node: &SyntaxNode, ctx: &dyn InferCtx, expected: Option<&IntegralCtx>) -> ExprType {
    let children: Vec<SyntaxNode> = node
        .children()
        .filter(|c| is_expression_kind(c.kind()))
        .collect();
    if children.len() < 2 {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    }

    let Some(op) = find_binary_op(node) else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };

    // Step 1-2: self-determined types of both operands
    let lhs_self = infer_expr_type(&children[0], ctx, None);
    let rhs_self = infer_expr_type(&children[1], ctx, None);

    let spec = op_spec(op);

    // Check for non-bit operands (auto-cast enums to their base integral type)
    if matches!(lhs_self.view, ExprView::Error(_)) {
        return lhs_self;
    }
    if matches!(rhs_self.view, ExprView::Error(_)) {
        return rhs_self;
    }
    let Some(lhs_bv) = try_integral_view(&lhs_self, ctx) else {
        return ExprType::error(ExprTypeErrorKind::NonBitOperand);
    };
    let Some(rhs_bv) = try_integral_view(&rhs_self, ctx) else {
        return ExprType::error(ExprTypeErrorKind::NonBitOperand);
    };

    // Step 3: operator result (self-determined)
    let result_self = operator_result_self(spec.category, spec.result_state, &lhs_bv, &rhs_bv);

    // Step 4: combine with outer context
    let result_ctx = apply_outer_context(spec.category, &result_self, expected);

    // Step 5: re-infer operands with context (branched by category)
    match spec.category {
        OpCategory::ContextBoth => {
            infer_expr_type(&children[0], ctx, Some(&result_ctx));
            infer_expr_type(&children[1], ctx, Some(&result_ctx));
        }
        OpCategory::ShiftLike => {
            infer_expr_type(&children[0], ctx, Some(&result_ctx));
            // RHS is self-determined
        }
        OpCategory::Comparison => {
            let cmp_ctx = comparison_context(&lhs_bv, &rhs_bv);
            infer_expr_type(&children[0], ctx, Some(&cmp_ctx));
            infer_expr_type(&children[1], ctx, Some(&cmp_ctx));
        }
        OpCategory::Logical => {
            // Both operands are self-determined
        }
    }

    // Step 6: return the coerced result
    ExprType::bitvec(coerce_integral(&result_self, &result_ctx))
}

fn infer_cond(node: &SyntaxNode, ctx: &dyn InferCtx, expected: Option<&IntegralCtx>) -> ExprType {
    let children: Vec<SyntaxNode> = node
        .children()
        .filter(|c| is_expression_kind(c.kind()))
        .collect();
    if children.len() < 3 {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    }

    // Condition is always self-determined
    let _cond = infer_expr_type(&children[0], ctx, None);

    // Self-determined types of both arms
    let then_ty = infer_expr_type(&children[1], ctx, None);
    let else_ty = infer_expr_type(&children[2], ctx, None);

    match (&then_ty.view, &else_ty.view) {
        (ExprView::BitVec(a), ExprView::BitVec(b)) => {
            // Compute common type from arms, then merge with outer context
            let merged = merge_bitvec(a, b);
            let arm_ctx = if let Some(outer) = expected {
                let width = match (merged.width.self_determined(), outer.width) {
                    (Some(mw), Some(ow)) => Some(mw.max(ow)),
                    (Some(mw), None) => Some(mw),
                    (None, w) => w,
                };
                IntegralCtx {
                    width,
                    signed: if merged.signed == Signedness::Signed
                        && outer.signed == Signedness::Signed
                    {
                        Signedness::Signed
                    } else {
                        Signedness::Unsigned
                    },
                    four_state: merged.four_state || outer.four_state,
                }
            } else {
                IntegralCtx {
                    width: merged.width.self_determined(),
                    signed: merged.signed,
                    four_state: merged.four_state,
                }
            };

            // Re-infer arms with context
            infer_expr_type(&children[1], ctx, Some(&arm_ctx));
            infer_expr_type(&children[2], ctx, Some(&arm_ctx));

            ExprType::bitvec(coerce_integral(&merged, &arm_ctx))
        }
        (ExprView::Plain, ExprView::Plain) if then_ty.ty == else_ty.ty => then_ty,
        (ExprView::Error(_), _) => then_ty,
        (_, ExprView::Error(_)) => else_ty,
        _ => ExprType::error(ExprTypeErrorKind::CondBranchTypeMismatch),
    }
}

fn infer_concat(node: &SyntaxNode, ctx: &dyn InferCtx) -> ExprType {
    let mut total_width: Option<u32> = Some(0);
    let mut all_known = true;
    let mut any_four_state = false;

    for child in node.children() {
        if !is_expression_kind(child.kind()) {
            continue;
        }
        let child_ty = infer_expr_type(&child, ctx, None);
        if let ExprView::Error(_) = &child_ty.view {
            return child_ty;
        }
        let Some(bv) = try_integral_view(&child_ty, ctx) else {
            return ExprType::error(ExprTypeErrorKind::ConcatNonBitOperand);
        };
        any_four_state = any_four_state || bv.four_state;
        match bv.width.self_determined() {
            Some(w) => {
                if let Some(ref mut tw) = total_width {
                    *tw = tw.saturating_add(w);
                }
            }
            None => {
                all_known = false;
            }
        }
    }

    let width = if all_known {
        BitWidth::Known(total_width.unwrap_or(0))
    } else {
        BitWidth::Unknown
    };

    ExprType::bitvec(BitVecType {
        width,
        signed: Signedness::Unsigned,
        four_state: any_four_state,
    })
}

fn infer_stream(node: &SyntaxNode, ctx: &dyn InferCtx) -> ExprType {
    let Some(operands) = node
        .children()
        .find(|c| c.kind() == SyntaxKind::StreamOperands)
    else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };

    let mut total_width: u32 = 0;
    let mut all_known = true;
    let mut any_four_state = false;

    for child in operands.children() {
        if !is_expression_kind(child.kind()) {
            continue;
        }
        let child_ty = infer_expr_type(&child, ctx, None);
        if let ExprView::Error(_) = &child_ty.view {
            return child_ty;
        }
        let Some(bv) = try_integral_view(&child_ty, ctx) else {
            return ExprType::error(ExprTypeErrorKind::ConcatNonBitOperand);
        };
        any_four_state = any_four_state || bv.four_state;
        match bv.width.self_determined() {
            Some(w) => total_width = total_width.saturating_add(w),
            None => all_known = false,
        }
    }

    let width = if all_known {
        BitWidth::Known(total_width)
    } else {
        BitWidth::Unknown
    };

    ExprType::bitvec(BitVecType {
        width,
        signed: Signedness::Unsigned,
        four_state: any_four_state,
    })
}

fn infer_replic(node: &SyntaxNode, ctx: &dyn InferCtx) -> ExprType {
    let children: Vec<SyntaxNode> = node.children().collect();

    let expr_children: Vec<SyntaxNode> = children
        .iter()
        .filter(|c| is_expression_kind(c.kind()))
        .cloned()
        .collect();

    if expr_children.is_empty() {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    }

    // First expression child is the replication count
    let count_node = &expr_children[0];
    let count = ctx.const_eval(count_node);

    let replic_count = match count {
        ConstInt::Known(n) => {
            if n <= 0 {
                return ExprType::error(ExprTypeErrorKind::InvalidReplicationCount);
            }
            u32::try_from(n).ok()
        }
        ConstInt::Error(e) => {
            return ExprType::error(ExprTypeErrorKind::ReplicationConstEvalFailed(e));
        }
        ConstInt::Unevaluated(_) => None,
    };

    // Remaining children are the inner items (to be concatenated)
    let inner_items = &expr_children[1..];

    // Check for ConcatExpr child
    let concat_child = children.iter().find(|c| c.kind() == SyntaxKind::ConcatExpr);

    let (inner_width, inner_four_state) = if let Some(concat) = concat_child {
        let inner = infer_concat(concat, ctx);
        if let ExprView::Error(_) = &inner.view {
            return inner;
        }
        let Some(bv) = try_integral_view(&inner, ctx) else {
            return ExprType::error(ExprTypeErrorKind::ConcatNonBitOperand);
        };
        (bv.width, bv.four_state)
    } else if inner_items.is_empty() {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    } else {
        // Sum widths of inner items
        let mut total: Option<u32> = Some(0);
        let mut all_known = true;
        let mut any_four_state = false;
        for item in inner_items {
            let item_ty = infer_expr_type(item, ctx, None);
            if let ExprView::Error(_) = &item_ty.view {
                return item_ty;
            }
            let Some(bv) = try_integral_view(&item_ty, ctx) else {
                return ExprType::error(ExprTypeErrorKind::ConcatNonBitOperand);
            };
            any_four_state = any_four_state || bv.four_state;
            match bv.width.self_determined() {
                Some(w) => {
                    if let Some(ref mut t) = total {
                        *t = t.saturating_add(w);
                    }
                }
                None => all_known = false,
            }
        }
        let width = if all_known {
            BitWidth::Known(total.unwrap_or(0))
        } else {
            BitWidth::Unknown
        };
        (width, any_four_state)
    };

    let result_width = match (replic_count, inner_width.self_determined()) {
        (Some(n), Some(w)) => BitWidth::Known(n.saturating_mul(w)),
        _ => BitWidth::Unknown,
    };

    ExprType::bitvec(BitVecType {
        width: result_width,
        signed: Signedness::Unsigned,
        four_state: inner_four_state,
    })
}

/// What kind of indexing operation `a[i]` performs on a given base type.
enum IndexKind {
    /// Base is `Ty::Array` -- peel outermost unpacked dimension.
    UnpackedElem,
    /// Base is integral with >= 2 packed dims -- peel outermost packed dim.
    PackedArrayDim,
    /// Base is integral with <= 1 packed dim -- bit-select (1-bit result).
    BitSelect,
}

fn classify_index(ty: &Ty) -> Result<IndexKind, ExprTypeErrorKind> {
    match ty {
        Ty::Array { .. } => Ok(IndexKind::UnpackedElem),
        Ty::Integral(i) if i.packed.len() >= 2 => Ok(IndexKind::PackedArrayDim),
        Ty::Integral(_) => Ok(IndexKind::BitSelect),
        _ => Err(ExprTypeErrorKind::IndexNonIndexable),
    }
}

fn infer_index(node: &SyntaxNode, ctx: &dyn InferCtx) -> ExprType {
    let children: Vec<SyntaxNode> = node
        .children()
        .filter(|c| is_expression_kind(c.kind()))
        .collect();
    if children.len() < 2 {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    }

    let base = infer_expr_type(&children[0], ctx, None);
    if matches!(base.view, ExprView::Error(_)) {
        return base;
    }

    let idx = infer_expr_type(&children[1], ctx, None);
    if matches!(idx.view, ExprView::Error(_)) {
        return idx;
    }

    apply_index(&base)
}

/// Apply one index to a base type.
fn apply_index(base: &ExprType) -> ExprType {
    match classify_index(&base.ty) {
        Ok(IndexKind::UnpackedElem) => match base.ty.peel_unpacked_dim() {
            Some(elem) => ExprType::from_ty(&elem),
            None => ExprType::error(ExprTypeErrorKind::IndexNonIndexable),
        },
        Ok(IndexKind::PackedArrayDim) => match base.ty.peel_packed_array_dim() {
            Some(inner) => ExprType::from_ty(&inner),
            None => ExprType::error(ExprTypeErrorKind::IndexNonIndexable),
        },
        Ok(IndexKind::BitSelect) => match base.ty.bit_select_result() {
            Some(bit) => ExprType::from_ty(&bit),
            None => ExprType::error(ExprTypeErrorKind::IndexNonIndexable),
        },
        Err(e) => ExprType::error(e),
    }
}

fn infer_range(node: &SyntaxNode, ctx: &dyn InferCtx) -> ExprType {
    use lyra_ast::{AstNode, RangeExpr, RangeKind};

    let range_expr = RangeExpr::cast(node.clone());
    let range_kind = range_expr
        .as_ref()
        .map_or(RangeKind::Fixed, |r| r.range_kind());

    let children: Vec<SyntaxNode> = node
        .children()
        .filter(|c| is_expression_kind(c.kind()))
        .collect();
    if children.len() < 2 {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    }

    // Infer base expression
    let base = infer_expr_type(&children[0], ctx, None);
    if matches!(base.view, ExprView::Error(_)) {
        return base;
    }

    // Part-select valid only on integral types
    let four_state = match &base.ty {
        Ty::Integral(i) => i.keyword.four_state(),
        _ => return ExprType::error(ExprTypeErrorKind::PartSelectNonIntegral),
    };

    let width: u32 = match range_kind {
        RangeKind::IndexedPlus | RangeKind::IndexedMinus => {
            // Infer base index expr -- propagate errors
            let base_idx = infer_expr_type(&children[1], ctx, None);
            if matches!(base_idx.view, ExprView::Error(_)) {
                return base_idx;
            }
            // Width expr: infer for error propagation, then const-eval
            let width_node = &children[children.len() - 1];
            let width_et = infer_expr_type(width_node, ctx, None);
            if matches!(width_et.view, ExprView::Error(_)) {
                return width_et;
            }
            match ctx.const_eval(width_node) {
                ConstInt::Known(w) if w > 0 => match u32::try_from(w) {
                    Ok(v) => v,
                    Err(_) => return ExprType::error(ExprTypeErrorKind::PartSelectWidthNonConst),
                },
                _ => return ExprType::error(ExprTypeErrorKind::PartSelectWidthNonConst),
            }
        }
        RangeKind::Fixed => {
            if children.len() < 3 {
                return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
            }
            // Infer hi and lo exprs for error propagation, then const-eval
            let hi_et = infer_expr_type(&children[1], ctx, None);
            if matches!(hi_et.view, ExprView::Error(_)) {
                return hi_et;
            }
            let lo_et = infer_expr_type(&children[2], ctx, None);
            if matches!(lo_et.view, ExprView::Error(_)) {
                return lo_et;
            }
            let hi = ctx.const_eval(&children[1]);
            let lo = ctx.const_eval(&children[2]);
            match (hi, lo) {
                (ConstInt::Known(h), ConstInt::Known(l)) => {
                    let diff = (i128::from(h) - i128::from(l)).unsigned_abs() + 1;
                    match u32::try_from(diff) {
                        Ok(v) => v,
                        Err(_) => {
                            return ExprType::error(ExprTypeErrorKind::PartSelectBoundsNonConst);
                        }
                    }
                }
                _ => return ExprType::error(ExprTypeErrorKind::PartSelectBoundsNonConst),
            }
        }
    };

    // Result: unsigned integral with computed width
    let kw = if four_state {
        IntegralKw::Logic
    } else {
        IntegralKw::Bit
    };
    let packed = if width > 1 {
        PackedDims::from(vec![PackedDim {
            msb: ConstInt::Known(i64::from(width) - 1),
            lsb: ConstInt::Known(0),
        }])
    } else {
        PackedDims::empty()
    };
    ExprType::from_ty(&Ty::Integral(Integral {
        keyword: kw,
        signed: false,
        packed,
    }))
}

fn infer_field_access(node: &SyntaxNode, ctx: &dyn InferCtx) -> ExprType {
    use lyra_ast::{AstNode, FieldExpr};

    let Some(field_expr) = FieldExpr::cast(node.clone()) else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let Some(lhs) = field_expr.base_expr() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let Some(field_tok) = field_expr.field_name() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };

    let lhs_type = infer_expr_type(&lhs, ctx, None);
    if let ExprView::Error(_) = &lhs_type.view {
        return lhs_type;
    }

    match ctx.member_lookup(&lhs_type.ty, field_tok.text()) {
        Ok(info) => match info.kind {
            MemberKind::BuiltinMethod(_) => ExprType::error(ExprTypeErrorKind::MethodRequiresCall),
            _ => ExprType::from_ty(&info.ty),
        },
        Err(MemberLookupError::NoMembersOnType) => {
            ExprType::error(ExprTypeErrorKind::NoMembersOnReceiver)
        }
        Err(MemberLookupError::UnknownMember) => ExprType::error(ExprTypeErrorKind::UnknownMember),
        Err(MemberLookupError::NotInModport) => {
            ExprType::error(ExprTypeErrorKind::MemberNotInModport)
        }
    }
}

fn infer_call(node: &SyntaxNode, ctx: &dyn InferCtx) -> ExprType {
    // System task/function calls: $clog2, $signed, $bits, etc.
    if system_tf_name(node).is_some() {
        return crate::system_functions::infer_system_call(node, ctx);
    }

    // User-defined call: classify callee form
    let callee_node = match node.first_child() {
        Some(c) if is_expression_kind(c.kind()) => c,
        _ => return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind),
    };

    match callee_node.kind() {
        SyntaxKind::NameRef | SyntaxKind::QualifiedName => {}
        SyntaxKind::FieldExpr => {
            return infer_method_call(node, &callee_node, ctx);
        }
        _ => {
            return ExprType::error(ExprTypeErrorKind::UnsupportedCalleeForm(
                CalleeFormKind::Other,
            ));
        }
    }

    // Resolve callee to a callable symbol
    let sym_id = match ctx.resolve_callable(&callee_node) {
        Ok(id) => id,
        Err(ResolveCallableError::NotFound) => {
            return ExprType::error(ExprTypeErrorKind::UnresolvedCall);
        }
        Err(ResolveCallableError::NotACallable(kind)) => {
            return ExprType::error(ExprTypeErrorKind::NotACallable(kind));
        }
    };

    // Get callable signature
    let Some(sig) = ctx.callable_sig(sym_id) else {
        return ExprType::error(ExprTypeErrorKind::UnresolvedCall);
    };

    // Task used in expression context
    if sig.kind == CallableKind::Task {
        return ExprType::error(ExprTypeErrorKind::TaskInExprContext);
    }

    // Check arguments
    check_call_args(node, &sig, ctx);

    // Return the declared return type
    ExprType::from_ty(&sig.return_ty)
}

/// Check call arguments against the callable signature.
///
/// For now, infers each argument's type (for context-determined sizing)
/// but does not emit diagnostics for type mismatches. Arity and named-arg
/// validation is performed.
fn check_call_args(call_node: &SyntaxNode, sig: &CallableSigRef, ctx: &dyn InferCtx) {
    // Find ArgList child
    let arg_list = call_node
        .children()
        .find(|c| c.kind() == SyntaxKind::ArgList);

    let args: Vec<SyntaxNode> = arg_list
        .iter()
        .flat_map(|al| al.children())
        .filter(|c| is_expression_kind(c.kind()))
        .collect();

    // Infer each argument with expected type from the port signature
    for (i, arg) in args.iter().enumerate() {
        let expected_ctx = sig.ports.get(i).and_then(|p| {
            let ety = ExprType::from_ty(&p.ty);
            match ety.view {
                ExprView::BitVec(bv) => Some(IntegralCtx {
                    width: bv.width.self_determined(),
                    signed: bv.signed,
                    four_state: bv.four_state,
                }),
                _ => None,
            }
        });
        infer_expr_type(arg, ctx, expected_ctx.as_ref());
    }
}

fn infer_method_call(
    call_node: &SyntaxNode,
    callee_node: &SyntaxNode,
    ctx: &dyn InferCtx,
) -> ExprType {
    use lyra_ast::{AstNode, FieldExpr};

    let Some(field_expr) = FieldExpr::cast(callee_node.clone()) else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let Some(field_tok) = field_expr.field_name() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let Some(lhs_node) = field_expr.base_expr() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };

    let lhs_type = infer_expr_type(&lhs_node, ctx, None);
    if let ExprView::Error(_) = &lhs_type.view {
        return lhs_type;
    }

    match ctx.member_lookup(&lhs_type.ty, field_tok.text()) {
        Ok(MemberInfo {
            kind: MemberKind::BuiltinMethod(bm),
            ty,
            ..
        }) => infer_builtin_method_call(call_node, bm, &ty, ctx),
        Ok(_) | Err(MemberLookupError::NoMembersOnType) => ExprType::error(
            ExprTypeErrorKind::UnsupportedCalleeForm(CalleeFormKind::MethodCall),
        ),
        Err(MemberLookupError::UnknownMember) => ExprType::error(ExprTypeErrorKind::UnknownMember),
        Err(MemberLookupError::NotInModport) => {
            ExprType::error(ExprTypeErrorKind::MemberNotInModport)
        }
    }
}

fn infer_builtin_method_call(
    call_node: &SyntaxNode,
    bm: BuiltinMethodKind,
    result_ty: &Ty,
    ctx: &dyn InferCtx,
) -> ExprType {
    use lyra_ast::{AstNode, CallExpr};

    let args: Vec<SyntaxNode> = CallExpr::cast(call_node.clone())
        .and_then(|c| c.arg_list())
        .map(|al| al.args().collect())
        .unwrap_or_default();

    let (min, max) = match bm {
        BuiltinMethodKind::Enum(ek) => ek.arity(),
    };
    if args.len() < min || args.len() > max {
        return ExprType::error(ExprTypeErrorKind::MethodArityMismatch);
    }

    let requires_integral = match bm {
        BuiltinMethodKind::Enum(ek) => ek.arg_requires_integral(),
    };
    if requires_integral && let Some(arg_node) = args.first() {
        let arg_type = infer_expr_type(arg_node, ctx, None);
        if try_integral_view(&arg_type, ctx).is_none() {
            return ExprType::error(ExprTypeErrorKind::MethodArgNotIntegral);
        }
    }

    ExprType::from_ty(result_ty)
}

fn merge_bitvec(a: &BitVecType, b: &BitVecType) -> BitVecType {
    BitVecType {
        width: match (a.width.self_determined(), b.width.self_determined()) {
            (Some(wa), Some(wb)) => BitWidth::Known(wa.max(wb)),
            _ => BitWidth::Unknown,
        },
        signed: if a.signed == Signedness::Signed && b.signed == Signedness::Signed {
            Signedness::Signed
        } else {
            Signedness::Unsigned
        },
        four_state: a.four_state || b.four_state,
    }
}
