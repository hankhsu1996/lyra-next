mod range;

use lyra_ast::{
    AstNode, BinExpr, CallExpr, ConcatExpr, CondExpr, Expr, LiteralKind, PrefixExpr, ReplicExpr,
    StreamExpr, SystemTfCall,
};
use lyra_lexer::SyntaxKind;
use lyra_parser::SyntaxNode;
use smol_str::SmolStr;

use crate::coerce::{
    IntegralCtx, OpCategory, apply_outer_context, coerce_integral, comparison_context, op_spec,
    operator_result_self,
};
use crate::enum_def::EnumId;
use crate::literal::parse_literal_shape;
use crate::member::{MemberInfo, MemberKind, MemberLookupError, MethodInvalidReason};
use crate::symbols::{GlobalSymbolId, SymbolKind};
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
    SliceWidthInvalid,
    SliceNonSliceableArray,
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
    MethodNotValidOnReceiver(MethodInvalidReason),
    MethodArgTypeMismatch,
    MethodArgNotLvalue,
    MethodKeyTypeUnknown,
    VoidUsedAsExpr,
    CastTargetNotAType,
    InternalStreamOperandError,
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
    /// The file being analyzed.
    fn file_id(&self) -> lyra_source::FileId;
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
pub(crate) fn try_integral_view(et: &ExprType, ctx: &dyn InferCtx) -> Option<BitVecType> {
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
        SyntaxKind::Expression => {
            match lyra_ast::Expression::cast(expr.clone()).and_then(|e| e.inner()) {
                Some(inner) => infer_expr_type(inner.syntax(), ctx, expected),
                None => ExprType::error(ExprTypeErrorKind::UnsupportedExprKind),
            }
        }
        SyntaxKind::ParenExpr => match Expr::cast(expr.clone()).map(Expr::unwrap_parens) {
            Some(inner) => infer_expr_type(inner.syntax(), ctx, expected),
            None => ExprType::error(ExprTypeErrorKind::UnsupportedExprKind),
        },
        SyntaxKind::PrefixExpr => infer_prefix(expr, ctx, expected),
        SyntaxKind::BinExpr => infer_binary(expr, ctx, expected),
        SyntaxKind::CondExpr => infer_cond(expr, ctx, expected),
        SyntaxKind::ConcatExpr => infer_concat(expr, ctx),
        SyntaxKind::ReplicExpr => infer_replic(expr, ctx),
        SyntaxKind::IndexExpr => infer_index(expr, ctx),
        SyntaxKind::RangeExpr => range::infer_range(expr, ctx),
        SyntaxKind::FieldExpr => infer_field_access(expr, ctx),
        SyntaxKind::CallExpr | SyntaxKind::SystemTfCall => infer_call(expr, ctx),
        SyntaxKind::StreamExpr => infer_stream(expr, ctx),
        SyntaxKind::CastExpr => infer_cast(expr, ctx),
        _ => ExprType::error(ExprTypeErrorKind::UnsupportedExprKind),
    }
}

/// Infer the type of an expression in statement context.
///
/// Void methods are legal in statement context; `VoidUsedAsExpr` is
/// replaced with a successful `Ty::Void` result.
pub fn infer_expr_type_stmt(expr: &SyntaxNode, ctx: &dyn InferCtx) -> ExprType {
    let result = infer_expr_type(expr, ctx, None);
    if matches!(
        result.view,
        ExprView::Error(ExprTypeErrorKind::VoidUsedAsExpr)
    ) {
        ExprType::from_ty(&Ty::Void)
    } else {
        result
    }
}

fn infer_literal(node: &SyntaxNode, expected: Option<&IntegralCtx>) -> ExprType {
    let Some(literal) = lyra_ast::Literal::cast(node.clone()) else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedLiteralKind);
    };
    let Some(kind) = literal.literal_kind() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedLiteralKind);
    };
    match kind {
        LiteralKind::Time { .. } => ExprType::from_ty(&Ty::Real(RealKw::Time)),
        LiteralKind::Real { .. } => ExprType::from_ty(&Ty::Real(RealKw::Real)),
        LiteralKind::String { .. } => ExprType::from_ty(&Ty::String),
        LiteralKind::UnbasedUnsized { token } => {
            let has_xz = token
                .text()
                .chars()
                .any(|c| matches!(c, 'x' | 'X' | 'z' | 'Z'));
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
    let Some(pfx) = PrefixExpr::cast(node.clone()) else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let Some(op_tok) = pfx.op_token() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let op = op_tok.kind();

    let Some(operand_node) = pfx.operand() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let operand_node = operand_node.syntax().clone();

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
    let Some(bin) = BinExpr::cast(node.clone()) else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let Some(lhs_node) = bin.lhs() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let Some(rhs_node) = bin.rhs() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };

    let Some(op) = bin.binary_op() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };

    // Step 1-2: self-determined types of both operands
    let lhs_self = infer_expr_type(lhs_node.syntax(), ctx, None);
    let rhs_self = infer_expr_type(rhs_node.syntax(), ctx, None);

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
            infer_expr_type(lhs_node.syntax(), ctx, Some(&result_ctx));
            infer_expr_type(rhs_node.syntax(), ctx, Some(&result_ctx));
        }
        OpCategory::ShiftLike => {
            infer_expr_type(lhs_node.syntax(), ctx, Some(&result_ctx));
            // RHS is self-determined
        }
        OpCategory::Comparison => {
            let cmp_ctx = comparison_context(&lhs_bv, &rhs_bv);
            infer_expr_type(lhs_node.syntax(), ctx, Some(&cmp_ctx));
            infer_expr_type(rhs_node.syntax(), ctx, Some(&cmp_ctx));
        }
        OpCategory::Logical => {
            // Both operands are self-determined
        }
    }

    // Step 6: return the coerced result
    ExprType::bitvec(coerce_integral(&result_self, &result_ctx))
}

fn infer_cond(node: &SyntaxNode, ctx: &dyn InferCtx, expected: Option<&IntegralCtx>) -> ExprType {
    let Some(cond_expr) = CondExpr::cast(node.clone()) else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let Some(cond_node) = cond_expr.condition() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let Some(then_node) = cond_expr.then_expr() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let Some(else_node) = cond_expr.else_expr() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };

    // Condition is always self-determined
    let _cond = infer_expr_type(cond_node.syntax(), ctx, None);

    // Self-determined types of both arms
    let then_ty = infer_expr_type(then_node.syntax(), ctx, None);
    let else_ty = infer_expr_type(else_node.syntax(), ctx, None);

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
            infer_expr_type(then_node.syntax(), ctx, Some(&arm_ctx));
            infer_expr_type(else_node.syntax(), ctx, Some(&arm_ctx));

            ExprType::bitvec(coerce_integral(&merged, &arm_ctx))
        }
        (ExprView::Plain, ExprView::Plain) if then_ty.ty == else_ty.ty => then_ty,
        (ExprView::Error(_), _) => then_ty,
        (_, ExprView::Error(_)) => else_ty,
        _ => ExprType::error(ExprTypeErrorKind::CondBranchTypeMismatch),
    }
}

fn infer_concat(node: &SyntaxNode, ctx: &dyn InferCtx) -> ExprType {
    let Some(concat) = ConcatExpr::cast(node.clone()) else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let mut total_width: Option<u32> = Some(0);
    let mut all_known = true;
    let mut any_four_state = false;

    for child in concat.operands() {
        let child_ty = infer_expr_type(child.syntax(), ctx, None);
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

/// Pack-width accumulator with explicit merge semantics.
enum StreamWidth {
    Known(u32),
    Dynamic,
    Error,
}

impl StreamWidth {
    fn add(self, rhs: StreamWidth) -> StreamWidth {
        match (self, rhs) {
            (StreamWidth::Error, _) | (_, StreamWidth::Error) => StreamWidth::Error,
            (StreamWidth::Dynamic, _) | (_, StreamWidth::Dynamic) => StreamWidth::Dynamic,
            (StreamWidth::Known(a), StreamWidth::Known(b)) => match a.checked_add(b) {
                Some(total) => StreamWidth::Known(total),
                None => StreamWidth::Error,
            },
        }
    }

    fn mul(count: u32, elem_width: u32) -> StreamWidth {
        match count.checked_mul(elem_width) {
            Some(total) => StreamWidth::Known(total),
            None => StreamWidth::Error,
        }
    }
}

/// Normalized view of an array element type for streaming width computation.
struct StreamElemView {
    bit_width: Option<u32>,
    four_state: bool,
}

fn stream_elem_view(elem_ty: &Ty, ctx: &dyn InferCtx) -> Option<StreamElemView> {
    match elem_ty {
        Ty::Integral(i) => Some(StreamElemView {
            bit_width: i.try_packed_width(),
            four_state: i.keyword.four_state(),
        }),
        Ty::Enum(id) => {
            let bv = ctx.enum_integral_view(id)?;
            Some(StreamElemView {
                bit_width: bv.width.self_determined(),
                four_state: bv.four_state,
            })
        }
        _ => None,
    }
}

fn infer_stream(node: &SyntaxNode, ctx: &dyn InferCtx) -> ExprType {
    let Some(stream) = StreamExpr::cast(node.clone()) else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let Some(operands) = stream.stream_operands() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };

    let mut width = StreamWidth::Known(0);
    let mut any_four_state = false;

    for item in operands.items() {
        let Some(expr_node) = item.expr() else {
            width = width.add(StreamWidth::Error);
            continue;
        };

        let child_ty = infer_expr_type(expr_node.syntax(), ctx, None);
        if let ExprView::Error(_) = &child_ty.view {
            width = width.add(StreamWidth::Error);
            continue;
        }

        if let Some(with_clause) = item.with_clause() {
            // Operand has `with [range]` clause
            let operand_width =
                infer_stream_with_operand(&child_ty, &with_clause, &mut any_four_state, ctx);
            width = width.add(operand_width);
        } else {
            // No with-clause: existing path -- require integral
            let Some(bv) = try_integral_view(&child_ty, ctx) else {
                width = width.add(StreamWidth::Error);
                continue;
            };
            any_four_state = any_four_state || bv.four_state;
            match bv.width.self_determined() {
                Some(w) => width = width.add(StreamWidth::Known(w)),
                None => width = width.add(StreamWidth::Dynamic),
            }
        }
    }

    let result_width = match width {
        StreamWidth::Known(n) => BitWidth::Known(n),
        StreamWidth::Dynamic => BitWidth::Unknown,
        StreamWidth::Error => {
            return ExprType::error(ExprTypeErrorKind::InternalStreamOperandError);
        }
    };

    ExprType::bitvec(BitVecType {
        width: result_width,
        signed: Signedness::Unsigned,
        four_state: any_four_state,
    })
}

fn infer_stream_with_operand(
    operand_ty: &ExprType,
    with_clause: &lyra_ast::StreamWithClause,
    any_four_state: &mut bool,
    ctx: &dyn InferCtx,
) -> StreamWidth {
    use lyra_ast::StreamRangeOp;

    let Some(range) = with_clause.range() else {
        return StreamWidth::Error;
    };

    // Operand must be an array type
    let Ty::Array { elem, .. } = &operand_ty.ty else {
        return StreamWidth::Error;
    };

    let Some(ev) = stream_elem_view(elem, ctx) else {
        // Element type is not bit-streamable (e.g. real, packed struct)
        return StreamWidth::Error;
    };
    *any_four_state = *any_four_state || ev.four_state;

    let Some(op) = range.op() else {
        return StreamWidth::Error;
    };

    let elem_count: StreamWidth = match op {
        StreamRangeOp::Single => StreamWidth::Known(1),
        StreamRangeOp::IndexedPlus | StreamRangeOp::IndexedMinus => {
            let Some(width_node) = range.rhs() else {
                return StreamWidth::Error;
            };
            match ctx.const_eval(width_node.syntax()) {
                ConstInt::Known(w) if w > 0 => match u32::try_from(w) {
                    Ok(v) => StreamWidth::Known(v),
                    Err(_) => StreamWidth::Dynamic,
                },
                ConstInt::Known(_) | ConstInt::Unevaluated(_) | ConstInt::Error(_) => {
                    StreamWidth::Dynamic
                }
            }
        }
        StreamRangeOp::Fixed => {
            let (Some(lo_node), Some(hi_node)) = (range.lhs(), range.rhs()) else {
                return StreamWidth::Error;
            };
            match (
                ctx.const_eval(lo_node.syntax()),
                ctx.const_eval(hi_node.syntax()),
            ) {
                (ConstInt::Known(lo), ConstInt::Known(hi)) => {
                    let diff = (i128::from(hi) - i128::from(lo)).unsigned_abs() + 1;
                    match u32::try_from(diff) {
                        Ok(v) => StreamWidth::Known(v),
                        Err(_) => StreamWidth::Error,
                    }
                }
                _ => StreamWidth::Dynamic,
            }
        }
    };

    match (elem_count, ev.bit_width) {
        (StreamWidth::Known(count), Some(bw)) => StreamWidth::mul(count, bw),
        (StreamWidth::Error, _) => StreamWidth::Error,
        _ => StreamWidth::Dynamic,
    }
}

fn infer_replic(node: &SyntaxNode, ctx: &dyn InferCtx) -> ExprType {
    let Some(replic) = ReplicExpr::cast(node.clone()) else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let Some(count_expr) = replic.count() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let count = ctx.const_eval(count_expr.syntax());

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

    // Body expressions after count (typically a single ConcatExpr)
    let body: Vec<lyra_ast::Expr> = replic.body_exprs().collect();

    let (inner_width, inner_four_state) =
        if body.len() == 1 && body[0].kind() == SyntaxKind::ConcatExpr {
            let inner = infer_concat(body[0].syntax(), ctx);
            if let ExprView::Error(_) = &inner.view {
                return inner;
            }
            let Some(bv) = try_integral_view(&inner, ctx) else {
                return ExprType::error(ExprTypeErrorKind::ConcatNonBitOperand);
            };
            (bv.width, bv.four_state)
        } else if body.is_empty() {
            return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
        } else {
            // Sum widths of inner items
            let mut total: Option<u32> = Some(0);
            let mut all_known = true;
            let mut any_four_state = false;
            for item in &body {
                let item_ty = infer_expr_type(item.syntax(), ctx, None);
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
    let Some(idx_expr) = lyra_ast::IndexExpr::cast(node.clone()) else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let Some(base_node) = idx_expr.base_expr() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let Some(index_node) = idx_expr.index_expr() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };

    let base = infer_expr_type(base_node.syntax(), ctx, None);
    if matches!(base.view, ExprView::Error(_)) {
        return base;
    }

    let idx = infer_expr_type(index_node.syntax(), ctx, None);
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

fn infer_field_access(node: &SyntaxNode, ctx: &dyn InferCtx) -> ExprType {
    use lyra_ast::FieldExpr;
    let Some(field_expr) = FieldExpr::cast(node.clone()) else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let Some(lhs) = field_expr.base_expr() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let Some(field_tok) = field_expr.field_name() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };

    let lhs_type = infer_expr_type(lhs.syntax(), ctx, None);
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
        Err(MemberLookupError::MethodNotValidOnReceiver(reason)) => {
            ExprType::error(ExprTypeErrorKind::MethodNotValidOnReceiver(reason))
        }
    }
}

fn infer_call(node: &SyntaxNode, ctx: &dyn InferCtx) -> ExprType {
    // System task/function calls: $clog2, $signed, $bits, etc.
    if SystemTfCall::cast(node.clone())
        .and_then(|s| s.system_name())
        .is_some()
    {
        return crate::system_functions::infer_system_call(node, ctx);
    }

    // User-defined call: classify callee form
    let Some(call) = CallExpr::cast(node.clone()) else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let Some(callee_expr) = call.callee() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let callee_node = callee_expr.syntax().clone();

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

    check_call_args(node, &sig, ctx);
    ExprType::from_ty(&sig.return_ty)
}

/// Check call arguments against the callable signature.
fn check_call_args(call_node: &SyntaxNode, sig: &CallableSigRef, ctx: &dyn InferCtx) {
    let Some(call) = CallExpr::cast(call_node.clone()) else {
        return;
    };
    let args: Vec<lyra_ast::Expr> = call
        .arg_list()
        .map(|al| al.args().collect())
        .unwrap_or_default();

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
        infer_expr_type(arg.syntax(), ctx, expected_ctx.as_ref());
    }
}

fn infer_method_call(
    call_node: &SyntaxNode,
    callee_node: &SyntaxNode,
    ctx: &dyn InferCtx,
) -> ExprType {
    use lyra_ast::FieldExpr;
    let Some(field_expr) = FieldExpr::cast(callee_node.clone()) else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let Some(field_tok) = field_expr.field_name() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let Some(lhs_node) = field_expr.base_expr() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };

    let lhs_type = infer_expr_type(lhs_node.syntax(), ctx, None);
    if let ExprView::Error(_) = &lhs_type.view {
        return lhs_type;
    }

    match ctx.member_lookup(&lhs_type.ty, field_tok.text()) {
        Ok(MemberInfo {
            kind: MemberKind::BuiltinMethod(bm),
            ty,
            receiver,
        }) => crate::builtin_methods::infer_builtin_method_call(
            call_node,
            bm,
            &ty,
            receiver.as_ref(),
            ctx,
        ),
        Ok(_) | Err(MemberLookupError::NoMembersOnType) => ExprType::error(
            ExprTypeErrorKind::UnsupportedCalleeForm(CalleeFormKind::MethodCall),
        ),
        Err(MemberLookupError::UnknownMember) => ExprType::error(ExprTypeErrorKind::UnknownMember),
        Err(MemberLookupError::NotInModport) => {
            ExprType::error(ExprTypeErrorKind::MemberNotInModport)
        }
        Err(MemberLookupError::MethodNotValidOnReceiver(reason)) => {
            ExprType::error(ExprTypeErrorKind::MethodNotValidOnReceiver(reason))
        }
    }
}

fn infer_cast(node: &SyntaxNode, ctx: &dyn InferCtx) -> ExprType {
    use lyra_ast::CastExpr;
    let Some(cast) = CastExpr::cast(node.clone()) else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };

    // Infer the inner expression (for side effects / context propagation)
    if let Some(inner) = cast.inner_expr() {
        let _ = infer_expr_type(inner.syntax(), ctx, None);
    }

    // Resolve the target type from the TypeSpec child
    let Some(typespec) = cast.cast_type() else {
        return ExprType::error(ExprTypeErrorKind::CastTargetNotAType);
    };
    let ts_node = typespec.syntax();

    // Try user-defined type (enum/typedef name) via resolve_type_arg
    if let Some(utr) = crate::user_type_ref(ts_node)
        && let Some(ty) = ctx.resolve_type_arg(utr.resolve_node())
    {
        return ExprType::from_ty(&ty);
    }

    // Keyword type: extract from TypeSpec directly
    let map = lyra_ast::AstIdMap::from_root(ctx.file_id(), ts_node);
    let ty = crate::extract_base_ty_from_typespec(ts_node, &map);
    if matches!(ty, Ty::Error) {
        return ExprType::error(ExprTypeErrorKind::CastTargetNotAType);
    }
    ExprType::from_ty(&ty)
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
