use lyra_lexer::SyntaxKind;
use lyra_parser::SyntaxNode;
use smol_str::SmolStr;

use crate::coerce::{
    IntegralCtx, OpCategory, apply_outer_context, coerce_integral, comparison_context, op_spec,
    operator_result_self,
};
use crate::expr_helpers::{find_binary_op, find_operator_token, is_expression_kind};
use crate::literal::parse_literal_shape;
use crate::symbols::{GlobalSymbolId, SymbolKind};
use crate::syntax_helpers::system_tf_name;
use crate::types::{ConstEvalError, ConstInt, RealKw, SymbolType, Ty};

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
    FieldAccessUnsupported,
    UserCallUnsupported,
    RangeUnsupported,
    UnsupportedCalleeForm(CalleeFormKind),
    UnresolvedCall,
    NotACallable(SymbolKind),
    TaskInExprContext,
}

/// Result of typing an expression (self-determined).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprType {
    /// Integral bit-vector (logic/int/bit/etc). All expression operations
    /// normalize the keyword to logic -- we don't track keyword in `BitVecType`.
    BitVec(BitVecType),
    /// Non-bit type. Wraps `Ty` directly (Real, String, Chandle, Event, Void)
    /// to avoid duplicating the `Ty` variant set.
    NonBit(Ty),
    Error(ExprTypeErrorKind),
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
                ExprType::BitVec(BitVecType {
                    width,
                    signed,
                    four_state,
                })
            }
            Ty::Real(_) | Ty::String | Ty::Chandle | Ty::Event | Ty::Void => {
                ExprType::NonBit(ty.clone())
            }
            Ty::Enum(_) | Ty::Struct(_) | Ty::Array { .. } => ExprType::NonBit(ty.clone()),
            Ty::Error => ExprType::Error(ExprTypeErrorKind::Unresolved),
        }
    }

    /// Map `SymbolType` to `ExprType`.
    pub fn from_symbol_type(st: &SymbolType) -> ExprType {
        match st {
            SymbolType::Value(ty) => ExprType::from_ty(ty),
            SymbolType::Net(net) => ExprType::from_ty(&net.data),
            SymbolType::TypeAlias(_) => ExprType::Error(ExprTypeErrorKind::NameRefIsTypeNotValue),
            SymbolType::Error(_) => ExprType::Error(ExprTypeErrorKind::Unresolved),
        }
    }

    /// Unsized decimal literal default: `BitVec(32, Signed, 2-state)`.
    pub fn int_literal() -> ExprType {
        ExprType::BitVec(BitVecType {
            width: BitWidth::Known(32),
            signed: Signedness::Signed,
            four_state: false,
        })
    }

    /// Sized bit-vector (2-state by default).
    pub fn bits(width: u32, signed: Signedness) -> ExprType {
        ExprType::BitVec(BitVecType {
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
        match self {
            ExprType::BitVec(bv) => bv.pretty(),
            ExprType::NonBit(ty) => ty.pretty(),
            ExprType::Error(_) => SmolStr::new_static("<error>"),
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
    /// Resolve a `NameRef`/`QualifiedName` to its `SymbolType` (for unpacked dim peeling).
    fn symbol_type_of_name(&self, name_node: &SyntaxNode) -> Option<SymbolType>;
    /// Evaluate a constant expression (for replication count).
    fn const_eval(&self, expr_node: &SyntaxNode) -> ConstInt;
    /// Resolve a callee name node to a callable symbol.
    fn resolve_callable(
        &self,
        callee_node: &SyntaxNode,
    ) -> Result<GlobalSymbolId, ResolveCallableError>;
    /// Get the signature of a callable symbol.
    fn callable_sig(&self, sym: GlobalSymbolId) -> Option<CallableSigRef>;
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
            None => ExprType::Error(ExprTypeErrorKind::UnsupportedExprKind),
        },
        SyntaxKind::PrefixExpr => infer_prefix(expr, ctx, expected),
        SyntaxKind::BinExpr => infer_binary(expr, ctx, expected),
        SyntaxKind::CondExpr => infer_cond(expr, ctx, expected),
        SyntaxKind::ConcatExpr => infer_concat(expr, ctx),
        SyntaxKind::ReplicExpr => infer_replic(expr, ctx),
        SyntaxKind::IndexExpr => infer_index(expr, ctx),
        SyntaxKind::RangeExpr => ExprType::Error(ExprTypeErrorKind::RangeUnsupported),
        SyntaxKind::FieldExpr => ExprType::Error(ExprTypeErrorKind::FieldAccessUnsupported),
        SyntaxKind::CallExpr | SyntaxKind::SystemTfCall => infer_call(expr, ctx),
        _ => ExprType::Error(ExprTypeErrorKind::UnsupportedExprKind),
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
        Some(SyntaxKind::TimeLiteral) => ExprType::NonBit(Ty::Real(RealKw::Time)),
        Some(SyntaxKind::RealLiteral) => ExprType::NonBit(Ty::Real(RealKw::Real)),
        Some(SyntaxKind::StringLiteral) => ExprType::NonBit(Ty::String),
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
                ExprType::BitVec(coerce_integral(&self_ty, ectx))
            } else {
                ExprType::BitVec(self_ty)
            }
        }
        _ => match parse_literal_shape(node) {
            Some(shape) => {
                let signed = if shape.signed {
                    Signedness::Signed
                } else {
                    Signedness::Unsigned
                };
                ExprType::BitVec(BitVecType {
                    width: BitWidth::Known(shape.width),
                    signed,
                    four_state: shape.has_xz,
                })
            }
            None => ExprType::Error(ExprTypeErrorKind::UnsupportedLiteralKind),
        },
    }
}

fn infer_prefix(node: &SyntaxNode, ctx: &dyn InferCtx, expected: Option<&IntegralCtx>) -> ExprType {
    let Some(op) = find_operator_token(node) else {
        return ExprType::Error(ExprTypeErrorKind::UnsupportedExprKind);
    };

    let Some(operand_node) = node.children().find(|c| is_expression_kind(c.kind())) else {
        return ExprType::Error(ExprTypeErrorKind::UnsupportedExprKind);
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
            match &operand {
                ExprType::BitVec(_) | ExprType::Error(_) => operand,
                ExprType::NonBit(_) => ExprType::Error(ExprTypeErrorKind::NonBitOperand),
            }
        }
        _ => ExprType::Error(ExprTypeErrorKind::UnsupportedExprKind),
    }
}

fn infer_binary(node: &SyntaxNode, ctx: &dyn InferCtx, expected: Option<&IntegralCtx>) -> ExprType {
    let children: Vec<SyntaxNode> = node
        .children()
        .filter(|c| is_expression_kind(c.kind()))
        .collect();
    if children.len() < 2 {
        return ExprType::Error(ExprTypeErrorKind::UnsupportedExprKind);
    }

    let Some(op) = find_binary_op(node) else {
        return ExprType::Error(ExprTypeErrorKind::UnsupportedExprKind);
    };

    // Step 1-2: self-determined types of both operands
    let lhs_self = infer_expr_type(&children[0], ctx, None);
    let rhs_self = infer_expr_type(&children[1], ctx, None);

    let spec = op_spec(op);

    // Check for non-bit operands
    let (lhs_bv, rhs_bv) = match (&lhs_self, &rhs_self) {
        (ExprType::BitVec(a), ExprType::BitVec(b)) => (a, b),
        (ExprType::Error(_), _) => return lhs_self,
        (_, ExprType::Error(_)) => return rhs_self,
        _ => return ExprType::Error(ExprTypeErrorKind::NonBitOperand),
    };

    // Step 3: operator result (self-determined)
    let result_self = operator_result_self(spec.category, spec.result_state, lhs_bv, rhs_bv);

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
            let cmp_ctx = comparison_context(lhs_bv, rhs_bv);
            infer_expr_type(&children[0], ctx, Some(&cmp_ctx));
            infer_expr_type(&children[1], ctx, Some(&cmp_ctx));
        }
        OpCategory::Logical => {
            // Both operands are self-determined
        }
    }

    // Step 6: return the coerced result
    ExprType::BitVec(coerce_integral(&result_self, &result_ctx))
}

fn infer_cond(node: &SyntaxNode, ctx: &dyn InferCtx, expected: Option<&IntegralCtx>) -> ExprType {
    let children: Vec<SyntaxNode> = node
        .children()
        .filter(|c| is_expression_kind(c.kind()))
        .collect();
    if children.len() < 3 {
        return ExprType::Error(ExprTypeErrorKind::UnsupportedExprKind);
    }

    // Condition is always self-determined
    let _cond = infer_expr_type(&children[0], ctx, None);

    // Self-determined types of both arms
    let then_ty = infer_expr_type(&children[1], ctx, None);
    let else_ty = infer_expr_type(&children[2], ctx, None);

    match (&then_ty, &else_ty) {
        (ExprType::BitVec(a), ExprType::BitVec(b)) => {
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

            ExprType::BitVec(coerce_integral(&merged, &arm_ctx))
        }
        (ExprType::NonBit(a), ExprType::NonBit(b)) if a == b => then_ty,
        (ExprType::Error(_), _) => then_ty,
        (_, ExprType::Error(_)) => else_ty,
        _ => ExprType::Error(ExprTypeErrorKind::CondBranchTypeMismatch),
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
        match &child_ty {
            ExprType::BitVec(bv) => {
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
            ExprType::NonBit(_) => {
                return ExprType::Error(ExprTypeErrorKind::ConcatNonBitOperand);
            }
            ExprType::Error(_) => return child_ty,
        }
    }

    let width = if all_known {
        BitWidth::Known(total_width.unwrap_or(0))
    } else {
        BitWidth::Unknown
    };

    ExprType::BitVec(BitVecType {
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
        return ExprType::Error(ExprTypeErrorKind::UnsupportedExprKind);
    }

    // First expression child is the replication count
    let count_node = &expr_children[0];
    let count = ctx.const_eval(count_node);

    let replic_count = match count {
        ConstInt::Known(n) => {
            if n <= 0 {
                return ExprType::Error(ExprTypeErrorKind::InvalidReplicationCount);
            }
            u32::try_from(n).ok()
        }
        ConstInt::Error(e) => {
            return ExprType::Error(ExprTypeErrorKind::ReplicationConstEvalFailed(e));
        }
        ConstInt::Unevaluated(_) => None,
    };

    // Remaining children are the inner items (to be concatenated)
    let inner_items = &expr_children[1..];

    // Check for ConcatExpr child
    let concat_child = children.iter().find(|c| c.kind() == SyntaxKind::ConcatExpr);

    let inner_width = if let Some(concat) = concat_child {
        let inner = infer_concat(concat, ctx);
        match &inner {
            ExprType::BitVec(bv) => bv.width,
            ExprType::Error(_) => return inner,
            ExprType::NonBit(_) => {
                return ExprType::Error(ExprTypeErrorKind::ConcatNonBitOperand);
            }
        }
    } else if inner_items.is_empty() {
        return ExprType::Error(ExprTypeErrorKind::UnsupportedExprKind);
    } else {
        // Sum widths of inner items
        let mut total: Option<u32> = Some(0);
        let mut all_known = true;
        for item in inner_items {
            let item_ty = infer_expr_type(item, ctx, None);
            match &item_ty {
                ExprType::BitVec(bv) => match bv.width.self_determined() {
                    Some(w) => {
                        if let Some(ref mut t) = total {
                            *t = t.saturating_add(w);
                        }
                    }
                    None => all_known = false,
                },
                ExprType::NonBit(_) => {
                    return ExprType::Error(ExprTypeErrorKind::ConcatNonBitOperand);
                }
                ExprType::Error(_) => return item_ty,
            }
        }
        if all_known {
            BitWidth::Known(total.unwrap_or(0))
        } else {
            BitWidth::Unknown
        }
    };

    let result_width = match (replic_count, inner_width.self_determined()) {
        (Some(n), Some(w)) => BitWidth::Known(n.saturating_mul(w)),
        _ => BitWidth::Unknown,
    };

    ExprType::BitVec(BitVecType {
        width: result_width,
        signed: Signedness::Unsigned,
        four_state: false,
    })
}

fn infer_index(node: &SyntaxNode, ctx: &dyn InferCtx) -> ExprType {
    let children: Vec<SyntaxNode> = node
        .children()
        .filter(|c| is_expression_kind(c.kind()))
        .collect();
    if children.is_empty() {
        return ExprType::Error(ExprTypeErrorKind::UnsupportedExprKind);
    }

    let base_node = &children[0];

    // Try to get the full symbol type (with unpacked dims) for the base
    let base_sym_ty = if matches!(
        base_node.kind(),
        SyntaxKind::NameRef | SyntaxKind::QualifiedName
    ) {
        ctx.symbol_type_of_name(base_node)
    } else {
        None
    };

    // Check for unpacked dim peeling first
    if let Some(ref st) = base_sym_ty {
        let ty = match st {
            SymbolType::Value(ty) | SymbolType::TypeAlias(ty) => Some(ty),
            SymbolType::Net(net) => Some(&net.data),
            SymbolType::Error(_) => None,
        };
        if let Some(peeled) = ty.and_then(|t| t.peel_unpacked_dim()) {
            return ExprType::from_ty(&peeled);
        }
    }

    // No unpacked dims to peel -- check if base is bitvec for packed bit-select
    let base_ty = infer_expr_type(base_node, ctx, None);
    match &base_ty {
        ExprType::BitVec(_) => ExprType::one_bit(),
        ExprType::Error(_) => base_ty,
        ExprType::NonBit(_) => ExprType::Error(ExprTypeErrorKind::IndexNonIndexable),
    }
}

fn infer_call(node: &SyntaxNode, ctx: &dyn InferCtx) -> ExprType {
    // System task/function calls: $clog2, etc.
    if let Some(tok) = system_tf_name(node) {
        return match tok.text() {
            "$clog2" => ExprType::BitVec(BitVecType {
                width: BitWidth::Known(32),
                signed: Signedness::Signed,
                four_state: false,
            }),
            _ => ExprType::Error(ExprTypeErrorKind::UnsupportedSystemCall),
        };
    }

    // User-defined call: classify callee form
    let callee_node = match node.first_child() {
        Some(c) if is_expression_kind(c.kind()) => c,
        _ => return ExprType::Error(ExprTypeErrorKind::UnsupportedExprKind),
    };

    match callee_node.kind() {
        SyntaxKind::NameRef | SyntaxKind::QualifiedName => {}
        SyntaxKind::FieldExpr => {
            return ExprType::Error(ExprTypeErrorKind::UnsupportedCalleeForm(
                CalleeFormKind::MethodCall,
            ));
        }
        _ => {
            return ExprType::Error(ExprTypeErrorKind::UnsupportedCalleeForm(
                CalleeFormKind::Other,
            ));
        }
    }

    // Resolve callee to a callable symbol
    let sym_id = match ctx.resolve_callable(&callee_node) {
        Ok(id) => id,
        Err(ResolveCallableError::NotFound) => {
            return ExprType::Error(ExprTypeErrorKind::UnresolvedCall);
        }
        Err(ResolveCallableError::NotACallable(kind)) => {
            return ExprType::Error(ExprTypeErrorKind::NotACallable(kind));
        }
    };

    // Get callable signature
    let Some(sig) = ctx.callable_sig(sym_id) else {
        return ExprType::Error(ExprTypeErrorKind::UnresolvedCall);
    };

    // Task used in expression context
    if sig.kind == CallableKind::Task {
        return ExprType::Error(ExprTypeErrorKind::TaskInExprContext);
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
            match ety {
                ExprType::BitVec(bv) => Some(IntegralCtx {
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
