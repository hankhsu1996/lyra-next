use lyra_ast::{AstNode, BinExpr, CastExpr, CondExpr, LiteralKind, PrefixExpr};
use lyra_lexer::SyntaxKind;
use lyra_parser::SyntaxNode;

use super::expr_type::{
    BitVecType, BitWidth, ExprType, ExprTypeErrorKind, ExprView, InferCtx, Signedness,
    try_integral_view,
};
use super::infer_expr_type;
use crate::coerce::{
    IntegralCtx, OpCategory, apply_outer_context, coerce_integral, comparison_context, op_spec,
    operator_result_self,
};
use crate::literal::parse_literal_shape;
use crate::types::{RealKw, Ty};

pub(super) fn infer_literal(node: &SyntaxNode, expected: Option<&IntegralCtx>) -> ExprType {
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

pub(super) fn infer_prefix(
    node: &SyntaxNode,
    ctx: &dyn InferCtx,
    expected: Option<&IntegralCtx>,
) -> ExprType {
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

pub(super) fn infer_binary(
    node: &SyntaxNode,
    ctx: &dyn InferCtx,
    expected: Option<&IntegralCtx>,
) -> ExprType {
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

pub(super) fn infer_cond(
    node: &SyntaxNode,
    ctx: &dyn InferCtx,
    expected: Option<&IntegralCtx>,
) -> ExprType {
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

pub(super) fn infer_cast(node: &SyntaxNode, ctx: &dyn InferCtx) -> ExprType {
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
