mod access;
mod aggregate;
mod assignment_pattern;
mod call;
mod expr_type;
mod range;
mod scalar;
pub(crate) mod scoped;

pub use call::{CallArgCheck, infer_call_arg_checks};
pub(crate) use expr_type::try_integral_view;
pub use expr_type::{
    BitVecType, BitWidth, CallableKind, CallablePort, CallableSigRef, CalleeFormKind, ExprType,
    ExprTypeErrorKind, ExprView, InferCtx, ResolveCallableError, Signedness,
};

use lyra_ast::{Expr, ExprKind, NewExpr, TaggedExpr};

use crate::coerce::IntegralCtx;
use crate::record::TaggedVariantError;
use crate::types::{Ty, UnpackedDim};

/// Infer the type of an expression, optionally in a context.
///
/// When `expected` is `None`, returns the self-determined type.
/// When `expected` is `Some(ctx)`, propagates context sizing per LRM 11.6.
pub fn infer_expr(expr: &Expr, ctx: &dyn InferCtx, expected: Option<&IntegralCtx>) -> ExprType {
    let Some(peeled) = expr.peeled() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let Some(ek) = peeled.classify() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let result = match ek {
        ExprKind::NameRef(_) | ExprKind::QualifiedName(_) => ctx.type_of_name(&peeled),
        ExprKind::Literal(l) => scalar::infer_literal(&l, expected),
        ExprKind::PrefixExpr(p) => scalar::infer_prefix(&p, ctx, expected),
        ExprKind::BinExpr(b) => scalar::infer_binary(&b, ctx, expected),
        ExprKind::CondExpr(c) => scalar::infer_cond(&c, ctx, expected),
        ExprKind::ConcatExpr(c) => aggregate::infer_concat(&c, ctx),
        ExprKind::AssignmentPatternExpr(_) => {
            ExprType::error(ExprTypeErrorKind::AssignmentPatternNeedsContext)
        }
        ExprKind::ReplicExpr(r) => aggregate::infer_replic(&r, ctx),
        ExprKind::IndexExpr(i) => access::infer_index(&i, ctx),
        ExprKind::RangeExpr(r) => range::infer_range(&r, ctx),
        ExprKind::FieldExpr(f) => access::infer_field_access(&f, ctx),
        ExprKind::CallExpr(c) => call::infer_call(&c, ctx),
        ExprKind::SystemTfCall(s) => call::infer_system_call(&s, ctx),
        ExprKind::StreamExpr(s) => aggregate::infer_stream(&s, ctx),
        ExprKind::CastExpr(c) => scalar::infer_cast(&c, ctx),
        ExprKind::NewExpr(ne) => infer_new_expr(&ne, None),
        ExprKind::TypeExpr(te) => scalar::infer_type_expr(&te, ctx),
        ExprKind::TaggedExpr(_) => ExprType::error(ExprTypeErrorKind::TaggedExprNeedsContext),
        ExprKind::DollarExpr(_) => ExprType {
            ty: Ty::int(),
            view: ExprView::QueueDollar,
        },
        ExprKind::MatchesExpr(me) => scalar::infer_matches(&me, ctx),
    };
    reject_void_value(result)
}

/// Reject `Ty::Void` in expression (value) context.
///
/// Void is legal only as a callable return type and in statement context.
/// Any expression position that requires a value rejects it uniformly.
fn reject_void_value(et: ExprType) -> ExprType {
    if matches!(et.ty, Ty::Void) {
        ExprType::error(ExprTypeErrorKind::VoidUsedAsExpr)
    } else {
        et
    }
}

/// Infer the type of an expression with an expected type for contextual typing.
///
/// For `NewExpr`, passes the expected type through so the constructor can
/// adopt the target array type. All other expression kinds ignore the expected
/// type and delegate to `infer_expr`.
pub fn infer_expr_with_expected(
    expr: &Expr,
    expected: Option<&Ty>,
    ctx: &dyn InferCtx,
    integral_ctx: Option<&IntegralCtx>,
) -> ExprType {
    let Some(peeled) = expr.peeled() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    match peeled.classify() {
        Some(ExprKind::NewExpr(ne)) => return infer_new_expr(&ne, expected),
        Some(ExprKind::TaggedExpr(te)) => return infer_tagged_expr(&te, expected, ctx),
        Some(ExprKind::AssignmentPatternExpr(ap)) => {
            return assignment_pattern::infer_assignment_pattern_with_expected(&ap, expected, ctx);
        }
        _ => {}
    }
    infer_expr(expr, ctx, integral_ctx)
}

fn infer_tagged_expr(te: &TaggedExpr, expected: Option<&Ty>, ctx: &dyn InferCtx) -> ExprType {
    let Some(Ty::Record(record_id)) = expected else {
        return if expected.is_some() {
            ExprType::error(ExprTypeErrorKind::TaggedExprExpectedTaggedUnion)
        } else {
            ExprType::error(ExprTypeErrorKind::TaggedExprNeedsContext)
        };
    };

    let Some(member_tok) = te.member_name() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };

    let variant = match ctx.tagged_union_variant(record_id, member_tok.text()) {
        Ok(v) => v,
        Err(TaggedVariantError::NotATaggedUnion) => {
            return ExprType::error(ExprTypeErrorKind::TaggedExprExpectedTaggedUnion);
        }
        Err(TaggedVariantError::UnknownMember) => {
            return ExprType::error(ExprTypeErrorKind::TaggedExprUnknownMember);
        }
    };

    let operand = te.operand();

    match (&variant.payload_ty, &operand) {
        (None, Some(_)) => {
            return ExprType::error(ExprTypeErrorKind::TaggedExprUnexpectedOperandForVoidMember);
        }
        (Some(_), None) => {
            return ExprType::error(ExprTypeErrorKind::TaggedExprMissingOperandForPayloadMember);
        }
        (Some(payload_ty), Some(op)) => {
            let op_result = infer_expr_with_expected(op, Some(payload_ty), ctx, None);
            if matches!(op_result.ty, Ty::Error) {
                return op_result;
            }
        }
        (None, None) => {}
    }

    ExprType::from_ty(&Ty::Record(*record_id))
}

fn infer_new_expr(ne: &NewExpr, expected: Option<&Ty>) -> ExprType {
    if !ne.has_brackets() {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    }
    match expected {
        Some(
            ty @ Ty::Array {
                dim: UnpackedDim::Unsized,
                ..
            },
        ) => ExprType::from_ty(ty),
        _ => ExprType::error(ExprTypeErrorKind::NewExprNeedsExpectedDynArray),
    }
}

/// Infer the type of an expression in statement context.
///
/// Void methods are legal in statement context; `VoidUsedAsExpr` is
/// replaced with a successful `Ty::Void` result.
pub fn infer_expr_stmt(expr: &Expr, ctx: &dyn InferCtx) -> ExprType {
    let result = infer_expr(expr, ctx, None);
    if matches!(
        result.view,
        ExprView::Error(ExprTypeErrorKind::VoidUsedAsExpr)
    ) {
        ExprType::from_ty(&Ty::Void)
    } else {
        result
    }
}
