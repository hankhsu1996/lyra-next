mod access;
mod aggregate;
mod call;
mod expr_type;
mod range;
mod scalar;

pub(crate) use expr_type::try_integral_view;
pub use expr_type::{
    BitVecType, BitWidth, CallableKind, CallablePort, CallableSigRef, CalleeFormKind, ExprType,
    ExprTypeErrorKind, ExprView, InferCtx, ResolveCallableError, Signedness,
};

use lyra_ast::{AstNode, Expr, ExprKind};
use lyra_parser::SyntaxNode;

use crate::coerce::IntegralCtx;
use crate::types::Ty;

/// Infer the type of an expression node, optionally in a context.
///
/// When `expected` is `None`, returns the self-determined type.
/// When `expected` is `Some(ctx)`, propagates context sizing per LRM 11.6.
pub fn infer_expr_type(
    expr: &SyntaxNode,
    ctx: &dyn InferCtx,
    expected: Option<&IntegralCtx>,
) -> ExprType {
    let Some(peeled) = Expr::peel(expr) else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let Some(ek) = peeled.classify() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    match ek {
        ExprKind::NameRef(n) => ctx.type_of_name(n.syntax()),
        ExprKind::QualifiedName(n) => ctx.type_of_name(n.syntax()),
        ExprKind::Literal(l) => scalar::infer_literal(&l, expected),
        ExprKind::PrefixExpr(p) => scalar::infer_prefix(&p, ctx, expected),
        ExprKind::BinExpr(b) => scalar::infer_binary(&b, ctx, expected),
        ExprKind::CondExpr(c) => scalar::infer_cond(&c, ctx, expected),
        ExprKind::ConcatExpr(c) => aggregate::infer_concat(&c, ctx),
        ExprKind::ReplicExpr(r) => aggregate::infer_replic(&r, ctx),
        ExprKind::IndexExpr(i) => access::infer_index(&i, ctx),
        ExprKind::RangeExpr(r) => range::infer_range(&r, ctx),
        ExprKind::FieldExpr(f) => access::infer_field_access(&f, ctx),
        ExprKind::CallExpr(c) => call::infer_call(&c, ctx),
        ExprKind::SystemTfCall(s) => call::infer_system_call(&s, ctx),
        ExprKind::StreamExpr(s) => aggregate::infer_stream(&s, ctx),
        ExprKind::CastExpr(c) => scalar::infer_cast(&c, ctx),
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
