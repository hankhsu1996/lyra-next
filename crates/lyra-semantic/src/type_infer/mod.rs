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

use lyra_ast::Expr;
use lyra_lexer::SyntaxKind;
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
    match peeled.kind() {
        SyntaxKind::NameRef | SyntaxKind::QualifiedName => ctx.type_of_name(peeled.syntax()),
        SyntaxKind::Literal => scalar::infer_literal(peeled.syntax(), expected),
        SyntaxKind::PrefixExpr => scalar::infer_prefix(peeled.syntax(), ctx, expected),
        SyntaxKind::BinExpr => scalar::infer_binary(peeled.syntax(), ctx, expected),
        SyntaxKind::CondExpr => scalar::infer_cond(peeled.syntax(), ctx, expected),
        SyntaxKind::ConcatExpr => aggregate::infer_concat(peeled.syntax(), ctx),
        SyntaxKind::ReplicExpr => aggregate::infer_replic(peeled.syntax(), ctx),
        SyntaxKind::IndexExpr => access::infer_index(peeled.syntax(), ctx),
        SyntaxKind::RangeExpr => range::infer_range(peeled.syntax(), ctx),
        SyntaxKind::FieldExpr => access::infer_field_access(peeled.syntax(), ctx),
        SyntaxKind::CallExpr | SyntaxKind::SystemTfCall => call::infer_call(peeled.syntax(), ctx),
        SyntaxKind::StreamExpr => aggregate::infer_stream(peeled.syntax(), ctx),
        SyntaxKind::CastExpr => scalar::infer_cast(peeled.syntax(), ctx),
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
