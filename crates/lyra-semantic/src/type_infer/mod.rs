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

use lyra_ast::{AstNode, Expr};
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
    match expr.kind() {
        SyntaxKind::NameRef | SyntaxKind::QualifiedName => ctx.type_of_name(expr),
        SyntaxKind::Literal => scalar::infer_literal(expr, expected),
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
        SyntaxKind::PrefixExpr => scalar::infer_prefix(expr, ctx, expected),
        SyntaxKind::BinExpr => scalar::infer_binary(expr, ctx, expected),
        SyntaxKind::CondExpr => scalar::infer_cond(expr, ctx, expected),
        SyntaxKind::ConcatExpr => aggregate::infer_concat(expr, ctx),
        SyntaxKind::ReplicExpr => aggregate::infer_replic(expr, ctx),
        SyntaxKind::IndexExpr => access::infer_index(expr, ctx),
        SyntaxKind::RangeExpr => range::infer_range(expr, ctx),
        SyntaxKind::FieldExpr => access::infer_field_access(expr, ctx),
        SyntaxKind::CallExpr | SyntaxKind::SystemTfCall => call::infer_call(expr, ctx),
        SyntaxKind::StreamExpr => aggregate::infer_stream(expr, ctx),
        SyntaxKind::CastExpr => scalar::infer_cast(expr, ctx),
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
