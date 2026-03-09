use lyra_ast::AssignmentPatternExpr;

use super::{ExprType, ExprTypeErrorKind, ExprView, InferCtx};
use crate::types::Ty;

/// Infer the type of an assignment pattern with an expected target type.
///
/// If the expected type supports assignment-pattern semantics, adopts it.
/// Otherwise returns a typed error.
pub(crate) fn infer_assignment_pattern_with_expected(
    _ap: &AssignmentPatternExpr,
    expected: Option<&Ty>,
    _ctx: &dyn InferCtx,
) -> ExprType {
    let Some(expected) = expected else {
        return ExprType::error(ExprTypeErrorKind::AssignmentPatternNeedsContext);
    };

    if matches!(expected, Ty::Error) {
        return ExprType::error(ExprTypeErrorKind::AssignmentPatternNeedsContext);
    }

    ExprType {
        ty: expected.clone(),
        view: ExprView::AssignmentPattern,
    }
}
