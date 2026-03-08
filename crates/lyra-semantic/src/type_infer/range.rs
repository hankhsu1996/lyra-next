use lyra_ast::RangeKind;

use super::expr_type::{ExprType, ExprTypeErrorKind, ExprView, InferCtx, try_integral_view};
use super::infer_expr;
use crate::types::{ConstInt, Ty, UnpackedDim};

/// Compute the constant width of a range select from const-eval results.
///
/// Fixed `[hi:lo]`: width = |hi - lo| + 1. Both must be known.
/// Indexed `[+:k]` / `[-:k]`: width = k. Must be known and > 0.
fn compute_slice_width(
    kind: RangeKind,
    op1_const: ConstInt,
    op2_const: ConstInt,
) -> Result<u32, ExprTypeErrorKind> {
    match kind {
        RangeKind::IndexedPlus | RangeKind::IndexedMinus => match op2_const {
            ConstInt::Known(w) if w > 0 => {
                u32::try_from(w).map_err(|_| ExprTypeErrorKind::SliceWidthInvalid)
            }
            _ => Err(ExprTypeErrorKind::SliceWidthInvalid),
        },
        RangeKind::Fixed => match (op1_const, op2_const) {
            (ConstInt::Known(h), ConstInt::Known(l)) => {
                let diff = (i128::from(h) - i128::from(l)).unsigned_abs() + 1;
                u32::try_from(diff).map_err(|_| ExprTypeErrorKind::SliceWidthInvalid)
            }
            _ => Err(ExprTypeErrorKind::SliceWidthInvalid),
        },
    }
}

/// Whether an expression view is valid as a queue index/slice bound.
///
/// Queue bounds must be integral or `$` (`QueueDollar`). `$` is accepted
/// only in queue index/slice context.
fn is_valid_queue_bound(et: &ExprType, ctx: &dyn InferCtx) -> bool {
    matches!(et.view, ExprView::QueueDollar) || try_integral_view(et, ctx).is_some()
}

/// Infer the type of a range expression (`a[hi:lo]`, `a[i+:w]`, `a[i-:w]`).
///
/// Dispatches by unpacked dimension kind:
/// - Queue: fixed slice returns queue of same element type; indexed
///   part-select is rejected.
/// - Fixed-size array: existing path via const-eval width.
/// - Integral: packed part-select.
pub(super) fn infer_range(range: &lyra_ast::RangeExpr, ctx: &dyn InferCtx) -> ExprType {
    let kind = range.range_kind();

    let Some(base_node) = range.base_expr() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let base = infer_expr(&base_node, ctx, None);
    if matches!(base.view, ExprView::Error(_)) {
        return base;
    }

    match &base.ty {
        Ty::Array {
            dim: UnpackedDim::Queue { .. },
            elem,
        } => infer_queue_slice(range, kind, elem, ctx),

        Ty::Array { dim, elem } if dim.fixed_len().is_some() => {
            infer_fixed_array_slice(range, kind, elem, dim, ctx)
        }

        Ty::Array { .. } => ExprType::error(ExprTypeErrorKind::SliceNonSliceableArray),

        Ty::Integral(_) => infer_integral_part_select(range, kind, &base, ctx),

        _ => ExprType::error(ExprTypeErrorKind::PartSelectNonIntegral),
    }
}

/// Queue slice: `q[a:b]` returns queue of same element type.
/// Indexed part-select (`+:` / `-:`) on queue is rejected.
fn infer_queue_slice(
    range: &lyra_ast::RangeExpr,
    kind: RangeKind,
    elem: &Ty,
    ctx: &dyn InferCtx,
) -> ExprType {
    if matches!(kind, RangeKind::IndexedPlus | RangeKind::IndexedMinus) {
        return ExprType::error(ExprTypeErrorKind::QueuePartSelectNotAllowed);
    }

    let Some((op1_node, op2_node)) = range.operand_exprs() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let op1_et = infer_expr(&op1_node, ctx, None);
    if matches!(op1_et.view, ExprView::Error(_)) {
        return op1_et;
    }
    let op2_et = infer_expr(&op2_node, ctx, None);
    if matches!(op2_et.view, ExprView::Error(_)) {
        return op2_et;
    }

    if !is_valid_queue_bound(&op1_et, ctx) || !is_valid_queue_bound(&op2_et, ctx) {
        return ExprType::error(ExprTypeErrorKind::PartSelectNonIntegral);
    }

    ExprType::from_ty(&Ty::Array {
        elem: Box::new(elem.clone()),
        dim: UnpackedDim::Queue { bound: None },
    })
}

/// Fixed-size array slice: compute width via const-eval.
fn infer_fixed_array_slice(
    range: &lyra_ast::RangeExpr,
    kind: RangeKind,
    elem: &Ty,
    _dim: &UnpackedDim,
    ctx: &dyn InferCtx,
) -> ExprType {
    let Some((op1_node, op2_node)) = range.operand_exprs() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let op1_et = infer_expr(&op1_node, ctx, None);
    if matches!(op1_et.view, ExprView::Error(_)) {
        return op1_et;
    }
    let op2_et = infer_expr(&op2_node, ctx, None);
    if matches!(op2_et.view, ExprView::Error(_)) {
        return op2_et;
    }

    // Reject `$` in non-queue slice context
    if matches!(op1_et.view, ExprView::QueueDollar) || matches!(op2_et.view, ExprView::QueueDollar)
    {
        return ExprType::error(ExprTypeErrorKind::DollarOutsideQueueContext);
    }

    let width =
        match compute_slice_width(kind, ctx.const_eval(&op1_node), ctx.const_eval(&op2_node)) {
            Ok(w) => w,
            Err(e) => return ExprType::error(e),
        };

    debug_assert!(width > 0, "array slice with width 0");
    ExprType::from_ty(&Ty::Array {
        elem: Box::new(elem.clone()),
        dim: UnpackedDim::Size(ConstInt::Known(i64::from(width))),
    })
}

/// Integral packed part-select.
fn infer_integral_part_select(
    range: &lyra_ast::RangeExpr,
    kind: RangeKind,
    base: &ExprType,
    ctx: &dyn InferCtx,
) -> ExprType {
    let Some((op1_node, op2_node)) = range.operand_exprs() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let op1_et = infer_expr(&op1_node, ctx, None);
    if matches!(op1_et.view, ExprView::Error(_)) {
        return op1_et;
    }
    let op2_et = infer_expr(&op2_node, ctx, None);
    if matches!(op2_et.view, ExprView::Error(_)) {
        return op2_et;
    }

    // Reject `$` in non-queue slice context
    if matches!(op1_et.view, ExprView::QueueDollar) || matches!(op2_et.view, ExprView::QueueDollar)
    {
        return ExprType::error(ExprTypeErrorKind::DollarOutsideQueueContext);
    }

    let width =
        match compute_slice_width(kind, ctx.const_eval(&op1_node), ctx.const_eval(&op2_node)) {
            Ok(w) => w,
            Err(e) => return ExprType::error(e),
        };

    match &base.ty {
        Ty::Integral(i) => ExprType::from_ty(&i.part_select_result(width)),
        _ => ExprType::error(ExprTypeErrorKind::PartSelectNonIntegral),
    }
}
