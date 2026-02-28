use lyra_ast::RangeKind;

use super::expr_type::{ExprType, ExprTypeErrorKind, ExprView, InferCtx};
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

/// Infer the type of a range expression (`a[hi:lo]`, `a[i+:w]`, `a[i-:w]`).
///
/// Supports packed integral part-select (LRM 11.5.1) and unpacked fixed-size
/// array slicing (LRM 7.4.6).
pub(super) fn infer_range(range: &lyra_ast::RangeExpr, ctx: &dyn InferCtx) -> ExprType {
    let kind = range.range_kind();

    // 1. Infer base expression and reject non-sliceable types early
    let Some(base_node) = range.base_expr() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let base = infer_expr(&base_node, ctx, None);
    if matches!(base.view, ExprView::Error(_)) {
        return base;
    }

    match &base.ty {
        Ty::Integral(_) | Ty::Array { .. } => {}
        _ => return ExprType::error(ExprTypeErrorKind::PartSelectNonIntegral),
    }

    // For arrays, check sliceability before operand work
    if let Ty::Array { dim, .. } = &base.ty
        && dim.fixed_len().is_none()
    {
        return ExprType::error(ExprTypeErrorKind::SliceNonSliceableArray);
    }

    // 2. Type-check operands for error propagation
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

    // 3. Const-eval and compute width
    let width =
        match compute_slice_width(kind, ctx.const_eval(&op1_node), ctx.const_eval(&op2_node)) {
            Ok(w) => w,
            Err(e) => return ExprType::error(e),
        };

    // 4. Construct result type
    match &base.ty {
        Ty::Integral(i) => ExprType::from_ty(&i.part_select_result(width)),

        Ty::Array { elem, .. } => {
            debug_assert!(width > 0, "array slice with width 0");
            ExprType::from_ty(&Ty::Array {
                elem: elem.clone(),
                dim: UnpackedDim::Size(ConstInt::Known(i64::from(width))),
            })
        }

        _ => ExprType::error(ExprTypeErrorKind::PartSelectNonIntegral),
    }
}
