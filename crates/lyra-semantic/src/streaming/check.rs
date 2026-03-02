use lyra_ast::{Expr, StreamRangeOp};
use lyra_source::TextRange;

use crate::Site;
use crate::streaming::shape::{StreamUnpackShape, WithInfo};
use crate::type_check::{TypeCheckCtx, TypeCheckItem};
use crate::types::Ty;

/// Diagnostic anchors for a streaming unpack assignment.
pub(crate) struct UnpackAssignSites {
    pub(crate) assign_site: Site,
    pub(crate) op_range: Option<TextRange>,
    pub(crate) lhs_site: Site,
    pub(crate) rhs_site: Site,
}

/// Check a streaming unpack shape against its RHS, emitting diagnostics.
///
/// One pass over operands in source order; width mismatch checked at end.
pub(crate) fn check_streaming_unpack(
    shape: &StreamUnpackShape,
    rhs: &Expr,
    sites: &UnpackAssignSites,
    ctx: &dyn TypeCheckCtx,
    items: &mut Vec<TypeCheckItem>,
) {
    let mut lhs_total_bits: Option<u32> = Some(0);

    for item in &shape.items {
        // Per-operand diagnostics: always run regardless of accumulation state.
        if !item.target.is_assignable() {
            items.push(TypeCheckItem::StreamUnpackOperandInvalid {
                operand_site: item.diag_site(),
            });
            lhs_total_bits = None;
            continue;
        }

        let Some(eid) = item.expr_id else {
            lhs_total_bits = None;
            continue;
        };

        if let Some(ref wi) = item.with_clause {
            let operand_et = ctx.expr_type_by_id(eid);
            let Ty::Array { ref elem, .. } = operand_et.ty else {
                items.push(TypeCheckItem::StreamWithNonArray {
                    with_site: wi.with_site,
                });
                lhs_total_bits = None;
                continue;
            };
            // Width contribution: only compute when total is still known.
            if let Some(total) = lhs_total_bits {
                let elem_bits = ctx.fixed_stream_width_bits_of_ty(elem);
                let elem_count = stream_with_selected_elems(wi, ctx);
                lhs_total_bits = match (elem_bits, elem_count) {
                    (Some(eb), Some(ec)) => eb.checked_mul(ec).and_then(|p| total.checked_add(p)),
                    _ => None,
                };
            }
        } else {
            let operand_bits = ctx.fixed_stream_width_bits(eid);
            if let Some(w) = operand_bits {
                lhs_total_bits = lhs_total_bits.and_then(|total| total.checked_add(w));
            } else {
                let operand_et = ctx.expr_type_by_id(eid);
                items.push(TypeCheckItem::StreamUnpackOperandUnsupported {
                    operand_site: item.diag_site(),
                    operand_ty: operand_et.ty.clone(),
                });
                lhs_total_bits = None;
            }
        }
    }

    // Width mismatch: only when both sides are statically known.
    let Some(lhs_w) = lhs_total_bits else {
        return;
    };
    let rhs_et = ctx.expr_type(rhs);
    let Some(rhs_w) = ctx.fixed_stream_width_bits_of_type(&rhs_et) else {
        return;
    };
    if lhs_w != rhs_w {
        items.push(TypeCheckItem::StreamUnpackWidthMismatch {
            assign_site: sites.assign_site,
            op_range: sites.op_range,
            lhs_site: sites.lhs_site,
            rhs_site: sites.rhs_site,
            lhs_width: lhs_w,
            rhs_width: rhs_w,
        });
    }
}

/// Compute the number of array elements selected by a `with [range]` clause.
///
/// Returns `Some(count)` when the range evaluates to a known positive element
/// count, `None` otherwise. No diagnostics emitted.
fn stream_with_selected_elems(wi: &WithInfo, ctx: &dyn TypeCheckCtx) -> Option<u32> {
    let op = wi.range_op?;
    match op {
        StreamRangeOp::Single => Some(1),
        StreamRangeOp::Fixed => {
            let lo = ctx.const_eval_int_by_site(wi.lhs_expr_site?)?;
            let hi = ctx.const_eval_int_by_site(wi.rhs_expr_site?)?;
            let diff = (i128::from(hi) - i128::from(lo)).unsigned_abs() + 1;
            u32::try_from(diff).ok()
        }
        StreamRangeOp::IndexedPlus | StreamRangeOp::IndexedMinus => {
            let width = ctx.const_eval_int_by_site(wi.rhs_expr_site?)?;
            if width <= 0 {
                return None;
            }
            u32::try_from(width).ok()
        }
    }
}
