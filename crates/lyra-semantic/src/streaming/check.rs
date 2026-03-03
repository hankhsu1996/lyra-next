use lyra_ast::{Expr, StreamRangeOp};
use lyra_source::TextRange;

use crate::Site;
use crate::streaming::shape::{StreamUnpackShape, WithInfo};
use crate::type_check::{TypeCheckCtx, TypeCheckItem};
use crate::types::{Ty, UnpackedDim};

/// Diagnostic anchors for a streaming unpack assignment.
pub(crate) struct UnpackAssignSites {
    pub(crate) assign_site: Site,
    pub(crate) op_range: Option<TextRange>,
    pub(crate) lhs_site: Site,
    pub(crate) rhs_site: Site,
}

/// First dynamically sized target eligible to absorb remaining bits.
struct GreedyAbsorber {
    site: Site,
    elem_bits: u32,
}

/// Check a streaming unpack shape against its RHS, emitting diagnostics.
///
/// One pass over operands in source order collects fixed-width bits and
/// identifies the first greedy absorber (dynamic array or queue without
/// `with` clause whose element type has fixed streaming width). Width
/// validation at the end accounts for greedy absorbers per LRM 11.4.14.4.
pub(crate) fn check_streaming_unpack(
    shape: &StreamUnpackShape,
    rhs: &Expr,
    sites: &UnpackAssignSites,
    ctx: &dyn TypeCheckCtx,
    items: &mut Vec<TypeCheckItem>,
) {
    let mut fixed_bits: Option<u32> = Some(0);
    let mut greedy: Option<GreedyAbsorber> = None;

    for item in &shape.items {
        if !item.target.is_assignable() {
            items.push(TypeCheckItem::StreamUnpackOperandInvalid {
                operand_site: item.diag_site(),
            });
            fixed_bits = None;
            continue;
        }

        let Some(eid) = item.expr_id else {
            fixed_bits = None;
            continue;
        };

        if let Some(ref wi) = item.with_clause {
            let operand_et = ctx.expr_type_by_id(eid);
            let Ty::Array { ref elem, .. } = operand_et.ty else {
                items.push(TypeCheckItem::StreamWithNonArray {
                    with_site: wi.with_site,
                });
                fixed_bits = None;
                continue;
            };
            if let Some(total) = fixed_bits {
                let elem_bits = ctx.fixed_stream_width_bits_of_ty(elem);
                let elem_count = stream_with_selected_elems(wi, ctx);
                fixed_bits = match (elem_bits, elem_count) {
                    (Some(eb), Some(ec)) => eb.checked_mul(ec).and_then(|p| total.checked_add(p)),
                    _ => None,
                };
            }
        } else {
            let operand_bits = ctx.fixed_stream_width_bits(eid);
            if let Some(w) = operand_bits {
                fixed_bits = fixed_bits.and_then(|total| total.checked_add(w));
            } else {
                // No fixed width -- check if this is an eligible greedy absorber.
                let operand_et = ctx.expr_type_by_id(eid);
                if let Some(elem_bits) = eligible_greedy_absorber(&operand_et.ty, ctx) {
                    if greedy.is_none() {
                        greedy = Some(GreedyAbsorber {
                            site: item.diag_site(),
                            elem_bits,
                        });
                    }
                    // Later dynamic targets get 0 elements (LRM): no
                    // width contribution and no diagnostic.
                } else {
                    items.push(TypeCheckItem::StreamUnpackOperandUnsupported {
                        operand_site: item.diag_site(),
                        operand_ty: operand_et.ty.clone(),
                    });
                    fixed_bits = None;
                }
            }
        }
    }

    let Some(fixed_w) = fixed_bits else {
        return;
    };
    let rhs_et = ctx.expr_type(rhs);
    let Some(rhs_w) = ctx.fixed_stream_width_bits_of_type(&rhs_et) else {
        return;
    };

    if let Some(ref g) = greedy {
        if rhs_w < fixed_w {
            items.push(TypeCheckItem::StreamUnpackWidthMismatch {
                assign_site: sites.assign_site,
                op_range: sites.op_range,
                lhs_site: sites.lhs_site,
                rhs_site: sites.rhs_site,
                lhs_width: fixed_w,
                rhs_width: rhs_w,
            });
        } else {
            let remaining = rhs_w - fixed_w;
            if remaining % g.elem_bits != 0 {
                items.push(TypeCheckItem::StreamUnpackGreedyRemainder {
                    assign_site: sites.assign_site,
                    greedy_site: g.site,
                    rhs_site: sites.rhs_site,
                    remaining,
                    elem_width: g.elem_bits,
                });
            }
        }
    } else if fixed_w != rhs_w {
        items.push(TypeCheckItem::StreamUnpackWidthMismatch {
            assign_site: sites.assign_site,
            op_range: sites.op_range,
            lhs_site: sites.lhs_site,
            rhs_site: sites.rhs_site,
            lhs_width: fixed_w,
            rhs_width: rhs_w,
        });
    }
}

/// Check if a type is an eligible greedy absorber (dynamic array or queue
/// whose element type has fixed streaming width).
///
/// Returns `Some(elem_bits)` when eligible, `None` otherwise.
fn eligible_greedy_absorber(ty: &Ty, ctx: &dyn TypeCheckCtx) -> Option<u32> {
    let Ty::Array { elem, dim } = ty else {
        return None;
    };
    match dim {
        UnpackedDim::Unsized | UnpackedDim::Queue { .. } => ctx.fixed_stream_width_bits_of_ty(elem),
        _ => None,
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
