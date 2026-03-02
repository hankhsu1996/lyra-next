use lyra_ast::Expr;
use lyra_source::TextRange;

use crate::Site;
use crate::streaming::shape::StreamUnpackShape;
use crate::type_check::{TypeCheckCtx, TypeCheckItem};

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
        if !item.target.is_assignable() {
            items.push(TypeCheckItem::StreamUnpackOperandInvalid {
                operand_site: item.diag_site(),
            });
            lhs_total_bits = None;
            continue;
        }

        if let Some(ref wi) = item.with_clause {
            items.push(TypeCheckItem::StreamUnpackWithClause {
                with_site: wi.with_site,
            });
            lhs_total_bits = None;
            continue;
        }

        // Assignable, no with clause -- compute operand width.
        let Some(eid) = item.expr_id else {
            lhs_total_bits = None;
            continue;
        };
        let operand_bits = ctx.fixed_stream_width_bits(eid);

        if let Some(w) = operand_bits {
            if let Some(ref mut total) = lhs_total_bits {
                *total = total.saturating_add(w);
            }
        } else {
            let operand_et = ctx.expr_type_by_id(eid);
            items.push(TypeCheckItem::StreamUnpackOperandUnsupported {
                operand_site: item.diag_site(),
                operand_ty: operand_et.ty.clone(),
            });
            lhs_total_bits = None;
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
