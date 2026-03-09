use lyra_ast::{AssignmentPatternExpr, AssignmentPatternItemKind, Expr, ExprKind};

use crate::Site;
use crate::site;
use crate::type_check::{TypeCheckCtx, TypeCheckItem, require_site};
use crate::type_infer::ExprView;
use crate::types::{AssocIndex, Ty, UnpackedDim};

/// Check an assignment pattern expression against its expected LHS type.
///
/// Dispatches to per-target-kind validation. Currently implements
/// associative-array target checking (LRM 7.9.11). Other target kinds
/// (struct, fixed-size array, queue, class) are not yet validated.
pub fn check_assignment_pattern(
    expr: &Expr,
    lhs_ty: &Ty,
    ctx: &dyn TypeCheckCtx,
    items: &mut Vec<TypeCheckItem>,
) {
    let Some(peeled) = expr.peeled() else {
        return;
    };
    let Some(ExprKind::AssignmentPatternExpr(ap)) = peeled.classify() else {
        return;
    };

    match lhs_ty {
        Ty::Array {
            elem,
            dim: UnpackedDim::Assoc(assoc_idx),
        } => {
            check_assoc_array_pattern(&ap, elem, assoc_idx, ctx, items);
        }
        // Struct, fixed-size array, queue, class targets: not yet validated.
        Ty::Record(_)
        | Ty::Array { .. }
        | Ty::Integral(_)
        | Ty::Enum(_)
        | Ty::String
        | Ty::Real(_)
        | Ty::Chandle
        | Ty::Event
        | Ty::Void
        | Ty::Interface(_)
        | Ty::Error => {}
    }
}

fn check_assoc_array_pattern(
    ap: &AssignmentPatternExpr,
    elem_ty: &Ty,
    assoc_idx: &AssocIndex,
    ctx: &dyn TypeCheckCtx,
    items: &mut Vec<TypeCheckItem>,
) {
    let map = ctx.ast_id_map();
    let Some(fallback) = site::opt_site_of(map, ap) else {
        return;
    };

    let mut seen_default = false;

    for item in ap.items() {
        let item_site = require_site(site::opt_site_of(map, &item), fallback, items);

        let Some(kind) = item.kind() else {
            continue;
        };

        match kind {
            AssignmentPatternItemKind::Positional(_) => {
                items.push(TypeCheckItem::AssignPatternPositionalInAssocArray { item_site });
            }
            AssignmentPatternItemKind::Default { ref value } => {
                if seen_default {
                    items.push(TypeCheckItem::AssignPatternDuplicateDefault { item_site });
                }
                seen_default = true;
                check_value_against_elem(value, elem_ty, ctx);
            }
            AssignmentPatternItemKind::Keyed { ref key, ref value } => {
                check_key_against_index(key, assoc_idx, item_site, ctx, items);
                check_value_against_elem(value, elem_ty, ctx);
            }
        }
    }
}

fn check_key_against_index(
    key: &Expr,
    assoc_idx: &AssocIndex,
    item_site: Site,
    ctx: &dyn TypeCheckCtx,
    items: &mut Vec<TypeCheckItem>,
) {
    let key_type = ctx.expr_type(key);
    if matches!(key_type.ty, Ty::Error) || matches!(key_type.view, ExprView::Error(_)) {
        return;
    }

    match assoc_idx {
        AssocIndex::Wildcard => {}
        AssocIndex::Typed(expected_key_ty) => {
            if !key_type_compatible(&key_type.ty, expected_key_ty) {
                items.push(TypeCheckItem::AssignPatternKeyTypeMismatch {
                    item_site,
                    expected: expected_key_ty.as_ref().clone(),
                    actual: key_type.ty.clone(),
                });
            }
        }
    }
}

fn key_type_compatible(actual: &Ty, expected: &Ty) -> bool {
    if actual == expected {
        return true;
    }
    matches!((actual, expected), (Ty::Integral(_), Ty::Integral(_)))
}

fn check_value_against_elem(value: &Expr, elem_ty: &Ty, ctx: &dyn TypeCheckCtx) {
    // Trigger type inference with expected type so downstream
    // assignment-compat diagnostics fire through existing machinery.
    let _value_type = ctx.expr_type_with_expected(value, elem_ty);
}
