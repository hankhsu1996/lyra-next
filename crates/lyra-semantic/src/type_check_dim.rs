use lyra_ast::{Expr, NetDecl, Port, TypedefDecl, UnpackedDimension};

use crate::site;
use crate::type_check::{TypeCheckCtx, TypeCheckItem};
use crate::types::{ConstEvalError, ConstInt};

/// Validate legality of an unpacked dimension (queue bound rules, etc.).
pub(crate) fn check_unpacked_dim_legality(
    ctx: &dyn TypeCheckCtx,
    dim: &UnpackedDimension,
    items: &mut Vec<TypeCheckItem>,
) {
    let kind = dim.classify();
    if let lyra_ast::UnpackedDimKind::Queue {
        bound: Some(ref bound_expr),
    } = kind
    {
        check_queue_bound(ctx, bound_expr, items);
    }
}

fn check_queue_bound(ctx: &dyn TypeCheckCtx, bound_expr: &Expr, items: &mut Vec<TypeCheckItem>) {
    let Some(bound_site) = site::opt_site_of(ctx.ast_id_map(), bound_expr) else {
        return;
    };

    match ctx.const_eval_int_by_site_full(bound_site) {
        ConstInt::Known(v) if v > 0 => {}
        ConstInt::Known(v) => {
            items.push(TypeCheckItem::QueueBoundNotPositive {
                bound_site,
                bound: v,
            });
        }
        ConstInt::Error(ConstEvalError::NonConstant) => {
            items.push(TypeCheckItem::QueueBoundNotConst { bound_site });
        }
        _ => {}
    }
}

/// Check unpacked dimensions on a `NetDecl` (LRM 7.10.5).
pub fn check_net_decl(
    nd: &NetDecl,
    ctx: &dyn TypeCheckCtx,
    _fallback: crate::Site,
    items: &mut Vec<TypeCheckItem>,
) {
    for decl in nd.declarators() {
        for dim in decl.unpacked_dimensions() {
            check_unpacked_dim_legality(ctx, &dim, items);
        }
    }
}

/// Check unpacked dimensions on a `TypedefDecl` (LRM 7.10.5).
pub fn check_typedef_decl(
    td: &TypedefDecl,
    ctx: &dyn TypeCheckCtx,
    _fallback: crate::Site,
    items: &mut Vec<TypeCheckItem>,
) {
    for dim in td.unpacked_dimensions() {
        check_unpacked_dim_legality(ctx, &dim, items);
    }
}

/// Check unpacked dimensions on a `Port` (LRM 7.10.5).
pub fn check_port_decl(
    port: &Port,
    ctx: &dyn TypeCheckCtx,
    _fallback: crate::Site,
    items: &mut Vec<TypeCheckItem>,
) {
    for dim in port.unpacked_dimensions() {
        check_unpacked_dim_legality(ctx, &dim, items);
    }
}
