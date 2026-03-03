use crate::Site;
use crate::site;
use crate::system_functions::{BitsArgKind, SystemFnKind, classify_bits_arg, lookup_builtin};
use crate::type_check::{TypeCheckCtx, TypeCheckItem, require_site, tf_arg_expr};
use crate::type_check_array_query;
use crate::type_infer::ExprView;
use crate::types::Ty;
use lyra_ast::{AstIdMap, SystemTfCall, TfArg};

/// Check a `SystemTfCall` for argument type errors.
pub fn check_system_call(
    stf: &SystemTfCall,
    ctx: &dyn TypeCheckCtx,
    fallback: Site,
    items: &mut Vec<TypeCheckItem>,
) {
    let Some(tok) = stf.system_name() else {
        return;
    };
    let name = tok.text();
    let Some(entry) = lookup_builtin(name) else {
        return;
    };
    let Some(al) = stf.arg_list() else {
        return;
    };
    let args: Vec<TfArg> = al.args().collect();
    let map = ctx.ast_id_map();
    match entry.kind {
        SystemFnKind::Bits => check_bits_call(stf, &args, map, ctx, fallback, items),
        SystemFnKind::IntToReal => {
            check_conversion_arg_integral(stf, &args, name, map, ctx, fallback, items);
        }
        SystemFnKind::RealToInt | SystemFnKind::RealToBits | SystemFnKind::ShortRealToBits => {
            check_conversion_arg_real(stf, &args, name, map, ctx, fallback, items);
        }
        SystemFnKind::BitsToReal => {
            check_conversion_bits_to_real(stf, &args, name, 64, ctx, fallback, items);
        }
        SystemFnKind::BitsToShortReal => {
            check_conversion_bits_to_real(stf, &args, name, 32, ctx, fallback, items);
        }
        SystemFnKind::ArrayQuery | SystemFnKind::ArrayQueryCount => {
            type_check_array_query::check_array_query_call(
                stf, &args, entry, map, ctx, fallback, items,
            );
        }
        _ => {}
    }
}

fn check_bits_call(
    stf: &SystemTfCall,
    args: &[TfArg],
    map: &AstIdMap,
    ctx: &dyn TypeCheckCtx,
    fallback: Site,
    items: &mut Vec<TypeCheckItem>,
) {
    let Some(first) = args.first() else {
        return;
    };
    let kind = classify_bits_arg(first, map, &|utr| ctx.resolve_type_arg(utr));
    if let BitsArgKind::Type(ty) = kind
        && ty.as_data_view().is_none()
    {
        let call_site = require_site(site::opt_site_of(map, stf), fallback, items);
        let arg_site = require_site(site::opt_site_of(map, first), call_site, items);
        items.push(TypeCheckItem::BitsNonDataType {
            call_site,
            arg_site,
        });
    }
}

fn check_conversion_arg_integral(
    stf: &SystemTfCall,
    args: &[TfArg],
    fn_name: &str,
    map: &AstIdMap,
    ctx: &dyn TypeCheckCtx,
    fallback: Site,
    items: &mut Vec<TypeCheckItem>,
) {
    let Some(first) = args.first() else {
        return;
    };
    let Some(first_expr) = tf_arg_expr(first) else {
        return;
    };
    let arg_ty = ctx.expr_type(first_expr);
    if matches!(arg_ty.view, ExprView::Error(_)) {
        return;
    }
    if !matches!(arg_ty.ty, Ty::Integral(_) | Ty::Enum(_)) {
        let call_site = require_site(site::opt_site_of(map, stf), fallback, items);
        let arg_site = require_site(site::opt_site_of(map, first), call_site, items);
        items.push(TypeCheckItem::ConversionArgCategory {
            call_site,
            arg_site,
            fn_name: smol_str::SmolStr::new(fn_name),
            expected: "integral",
        });
    }
}

fn check_conversion_arg_real(
    stf: &SystemTfCall,
    args: &[TfArg],
    fn_name: &str,
    map: &AstIdMap,
    ctx: &dyn TypeCheckCtx,
    fallback: Site,
    items: &mut Vec<TypeCheckItem>,
) {
    let Some(first) = args.first() else {
        return;
    };
    let Some(first_expr) = tf_arg_expr(first) else {
        return;
    };
    let arg_ty = ctx.expr_type(first_expr);
    if matches!(arg_ty.view, ExprView::Error(_)) {
        return;
    }
    if !arg_ty.ty.is_real() {
        let call_site = require_site(site::opt_site_of(map, stf), fallback, items);
        let arg_site = require_site(site::opt_site_of(map, first), call_site, items);
        items.push(TypeCheckItem::ConversionArgCategory {
            call_site,
            arg_site,
            fn_name: smol_str::SmolStr::new(fn_name),
            expected: "real",
        });
    }
}

fn check_conversion_bits_to_real(
    stf: &SystemTfCall,
    args: &[TfArg],
    fn_name: &str,
    expected_width: u32,
    ctx: &dyn TypeCheckCtx,
    fallback: Site,
    items: &mut Vec<TypeCheckItem>,
) {
    let map = ctx.ast_id_map();
    let Some(first) = args.first() else {
        return;
    };
    let Some(first_expr) = tf_arg_expr(first) else {
        return;
    };
    let arg_ty = ctx.expr_type(first_expr);
    if matches!(arg_ty.view, ExprView::Error(_)) {
        return;
    }
    let call_site = require_site(site::opt_site_of(map, stf), fallback, items);
    let arg_site = require_site(site::opt_site_of(map, first), call_site, items);
    if !matches!(arg_ty.ty, Ty::Integral(_)) {
        items.push(TypeCheckItem::ConversionArgCategory {
            call_site,
            arg_site,
            fn_name: smol_str::SmolStr::new(fn_name),
            expected: "integral",
        });
        return;
    }
    if let Some(actual_width) = crate::type_check::bitvec_known_width(&arg_ty)
        && actual_width != expected_width
    {
        items.push(TypeCheckItem::ConversionWidthMismatch {
            call_site,
            arg_site,
            fn_name: smol_str::SmolStr::new(fn_name),
            expected_width,
            actual_width,
        });
    }
}
