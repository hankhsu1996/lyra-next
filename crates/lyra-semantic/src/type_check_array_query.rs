use crate::Site;
use crate::site;
use crate::system_functions::{BitsArgKind, SystemFnKind, classify_bits_arg};
use crate::type_check::{TypeCheckCtx, TypeCheckItem, require_site, tf_arg_expr};
use crate::type_infer::ExprView;
use crate::types::{Ty, UnpackedDim, collect_array_dims};
use lyra_ast::{AstIdMap, SystemTfCall, TfArg};

/// Whether the array query is a range function or a counting function.
pub(crate) enum ArrayQueryKind {
    /// `$left`, `$right`, `$low`, `$high`, `$size`, `$increment`
    Range,
    /// `$dimensions`, `$unpacked_dimensions`
    Count,
}

/// Whether the first argument is a type name or a variable expression.
pub(crate) enum CallForm {
    /// First argument resolves to a type (e.g. `$size(my_typedef)`).
    TypeForm,
    /// First argument is a variable expression (e.g. `$size(arr)`).
    ValueForm,
}

/// Diagnostic produced by array query legality checking.
pub(crate) enum ArrayQueryDiag {
    /// Type-form on a dynamically-sized type (range queries only).
    DynamicTypeForm,
    /// Dimension argument selects a variable-sized dimension.
    VarSizedDimByNumber,
}

/// Inputs to the legality check.
pub(crate) struct ArrayQueryCall<'a> {
    pub kind: ArrayQueryKind,
    pub form: CallForm,
    pub fn_name: &'a str,
    pub operand_ty: &'a Ty,
    pub dim_num: Option<u32>,
}

/// Check whether an array query call is legal per LRM 20.7.
///
/// Returns `Ok(())` if legal, or `Err(diag)` describing the violation.
pub(crate) fn check_array_query_legality(call: &ArrayQueryCall<'_>) -> Result<(), ArrayQueryDiag> {
    if matches!(call.form, CallForm::TypeForm)
        && matches!(call.kind, ArrayQueryKind::Range)
        && has_variable_sized_dim(call.operand_ty)
    {
        return Err(ArrayQueryDiag::DynamicTypeForm);
    }

    if matches!(call.form, CallForm::ValueForm)
        && let Some(n) = call.dim_num
        && n >= 1
        && dim_is_variable_sized(call.operand_ty, n)
    {
        return Err(ArrayQueryDiag::VarSizedDimByNumber);
    }

    Ok(())
}

/// Orchestrate array query type checking for a `SystemTfCall` node.
///
/// Classifies the call arguments, runs legality checks, and emits
/// `TypeCheckItem` findings for illegal usage.
pub(crate) fn check_array_query_call(
    stf: &SystemTfCall,
    args: &[TfArg],
    entry: &crate::system_functions::SystemFnEntry,
    map: &AstIdMap,
    ctx: &dyn TypeCheckCtx,
    fallback: Site,
    items: &mut Vec<TypeCheckItem>,
) {
    let Some(first) = args.first() else {
        return;
    };

    let query_kind = match entry.kind {
        SystemFnKind::ArrayQuery => ArrayQueryKind::Range,
        SystemFnKind::ArrayQueryCount => ArrayQueryKind::Count,
        _ => return,
    };
    let fn_name = entry.name;

    // Classify the first argument as type-form or value-form, reusing
    // the same classifier used by `$bits`.
    let bits_kind = classify_bits_arg(first, map, &|utr| ctx.resolve_type_arg(utr));
    let (form, operand_ty) = match bits_kind {
        BitsArgKind::Type(ty) => (CallForm::TypeForm, ty),
        BitsArgKind::Expr => {
            let Some(first_expr) = tf_arg_expr(first) else {
                return;
            };
            let et = ctx.expr_type(first_expr);
            if matches!(et.view, ExprView::Error(_)) {
                return;
            }
            (CallForm::ValueForm, et.ty)
        }
    };

    // If a second argument exists, try to const-evaluate it as a dimension number.
    let dim_num = args.get(1).and_then(|a| {
        let expr = tf_arg_expr(a)?;
        let val = ctx.const_eval_int(expr)?;
        u32::try_from(val).ok()
    });

    let call = ArrayQueryCall {
        kind: query_kind,
        form,
        fn_name,
        operand_ty: &operand_ty,
        dim_num,
    };

    if let Err(diag) = check_array_query_legality(&call) {
        let call_site = require_site(site::opt_site_of(map, stf), fallback, items);
        match diag {
            ArrayQueryDiag::DynamicTypeForm => {
                let arg_site = require_site(site::opt_site_of(map, first), call_site, items);
                items.push(TypeCheckItem::ArrayQueryDynTypeForm {
                    call_site,
                    arg_site,
                    fn_name: smol_str::SmolStr::new(call.fn_name),
                });
            }
            ArrayQueryDiag::VarSizedDimByNumber => {
                let dim_arg = args.get(1);
                let dim_arg_site = dim_arg
                    .and_then(|a| site::opt_site_of(map, a))
                    .unwrap_or(call_site);
                items.push(TypeCheckItem::ArrayQueryVarSizedDimByNumber {
                    call_site,
                    dim_arg_site,
                    fn_name: smol_str::SmolStr::new(call.fn_name),
                });
            }
        }
    }
}

/// Whether the type has any variable-sized unpacked dimension.
fn has_variable_sized_dim(ty: &Ty) -> bool {
    let (_base, dims) = collect_array_dims(ty);
    dims.iter().any(|d| is_variable_dim(d))
}

/// Whether unpacked dimension number `n` (1-based) is variable-sized.
///
/// Dimension numbering follows LRM 20.7: dimension 1 is the outermost
/// unpacked dimension, dimension 2 is the next, etc. Dimensions beyond
/// the unpacked count refer to packed dimensions (never variable-sized).
fn dim_is_variable_sized(ty: &Ty, n: u32) -> bool {
    let (_base, dims) = collect_array_dims(ty);
    let idx = (n as usize).wrapping_sub(1);
    dims.get(idx).is_some_and(|d| is_variable_dim(d))
}

fn is_variable_dim(dim: &UnpackedDim) -> bool {
    matches!(
        dim,
        UnpackedDim::Unsized | UnpackedDim::Queue { .. } | UnpackedDim::Assoc(_)
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{AssocIndex, ConstInt};

    fn int_ty() -> Ty {
        Ty::int()
    }

    fn dyn_array_of(elem: Ty) -> Ty {
        Ty::Array {
            elem: Box::new(elem),
            dim: UnpackedDim::Unsized,
        }
    }

    fn fixed_array_of(elem: Ty, size: u32) -> Ty {
        Ty::Array {
            elem: Box::new(elem),
            dim: UnpackedDim::Size(ConstInt::Known(i64::from(size))),
        }
    }

    fn queue_of(elem: Ty) -> Ty {
        Ty::Array {
            elem: Box::new(elem),
            dim: UnpackedDim::Queue { bound: None },
        }
    }

    fn assoc_of(elem: Ty) -> Ty {
        Ty::Array {
            elem: Box::new(elem),
            dim: UnpackedDim::Assoc(AssocIndex::Wildcard),
        }
    }

    #[test]
    fn type_form_range_on_dynamic_array_is_error() {
        let ty = dyn_array_of(int_ty());
        let call = ArrayQueryCall {
            kind: ArrayQueryKind::Range,
            form: CallForm::TypeForm,
            fn_name: "$size",
            operand_ty: &ty,
            dim_num: None,
        };
        assert!(matches!(
            check_array_query_legality(&call),
            Err(ArrayQueryDiag::DynamicTypeForm)
        ));
    }

    #[test]
    fn type_form_count_on_dynamic_array_is_ok() {
        let ty = dyn_array_of(int_ty());
        let call = ArrayQueryCall {
            kind: ArrayQueryKind::Count,
            form: CallForm::TypeForm,
            fn_name: "$dimensions",
            operand_ty: &ty,
            dim_num: None,
        };
        assert!(check_array_query_legality(&call).is_ok());
    }

    #[test]
    fn type_form_range_on_queue_is_error() {
        let ty = queue_of(int_ty());
        let call = ArrayQueryCall {
            kind: ArrayQueryKind::Range,
            form: CallForm::TypeForm,
            fn_name: "$left",
            operand_ty: &ty,
            dim_num: None,
        };
        assert!(matches!(
            check_array_query_legality(&call),
            Err(ArrayQueryDiag::DynamicTypeForm)
        ));
    }

    #[test]
    fn type_form_range_on_assoc_is_error() {
        let ty = assoc_of(int_ty());
        let call = ArrayQueryCall {
            kind: ArrayQueryKind::Range,
            form: CallForm::TypeForm,
            fn_name: "$right",
            operand_ty: &ty,
            dim_num: None,
        };
        assert!(matches!(
            check_array_query_legality(&call),
            Err(ArrayQueryDiag::DynamicTypeForm)
        ));
    }

    #[test]
    fn type_form_range_on_fixed_array_is_ok() {
        let ty = fixed_array_of(int_ty(), 10);
        let call = ArrayQueryCall {
            kind: ArrayQueryKind::Range,
            form: CallForm::TypeForm,
            fn_name: "$size",
            operand_ty: &ty,
            dim_num: None,
        };
        assert!(check_array_query_legality(&call).is_ok());
    }

    #[test]
    fn value_form_dim_selects_variable_dim() {
        // int a[3][][5] -- dim 1 = Size(3), dim 2 = Unsized, dim 3 = Size(5)
        let inner = fixed_array_of(int_ty(), 5);
        let mid = dyn_array_of(inner);
        let outer = fixed_array_of(mid, 3);
        let call = ArrayQueryCall {
            kind: ArrayQueryKind::Range,
            form: CallForm::ValueForm,
            fn_name: "$size",
            operand_ty: &outer,
            dim_num: Some(2),
        };
        assert!(matches!(
            check_array_query_legality(&call),
            Err(ArrayQueryDiag::VarSizedDimByNumber)
        ));
    }

    #[test]
    fn value_form_dim_selects_fixed_dim_is_ok() {
        let inner = fixed_array_of(int_ty(), 5);
        let mid = dyn_array_of(inner);
        let outer = fixed_array_of(mid, 3);
        let call = ArrayQueryCall {
            kind: ArrayQueryKind::Range,
            form: CallForm::ValueForm,
            fn_name: "$size",
            operand_ty: &outer,
            dim_num: Some(1),
        };
        assert!(check_array_query_legality(&call).is_ok());
    }

    #[test]
    fn value_form_dim_beyond_unpacked_is_ok() {
        let ty = fixed_array_of(int_ty(), 4);
        let call = ArrayQueryCall {
            kind: ArrayQueryKind::Range,
            form: CallForm::ValueForm,
            fn_name: "$size",
            operand_ty: &ty,
            dim_num: Some(5),
        };
        assert!(check_array_query_legality(&call).is_ok());
    }

    #[test]
    fn value_form_no_dim_arg_is_ok() {
        let ty = dyn_array_of(int_ty());
        let call = ArrayQueryCall {
            kind: ArrayQueryKind::Range,
            form: CallForm::ValueForm,
            fn_name: "$size",
            operand_ty: &ty,
            dim_num: None,
        };
        assert!(check_array_query_legality(&call).is_ok());
    }
}
