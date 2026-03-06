use lyra_ast::{CallExpr, Expr, ExprKind};
use smol_str::SmolStr;

use crate::member::{
    ArrayMethodKind, ArrayReceiverInfo, ArrayReceiverKind, AssocKey, BuiltinMethodKind,
    ReceiverInfo, StringMethodKind,
};
use crate::type_infer::scoped::ScopedInferCtx;
use crate::type_infer::{
    ExprType, ExprTypeErrorKind, ExprView, InferCtx, infer_expr, try_integral_view,
};
use crate::types::Ty;

pub(crate) fn infer_builtin_method_call(
    call: &CallExpr,
    bm: BuiltinMethodKind,
    result_ty: &Ty,
    receiver: Option<&ReceiverInfo>,
    ctx: &dyn InferCtx,
) -> ExprType {
    let args: Vec<Expr> = call
        .arg_list()
        .map(|al| al.args().collect())
        .unwrap_or_default();

    let (min, max) = match bm {
        BuiltinMethodKind::Enum(ek) => ek.arity(),
        BuiltinMethodKind::Array(ak) => ak.arity(),
        BuiltinMethodKind::String(sk) => sk.arity(),
    };
    if args.len() < min || args.len() > max {
        return ExprType::error(ExprTypeErrorKind::MethodArityMismatch);
    }

    match bm {
        BuiltinMethodKind::Enum(ek) => {
            if ek.arg_requires_integral()
                && let Some(arg) = args.first()
            {
                let arg_type = infer_expr(arg, ctx, None);
                if let ExprView::Error(_) = &arg_type.view {
                    return arg_type;
                }
                if try_integral_view(&arg_type, ctx).is_none() {
                    return ExprType::error(ExprTypeErrorKind::MethodArgNotIntegral);
                }
            }
        }
        BuiltinMethodKind::Array(ak) => {
            if let Some(err) = check_array_method_args(ak, &args, receiver, ctx) {
                return err;
            }
            if let Some(override_ty) = check_array_with_clause(call, ak, receiver, ctx) {
                return override_ty;
            }
            if ak.returns_void() {
                return ExprType::error(ExprTypeErrorKind::VoidUsedAsExpr);
            }
        }
        BuiltinMethodKind::String(sk) => {
            return check_string_method(sk, &args, result_ty, ctx);
        }
    }

    ExprType::from_ty(result_ty)
}

/// Type-check the `with (expr)` clause on a 7.12 array method.
///
/// This is the sole place where the with-clause `ScopedInferCtx` is
/// constructed. All iterator variable binding and `item.index()` access
/// flows through the scoped context created here.
///
/// Handles four cases:
/// - `with` on a non-accepting method (`reverse`/`shuffle`) -> `WithClauseNotAccepted`
/// - Missing `with` on methods that require it -> `WithClauseRequired`
/// - `map` -> wraps with-expression type in a dynamic array
/// - Reduction methods -> returns with-expression type as override
fn check_array_with_clause(
    call: &CallExpr,
    ak: ArrayMethodKind,
    receiver: Option<&ReceiverInfo>,
    ctx: &dyn InferCtx,
) -> Option<ExprType> {
    let Some(ReceiverInfo::Array(recv)) = receiver else {
        return None;
    };
    if !ak.accepts_with_clause() {
        if call.with_clause().is_some() {
            return Some(ExprType::error(ExprTypeErrorKind::WithClauseNotAccepted));
        }
        return None;
    }
    let Some(with_clause) = call.with_clause() else {
        if ak.requires_with_clause() {
            return Some(ExprType::error(ExprTypeErrorKind::WithClauseRequired));
        }
        return None;
    };
    let with_expr = with_clause.with_expr()?;
    let iter_name = extract_iter_name(call);
    let item_type = ExprType::from_ty(&recv.elem_ty);
    let scoped = ScopedInferCtx {
        inner: ctx,
        bindings: [(iter_name.clone(), item_type)],
        iter_name: Some(iter_name),
    };
    let with_type = infer_expr(&with_expr, &scoped, None);
    if let ExprView::Error(_) = &with_type.view {
        return Some(with_type);
    }
    if matches!(ak, ArrayMethodKind::Map) {
        return Some(ExprType::from_ty(&Ty::Array {
            elem: Box::new(with_type.ty),
            dim: crate::types::UnpackedDim::Unsized,
        }));
    }
    if ak.is_reduction() {
        return Some(with_type);
    }
    None
}

/// Extract the custom iterator variable name from the first argument of
/// a locator/map method call, or fall back to `"item"` (LRM 7.12 default).
fn extract_iter_name(call: &CallExpr) -> SmolStr {
    if let Some(al) = call.arg_list()
        && let Some(first_arg) = al.args().next()
        && let Some(ExprKind::NameRef(nr)) = first_arg.classify()
        && let Some(ident_tok) = nr.ident()
    {
        return SmolStr::new(ident_tok.text());
    }
    SmolStr::new_static("item")
}

fn check_array_method_args(
    ak: ArrayMethodKind,
    args: &[Expr],
    receiver: Option<&ReceiverInfo>,
    ctx: &dyn InferCtx,
) -> Option<ExprType> {
    let Some(ReceiverInfo::Array(recv)) = receiver else {
        return None;
    };

    match ak {
        ArrayMethodKind::Delete => check_delete_args(args, recv, ctx),
        ArrayMethodKind::Exists => check_assoc_key_arg(args, recv, ctx, false),
        ArrayMethodKind::First
        | ArrayMethodKind::Last
        | ArrayMethodKind::Next
        | ArrayMethodKind::Prev => check_assoc_key_arg(args, recv, ctx, true),
        ArrayMethodKind::Insert => check_insert_args(args, recv, ctx),
        ArrayMethodKind::PushFront | ArrayMethodKind::PushBack => check_elem_arg(args, recv, ctx),
        _ => None,
    }
}

fn check_delete_args(
    args: &[Expr],
    recv: &ArrayReceiverInfo,
    ctx: &dyn InferCtx,
) -> Option<ExprType> {
    let arg = args.first()?;
    match recv.kind {
        ArrayReceiverKind::Queue => {
            let arg_type = infer_expr(arg, ctx, None);
            if let ExprView::Error(_) = &arg_type.view {
                return Some(arg_type);
            }
            if try_integral_view(&arg_type, ctx).is_none() {
                return Some(ExprType::error(ExprTypeErrorKind::MethodArgNotIntegral));
            }
            None
        }
        ArrayReceiverKind::Assoc => check_assoc_key_arg(args, recv, ctx, false),
        _ => None,
    }
}

/// Validate a key argument on an associative array method.
///
/// Dispatches by associative key class:
/// - known key: assignment-compatible with the declared key type
/// - wildcard key: any integral type (LRM 7.8.1)
/// - unknown key: deferred (no validation)
///
/// After key-domain validation, enforces `require_lvalue` uniformly.
fn check_assoc_key_arg(
    args: &[Expr],
    recv: &ArrayReceiverInfo,
    ctx: &dyn InferCtx,
    require_lvalue: bool,
) -> Option<ExprType> {
    let arg = args.first()?;
    let key_err = match &recv.assoc_key {
        Some(AssocKey::Known(key_ty)) => check_typed_key_arg(arg, key_ty, ctx),
        Some(AssocKey::Wildcard) => check_wildcard_key_arg(arg, ctx),
        Some(AssocKey::Unknown) | None => return None,
    };
    if key_err.is_some() {
        return key_err;
    }
    if require_lvalue && !is_assignable_ref(arg) {
        return Some(ExprType::error(ExprTypeErrorKind::MethodArgNotLvalue));
    }
    None
}

/// Validate a key argument against wildcard associative key rules.
///
/// Per LRM 7.8.1, wildcard associative arrays accept any integral key.
fn check_wildcard_key_arg(arg: &Expr, ctx: &dyn InferCtx) -> Option<ExprType> {
    let arg_type = infer_expr(arg, ctx, None);
    if let ExprView::Error(_) = &arg_type.view {
        return Some(arg_type);
    }
    if try_integral_view(&arg_type, ctx).is_none() {
        return Some(ExprType::error(ExprTypeErrorKind::MethodArgNotIntegral));
    }
    None
}

fn check_insert_args(
    args: &[Expr],
    recv: &ArrayReceiverInfo,
    ctx: &dyn InferCtx,
) -> Option<ExprType> {
    if args.len() >= 2 {
        let idx_type = infer_expr(&args[0], ctx, None);
        if let ExprView::Error(_) = &idx_type.view {
            return Some(idx_type);
        }
        if try_integral_view(&idx_type, ctx).is_none() {
            return Some(ExprType::error(ExprTypeErrorKind::MethodArgNotIntegral));
        }
        let item_type = infer_expr(&args[1], ctx, None);
        if let ExprView::Error(_) = &item_type.view {
            return Some(item_type);
        }
        if !check_arg_assignable(&recv.elem_ty, &item_type.ty) {
            return Some(ExprType::error(ExprTypeErrorKind::MethodArgTypeMismatch));
        }
    }
    None
}

fn check_elem_arg(args: &[Expr], recv: &ArrayReceiverInfo, ctx: &dyn InferCtx) -> Option<ExprType> {
    if let Some(arg) = args.first() {
        let arg_type = infer_expr(arg, ctx, None);
        if let ExprView::Error(_) = &arg_type.view {
            return Some(arg_type);
        }
        if !check_arg_assignable(&recv.elem_ty, &arg_type.ty) {
            return Some(ExprType::error(ExprTypeErrorKind::MethodArgTypeMismatch));
        }
    }
    None
}

fn check_typed_key_arg(arg: &Expr, key_ty: &Ty, ctx: &dyn InferCtx) -> Option<ExprType> {
    let arg_type = infer_expr(arg, ctx, None);
    if let ExprView::Error(_) = &arg_type.view {
        return Some(arg_type);
    }
    if !check_arg_assignable(key_ty, &arg_type.ty) {
        return Some(ExprType::error(ExprTypeErrorKind::MethodArgTypeMismatch));
    }
    None
}

fn check_arg_assignable(expected: &Ty, actual: &Ty) -> bool {
    if matches!(expected, Ty::Error) || matches!(actual, Ty::Error) {
        return true;
    }
    match (expected, actual) {
        (Ty::Integral(_), Ty::Integral(_)) | (Ty::Real(_), Ty::Real(_)) => true,
        _ => expected == actual,
    }
}

fn check_string_method(
    sk: StringMethodKind,
    args: &[Expr],
    result_ty: &Ty,
    ctx: &dyn InferCtx,
) -> ExprType {
    let sig = sk.sig();
    for (i, param_ty) in sig.params.iter().enumerate() {
        if let Some(arg) = args.get(i) {
            let arg_type = infer_expr(arg, ctx, None);
            if let ExprView::Error(_) = &arg_type.view {
                return arg_type;
            }
            if !matches!(arg_type.ty, Ty::Error) && !param_ty.accepts(&arg_type.ty) {
                return ExprType::error(ExprTypeErrorKind::MethodArgTypeMismatch);
            }
        }
    }
    if sk.returns_void() {
        return ExprType::error(ExprTypeErrorKind::VoidUsedAsExpr);
    }
    ExprType::from_ty(result_ty)
}

use crate::lhs::is_assignable_ref;
