use lyra_ast::{CallExpr, Expr};

use crate::member::{
    ArrayMethodKind, ArrayReceiverInfo, ArrayReceiverKind, AssocKey, BuiltinMethodKind,
    ReceiverInfo, StringMethodKind,
};
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
        ArrayMethodKind::Size
        | ArrayMethodKind::Num
        | ArrayMethodKind::PopFront
        | ArrayMethodKind::PopBack => None,
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
        ArrayReceiverKind::Assoc => match &recv.assoc_key {
            Some(AssocKey::Known(key_ty)) => check_typed_key_arg(arg, key_ty, ctx),
            Some(AssocKey::Wildcard | AssocKey::Unknown) => {
                Some(ExprType::error(ExprTypeErrorKind::MethodKeyTypeUnknown))
            }
            None => None,
        },
        _ => None,
    }
}

fn check_assoc_key_arg(
    args: &[Expr],
    recv: &ArrayReceiverInfo,
    ctx: &dyn InferCtx,
    require_lvalue: bool,
) -> Option<ExprType> {
    if let Some(arg) = args.first()
        && let Some(AssocKey::Known(key_ty)) = &recv.assoc_key
    {
        if let Some(err) = check_typed_key_arg(arg, key_ty, ctx) {
            return Some(err);
        }
        if require_lvalue && !is_assignable_ref(arg) {
            return Some(ExprType::error(ExprTypeErrorKind::MethodArgNotLvalue));
        }
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
