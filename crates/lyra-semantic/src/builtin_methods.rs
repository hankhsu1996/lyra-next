use lyra_parser::SyntaxNode;

use crate::member::{
    ArrayMethodKind, ArrayReceiverInfo, ArrayReceiverKind, AssocKey, BuiltinMethodKind,
    ReceiverInfo,
};
use crate::type_infer::{
    ExprType, ExprTypeErrorKind, ExprView, InferCtx, infer_expr_type, try_integral_view,
};
use crate::types::Ty;

pub(crate) fn infer_builtin_method_call(
    call_node: &SyntaxNode,
    bm: BuiltinMethodKind,
    result_ty: &Ty,
    receiver: Option<&ReceiverInfo>,
    ctx: &dyn InferCtx,
) -> ExprType {
    use lyra_ast::{AstNode, CallExpr};

    let args: Vec<SyntaxNode> = CallExpr::cast(call_node.clone())
        .and_then(|c| c.arg_list())
        .map(|al| al.args().collect())
        .unwrap_or_default();

    let (min, max) = match bm {
        BuiltinMethodKind::Enum(ek) => ek.arity(),
        BuiltinMethodKind::Array(ak) => ak.arity(),
    };
    if args.len() < min || args.len() > max {
        return ExprType::error(ExprTypeErrorKind::MethodArityMismatch);
    }

    match bm {
        BuiltinMethodKind::Enum(ek) => {
            if ek.arg_requires_integral()
                && let Some(arg_node) = args.first()
            {
                let arg_type = infer_expr_type(arg_node, ctx, None);
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
    }

    ExprType::from_ty(result_ty)
}

fn check_array_method_args(
    ak: ArrayMethodKind,
    args: &[SyntaxNode],
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
    args: &[SyntaxNode],
    recv: &ArrayReceiverInfo,
    ctx: &dyn InferCtx,
) -> Option<ExprType> {
    let arg_node = args.first()?;
    match recv.kind {
        ArrayReceiverKind::Queue => {
            let arg_type = infer_expr_type(arg_node, ctx, None);
            if let ExprView::Error(_) = &arg_type.view {
                return Some(arg_type);
            }
            if try_integral_view(&arg_type, ctx).is_none() {
                return Some(ExprType::error(ExprTypeErrorKind::MethodArgNotIntegral));
            }
            None
        }
        ArrayReceiverKind::Assoc => match &recv.assoc_key {
            Some(AssocKey::Known(key_ty)) => check_typed_key_arg(arg_node, key_ty, ctx),
            Some(AssocKey::Wildcard | AssocKey::Unknown) => {
                Some(ExprType::error(ExprTypeErrorKind::MethodKeyTypeUnknown))
            }
            None => None,
        },
        _ => None,
    }
}

fn check_assoc_key_arg(
    args: &[SyntaxNode],
    recv: &ArrayReceiverInfo,
    ctx: &dyn InferCtx,
    require_lvalue: bool,
) -> Option<ExprType> {
    if let Some(arg_node) = args.first()
        && let Some(AssocKey::Known(key_ty)) = &recv.assoc_key
    {
        if let Some(err) = check_typed_key_arg(arg_node, key_ty, ctx) {
            return Some(err);
        }
        if require_lvalue && !is_lvalue(arg_node) {
            return Some(ExprType::error(ExprTypeErrorKind::MethodArgNotLvalue));
        }
    }
    None
}

fn check_insert_args(
    args: &[SyntaxNode],
    recv: &ArrayReceiverInfo,
    ctx: &dyn InferCtx,
) -> Option<ExprType> {
    if args.len() >= 2 {
        let idx_type = infer_expr_type(&args[0], ctx, None);
        if let ExprView::Error(_) = &idx_type.view {
            return Some(idx_type);
        }
        if try_integral_view(&idx_type, ctx).is_none() {
            return Some(ExprType::error(ExprTypeErrorKind::MethodArgNotIntegral));
        }
        let item_type = infer_expr_type(&args[1], ctx, None);
        if let ExprView::Error(_) = &item_type.view {
            return Some(item_type);
        }
        if !check_arg_assignable(&recv.elem_ty, &item_type.ty) {
            return Some(ExprType::error(ExprTypeErrorKind::MethodArgTypeMismatch));
        }
    }
    None
}

fn check_elem_arg(
    args: &[SyntaxNode],
    recv: &ArrayReceiverInfo,
    ctx: &dyn InferCtx,
) -> Option<ExprType> {
    if let Some(arg_node) = args.first() {
        let arg_type = infer_expr_type(arg_node, ctx, None);
        if let ExprView::Error(_) = &arg_type.view {
            return Some(arg_type);
        }
        if !check_arg_assignable(&recv.elem_ty, &arg_type.ty) {
            return Some(ExprType::error(ExprTypeErrorKind::MethodArgTypeMismatch));
        }
    }
    None
}

fn check_typed_key_arg(arg_node: &SyntaxNode, key_ty: &Ty, ctx: &dyn InferCtx) -> Option<ExprType> {
    let arg_type = infer_expr_type(arg_node, ctx, None);
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
    expected == actual
}

use crate::expr_lvalue::is_lvalue;
