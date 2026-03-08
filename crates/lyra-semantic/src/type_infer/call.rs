use super::expr_type::{
    CallableKind, CallableSigRef, CalleeFormKind, ExprType, ExprTypeErrorKind, ExprView, InferCtx,
    ResolveCallableError,
};
use super::{infer_expr, infer_expr_with_expected};
use crate::coerce::IntegralCtx;
use crate::member::{MemberInfo, MemberKind, MemberLookupError};
use crate::symbols::GlobalSymbolId;
use crate::types::Ty;
use lyra_ast::{CallExpr, Expr, ExprKind, FieldExpr, SystemTfCall};

pub(super) fn infer_call(call: &CallExpr, ctx: &dyn InferCtx) -> ExprType {
    let Some(callee_expr) = call.callee() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };

    let Some(callee_kind) = callee_expr.classify() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedCalleeForm(
            CalleeFormKind::Other,
        ));
    };

    match callee_kind {
        ExprKind::NameRef(_) | ExprKind::QualifiedName(_) => {}
        ExprKind::FieldExpr(field_expr) => {
            return infer_method_call(call, &field_expr, ctx);
        }
        _ => {
            return ExprType::error(ExprTypeErrorKind::UnsupportedCalleeForm(
                CalleeFormKind::Other,
            ));
        }
    }

    // Resolve callee to a callable symbol
    let sym_id = match ctx.resolve_callable(&callee_expr) {
        Ok(id) => id,
        Err(ResolveCallableError::NotFound) => {
            return ExprType::error(ExprTypeErrorKind::UnresolvedCall);
        }
        Err(ResolveCallableError::NotACallable(kind)) => {
            return ExprType::error(ExprTypeErrorKind::NotACallable(kind));
        }
    };

    // Get callable signature
    let Some(sig) = ctx.callable_sig(sym_id) else {
        return ExprType::error(ExprTypeErrorKind::UnresolvedCall);
    };

    // Task used in expression context
    if sig.kind == CallableKind::Task {
        return ExprType::error(ExprTypeErrorKind::TaskInExprContext);
    }

    check_call_args(call, &sig, ctx);
    ExprType::from_ty(&sig.return_ty)
}

pub(super) fn infer_system_call(stf: &SystemTfCall, ctx: &dyn InferCtx) -> ExprType {
    crate::system_functions::infer_system_call(stf, ctx)
}

/// Check call arguments against the callable signature.
fn check_call_args(call: &CallExpr, sig: &CallableSigRef, ctx: &dyn InferCtx) {
    let args: Vec<Expr> = call
        .arg_list()
        .map(|al| al.args().collect())
        .unwrap_or_default();

    // Infer each argument with expected type from the port signature.
    // Passes both the full port type (for context-determined expressions
    // like tagged unions and `new[]`) and integral context (for width/
    // signedness propagation per LRM 11.6).
    for (i, arg) in args.iter().enumerate() {
        if let Some(port) = sig.ports.get(i) {
            let integral_ctx = {
                let ety = ExprType::from_ty(&port.ty);
                match ety.view {
                    ExprView::BitVec(bv) => Some(IntegralCtx {
                        width: bv.width.self_determined(),
                        signed: bv.signed,
                        four_state: bv.four_state,
                    }),
                    _ => None,
                }
            };
            infer_expr_with_expected(arg, Some(&port.ty), ctx, integral_ctx.as_ref());
        } else {
            infer_expr(arg, ctx, None);
        }
    }
}

fn infer_method_call(call: &CallExpr, field_expr: &FieldExpr, ctx: &dyn InferCtx) -> ExprType {
    let Some((_kind, field_semantic)) = field_expr.member_lookup_name() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let Some(lhs_node) = field_expr.base_expr() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };

    // Iterator pseudo-methods (e.g. item.index()) -- LRM 7.12.4
    if let Some(ret_ty) = ctx.iter_method_return(&lhs_node, &field_semantic) {
        return infer_iter_method_call(call, &ret_ty, ctx);
    }

    let lhs_type = infer_expr(&lhs_node, ctx, None);
    if let ExprView::Error(_) = &lhs_type.view {
        return lhs_type;
    }

    match ctx.member_lookup(&lhs_type.ty, &field_semantic) {
        Ok(MemberInfo {
            kind: MemberKind::BuiltinMethod(bm),
            ty,
            receiver,
        }) => {
            crate::builtin_methods::infer_builtin_method_call(call, bm, &ty, receiver.as_ref(), ctx)
        }
        Ok(MemberInfo {
            kind: MemberKind::InterfaceCallable { global_sym },
            ..
        }) => infer_interface_callable(call, global_sym, ctx),
        Ok(_) | Err(MemberLookupError::NoMembersOnType) => ExprType::error(
            ExprTypeErrorKind::UnsupportedCalleeForm(CalleeFormKind::MethodCall),
        ),
        Err(MemberLookupError::UnknownMember) => ExprType::error(ExprTypeErrorKind::UnknownMember),
        Err(MemberLookupError::NotInModport) => {
            ExprType::error(ExprTypeErrorKind::MemberNotInModport)
        }
        Err(MemberLookupError::MethodNotValidOnReceiver(reason)) => {
            ExprType::error(ExprTypeErrorKind::MethodNotValidOnReceiver(reason))
        }
    }
}

/// Infer a call to an interface task/function member (LRM 25.5, 25.7).
fn infer_interface_callable(
    call: &CallExpr,
    global_sym: GlobalSymbolId,
    ctx: &dyn InferCtx,
) -> ExprType {
    let Some(sig) = ctx.callable_sig(global_sym) else {
        return ExprType::error(ExprTypeErrorKind::UnresolvedCall);
    };
    if sig.kind == CallableKind::Task {
        // Task calls are statement-level only -- return Void
        check_call_args(call, &sig, ctx);
        return ExprType::from_ty(&Ty::Void);
    }
    check_call_args(call, &sig, ctx);
    ExprType::from_ty(&sig.return_ty)
}

/// Validate and type an iterator pseudo-method call (LRM 7.12.4).
///
/// Accepts 0 or 1 integral arguments (dimension index). The dimension
/// argument is validated as integral but currently not used to select a
/// dimension -- full multi-dimensional semantics require the foreach /
/// multi-dim iterator model which is not yet implemented.
fn infer_iter_method_call(call: &CallExpr, ret_ty: &Ty, ctx: &dyn InferCtx) -> ExprType {
    let arg_list = call.arg_list();
    let mut args = arg_list.iter().flat_map(|al| al.args());
    let first = args.next();
    if args.next().is_some() {
        return ExprType::error(ExprTypeErrorKind::MethodArityMismatch);
    }
    if let Some(arg) = first {
        let arg_type = infer_expr(&arg, ctx, None);
        if let ExprView::Error(_) = &arg_type.view {
            return arg_type;
        }
        if super::try_integral_view(&arg_type, ctx).is_none() {
            return ExprType::error(ExprTypeErrorKind::MethodArgNotIntegral);
        }
    }
    ExprType::from_ty(ret_ty)
}
