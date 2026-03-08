use std::sync::Arc;

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

/// Per-argument contextual checking result for a call expression.
///
/// One entry per supplied positional argument in source order.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallArgCheck {
    pub expected_ty: Option<Ty>,
    pub inferred: ExprType,
}

/// Infer a single call argument against its port type.
///
/// Passes both the full port type (for context-determined expressions
/// like tagged unions and `new[]`) and integral context (for width/
/// signedness propagation per LRM 11.6).
fn infer_call_arg(arg: &Expr, port: Option<&super::CallablePort>, ctx: &dyn InferCtx) -> ExprType {
    if let Some(port) = port {
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
        infer_expr_with_expected(arg, Some(&port.ty), ctx, integral_ctx.as_ref())
    } else {
        infer_expr(arg, ctx, None)
    }
}

/// Check call arguments against the callable signature (non-allocating).
fn check_call_args(call: &CallExpr, sig: &CallableSigRef, ctx: &dyn InferCtx) {
    let args: Vec<Expr> = call
        .arg_list()
        .map(|al| al.args().collect())
        .unwrap_or_default();
    for (i, arg) in args.iter().enumerate() {
        infer_call_arg(arg, sig.ports.get(i), ctx);
    }
}

/// Collect per-argument contextual checking results for a call with a known signature.
fn collect_call_arg_checks(
    call: &CallExpr,
    sig: &CallableSigRef,
    ctx: &dyn InferCtx,
) -> Arc<[CallArgCheck]> {
    let args: Vec<Expr> = call
        .arg_list()
        .map(|al| al.args().collect())
        .unwrap_or_default();
    let mut checks = Vec::with_capacity(args.len());
    for (i, arg) in args.iter().enumerate() {
        let port = sig.ports.get(i);
        let inferred = infer_call_arg(arg, port, ctx);
        checks.push(CallArgCheck {
            expected_ty: port.map(|p| p.ty.clone()),
            inferred,
        });
    }
    checks.into()
}

/// Compute per-argument contextual checking results for a call expression.
///
/// Resolves the callee, obtains its signature, and infers each supplied
/// argument with the appropriate expected type context. Returns one
/// `CallArgCheck` per supplied positional argument. Handles both
/// direct calls and interface callable method calls.
pub fn infer_call_arg_checks(call: &CallExpr, ctx: &dyn InferCtx) -> Arc<[CallArgCheck]> {
    let Some(callee_expr) = call.callee() else {
        return Arc::from([]);
    };
    let Some(callee_kind) = callee_expr.classify() else {
        return Arc::from([]);
    };

    match callee_kind {
        ExprKind::NameRef(_) | ExprKind::QualifiedName(_) => {}
        ExprKind::FieldExpr(field_expr) => {
            let Some(sig) = resolve_method_callable_sig(&field_expr, ctx) else {
                return Arc::from([]);
            };
            return collect_call_arg_checks(call, &sig, ctx);
        }
        _ => return Arc::from([]),
    }

    let Ok(sym_id) = ctx.resolve_callable(&callee_expr) else {
        return Arc::from([]);
    };
    let Some(sig) = ctx.callable_sig(sym_id) else {
        return Arc::from([]);
    };

    collect_call_arg_checks(call, &sig, ctx)
}

/// Result of resolving a method-call callee's member.
enum MethodCallee {
    /// Iterator pseudo-method with known return type (LRM 7.12.4).
    IteratorMethod(Ty),
    /// Successfully resolved member or lookup error.
    Member(Result<MemberInfo, MemberLookupError>),
    /// LHS type inference produced an error.
    LhsError(ExprType),
    /// Field expression is missing required syntactic parts.
    MissingParts,
}

/// Shared method-call resolution: extract field parts, check iterator
/// methods, infer LHS type, and perform member lookup.
///
/// Both `infer_method_call` (full inference) and
/// `resolve_method_callable_sig` (signature-only query) use this to
/// avoid duplicating the resolution logic.
fn resolve_method_callee(field_expr: &FieldExpr, ctx: &dyn InferCtx) -> MethodCallee {
    let Some((_kind, field_semantic)) = field_expr.member_lookup_name() else {
        return MethodCallee::MissingParts;
    };
    let Some(lhs_node) = field_expr.base_expr() else {
        return MethodCallee::MissingParts;
    };
    if let Some(ret_ty) = ctx.iter_method_return(&lhs_node, &field_semantic) {
        return MethodCallee::IteratorMethod(ret_ty);
    }
    let lhs_type = infer_expr(&lhs_node, ctx, None);
    if matches!(lhs_type.view, ExprView::Error(_)) {
        return MethodCallee::LhsError(lhs_type);
    }
    MethodCallee::Member(ctx.member_lookup(&lhs_type.ty, &field_semantic))
}

/// Resolve a method-call callee to a callable signature, if possible.
///
/// Returns `Some` for interface callable methods (LRM 25.5, 25.7) that
/// have a standard callable signature. Returns `None` for builtin methods
/// (which have custom argument checking) and error cases.
fn resolve_method_callable_sig(
    field_expr: &FieldExpr,
    ctx: &dyn InferCtx,
) -> Option<CallableSigRef> {
    match resolve_method_callee(field_expr, ctx) {
        MethodCallee::Member(Ok(MemberInfo {
            kind: MemberKind::InterfaceCallable { global_sym },
            ..
        })) => ctx.callable_sig(global_sym),
        _ => None,
    }
}

fn infer_method_call(call: &CallExpr, field_expr: &FieldExpr, ctx: &dyn InferCtx) -> ExprType {
    match resolve_method_callee(field_expr, ctx) {
        MethodCallee::MissingParts => ExprType::error(ExprTypeErrorKind::UnsupportedExprKind),
        MethodCallee::IteratorMethod(ret_ty) => infer_iter_method_call(call, &ret_ty, ctx),
        MethodCallee::LhsError(e) => e,
        MethodCallee::Member(Ok(MemberInfo {
            kind: MemberKind::BuiltinMethod(bm),
            ty,
            receiver,
        })) => {
            crate::builtin_methods::infer_builtin_method_call(call, bm, &ty, receiver.as_ref(), ctx)
        }
        MethodCallee::Member(Ok(MemberInfo {
            kind: MemberKind::InterfaceCallable { global_sym },
            ..
        })) => infer_interface_callable(call, global_sym, ctx),
        MethodCallee::Member(Ok(_) | Err(MemberLookupError::NoMembersOnType)) => ExprType::error(
            ExprTypeErrorKind::UnsupportedCalleeForm(CalleeFormKind::MethodCall),
        ),
        MethodCallee::Member(Err(MemberLookupError::UnknownMember)) => {
            ExprType::error(ExprTypeErrorKind::UnknownMember)
        }
        MethodCallee::Member(Err(MemberLookupError::NotInModport)) => {
            ExprType::error(ExprTypeErrorKind::MemberNotInModport)
        }
        MethodCallee::Member(Err(MemberLookupError::MethodNotValidOnReceiver(reason))) => {
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
