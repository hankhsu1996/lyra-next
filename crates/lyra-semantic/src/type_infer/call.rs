use lyra_ast::{AstNode, CallExpr, Expr, FieldExpr, SystemTfCall};
use lyra_lexer::SyntaxKind;
use lyra_parser::SyntaxNode;

use super::expr_type::{
    CallableKind, CallableSigRef, CalleeFormKind, ExprType, ExprTypeErrorKind, ExprView, InferCtx,
    ResolveCallableError,
};
use super::infer_expr_type;
use crate::coerce::IntegralCtx;
use crate::member::{MemberInfo, MemberKind, MemberLookupError};

pub(super) fn infer_call(node: &SyntaxNode, ctx: &dyn InferCtx) -> ExprType {
    // System task/function calls: $clog2, $signed, $bits, etc.
    if SystemTfCall::cast(node.clone())
        .and_then(|s| s.system_name())
        .is_some()
    {
        return crate::system_functions::infer_system_call(node, ctx);
    }

    // User-defined call: classify callee form
    let Some(call) = CallExpr::cast(node.clone()) else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let Some(callee_expr) = call.callee() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let callee_node = callee_expr.syntax().clone();

    match callee_node.kind() {
        SyntaxKind::NameRef | SyntaxKind::QualifiedName => {}
        SyntaxKind::FieldExpr => {
            return infer_method_call(node, &callee_node, ctx);
        }
        _ => {
            return ExprType::error(ExprTypeErrorKind::UnsupportedCalleeForm(
                CalleeFormKind::Other,
            ));
        }
    }

    // Resolve callee to a callable symbol
    let sym_id = match ctx.resolve_callable(&callee_node) {
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

    check_call_args(node, &sig, ctx);
    ExprType::from_ty(&sig.return_ty)
}

/// Check call arguments against the callable signature.
fn check_call_args(call_node: &SyntaxNode, sig: &CallableSigRef, ctx: &dyn InferCtx) {
    let Some(call) = CallExpr::cast(call_node.clone()) else {
        return;
    };
    let args: Vec<Expr> = call
        .arg_list()
        .map(|al| al.args().collect())
        .unwrap_or_default();

    // Infer each argument with expected type from the port signature
    for (i, arg) in args.iter().enumerate() {
        let expected_ctx = sig.ports.get(i).and_then(|p| {
            let ety = ExprType::from_ty(&p.ty);
            match ety.view {
                ExprView::BitVec(bv) => Some(IntegralCtx {
                    width: bv.width.self_determined(),
                    signed: bv.signed,
                    four_state: bv.four_state,
                }),
                _ => None,
            }
        });
        infer_expr_type(arg.syntax(), ctx, expected_ctx.as_ref());
    }
}

fn infer_method_call(
    call_node: &SyntaxNode,
    callee_node: &SyntaxNode,
    ctx: &dyn InferCtx,
) -> ExprType {
    let Some(field_expr) = FieldExpr::cast(callee_node.clone()) else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let Some(field_tok) = field_expr.field_name() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let Some(lhs_node) = field_expr.base_expr() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };

    let lhs_type = infer_expr_type(lhs_node.syntax(), ctx, None);
    if let ExprView::Error(_) = &lhs_type.view {
        return lhs_type;
    }

    match ctx.member_lookup(&lhs_type.ty, field_tok.text()) {
        Ok(MemberInfo {
            kind: MemberKind::BuiltinMethod(bm),
            ty,
            receiver,
        }) => crate::builtin_methods::infer_builtin_method_call(
            call_node,
            bm,
            &ty,
            receiver.as_ref(),
            ctx,
        ),
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
