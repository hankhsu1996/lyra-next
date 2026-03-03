use crate::Site;
use crate::modport_def::PortDirection;
use crate::modport_facts::FieldAccessFacts;
use crate::site;
use crate::type_check::{AccessKind, AccessMode, TypeCheckCtx, TypeCheckItem, require_site};
use crate::type_infer::ExprView;
use crate::types::Ty;
use lyra_ast::{CallExpr, CastExpr, Expr, ExprKind, StreamOperandItem};
use lyra_source::NameSpan;

/// Check a `CastExpr` for enum cast-out-of-range.
pub fn check_cast_expr(
    cast: &CastExpr,
    ctx: &dyn TypeCheckCtx,
    fallback: Site,
    items: &mut Vec<TypeCheckItem>,
) {
    let Some(typespec) = cast.cast_type() else {
        return;
    };
    let Some(utr) = crate::type_extract::user_type_ref(&typespec) else {
        return;
    };
    let Some(ty) = ctx.resolve_type_arg(&utr) else {
        return;
    };
    let Ty::Enum(ref enum_id) = ty else {
        return;
    };
    let Some(inner) = cast.inner_expr() else {
        return;
    };
    let Some(value) = ctx.const_eval_int(&inner) else {
        return;
    };
    let Some(value_set) = ctx.enum_known_value_set(enum_id) else {
        return;
    };
    if value_set.binary_search(&value).is_err() {
        let map = ctx.ast_id_map();
        let cast_site = require_site(site::opt_site_of(map, cast), fallback, items);
        items.push(TypeCheckItem::EnumCastOutOfRange {
            cast_site,
            enum_id: *enum_id,
            value,
        });
    }
}

/// Check a `CallExpr` for method-call errors.
pub fn check_method_call(call: &CallExpr, ctx: &dyn TypeCheckCtx, items: &mut Vec<TypeCheckItem>) {
    use crate::type_infer::ExprTypeErrorKind;

    let Some(callee) = call.callee() else {
        return;
    };
    let Some(ExprKind::FieldExpr(field_expr)) = callee.classify() else {
        return;
    };
    let Some(field_tok) = field_expr.field_name() else {
        return;
    };

    let Some(expr) = Expr::from_ast(call) else {
        return;
    };
    let result = ctx.expr_type_stmt(&expr);
    let ExprView::Error(error_kind) = &result.view else {
        return;
    };
    match error_kind {
        ExprTypeErrorKind::UnknownMember
        | ExprTypeErrorKind::NoMembersOnReceiver
        | ExprTypeErrorKind::MethodArityMismatch
        | ExprTypeErrorKind::MethodArgTypeMismatch
        | ExprTypeErrorKind::MethodArgNotIntegral
        | ExprTypeErrorKind::MethodNotValidOnReceiver(_) => {
            let call_name_span = NameSpan::new(field_tok.text_range());
            items.push(TypeCheckItem::MethodCallError {
                call_name_span,
                method_name: smol_str::SmolStr::new(field_tok.text()),
                error_kind: *error_kind,
            });
        }
        _ => {}
    }
}

/// Check a `FieldExpr` for modport direction violations.
pub fn check_field_direction(
    field: &lyra_ast::FieldExpr,
    ctx: &dyn TypeCheckCtx,
    facts: &FieldAccessFacts,
    access: AccessMode,
    items: &mut Vec<TypeCheckItem>,
) {
    use crate::modport_facts::FieldAccessTarget;

    let map = ctx.ast_id_map();
    let Some(ast_id) = site::opt_site_of(map, field) else {
        return;
    };
    let Some(fact) = facts.get(&ast_id) else {
        return;
    };
    if fact.direction == PortDirection::Ref {
        items.push(TypeCheckItem::ModportRefUnsupported {
            member_name_span: fact.member_name_span,
        });
        return;
    }

    // Direction check
    match access {
        AccessMode::Read => {
            if !direction_permits(fact.direction, AccessKind::Read) {
                items.push(TypeCheckItem::ModportDirectionViolation {
                    member_name_span: fact.member_name_span,
                    direction: fact.direction,
                    access: AccessKind::Read,
                });
            }
        }
        AccessMode::Write => {
            if !direction_permits(fact.direction, AccessKind::Write) {
                items.push(TypeCheckItem::ModportDirectionViolation {
                    member_name_span: fact.member_name_span,
                    direction: fact.direction,
                    access: AccessKind::Write,
                });
            }
        }
        AccessMode::ReadWrite => {
            if !direction_permits(fact.direction, AccessKind::Read) {
                items.push(TypeCheckItem::ModportDirectionViolation {
                    member_name_span: fact.member_name_span,
                    direction: fact.direction,
                    access: AccessKind::Read,
                });
            }
            if !direction_permits(fact.direction, AccessKind::Write) {
                items.push(TypeCheckItem::ModportDirectionViolation {
                    member_name_span: fact.member_name_span,
                    direction: fact.direction,
                    access: AccessKind::Write,
                });
            }
        }
    }

    // Target legality check
    match &fact.target {
        FieldAccessTarget::Empty => {
            items.push(TypeCheckItem::ModportEmptyPortAccess {
                member_name_span: fact.member_name_span,
            });
        }
        FieldAccessTarget::Expr(expr_id) => {
            let is_write = matches!(access, AccessMode::Write | AccessMode::ReadWrite);
            if is_write && !ctx.is_modport_target_lvalue(*expr_id) {
                items.push(TypeCheckItem::ModportExprNotAssignable {
                    member_name_span: fact.member_name_span,
                });
            }
        }
        FieldAccessTarget::Member => {}
    }
}

/// Check whether a modport direction permits the given access kind.
/// Input = read-only, Output = write-only, Inout = both.
fn direction_permits(dir: PortDirection, access: AccessKind) -> bool {
    !matches!(
        (dir, access),
        (PortDirection::Input, AccessKind::Write) | (PortDirection::Output, AccessKind::Read)
    )
}

/// Check a `StreamOperandItem` for non-array `with` clause.
///
/// When `access` is `Write` or `ReadWrite`, the non-array `with` check is
/// skipped because `check_streaming_unpack` handles it for LHS streaming
/// targets with its own anchor.
pub fn check_stream_operand(
    item: &StreamOperandItem,
    ctx: &dyn TypeCheckCtx,
    fallback: Site,
    access: AccessMode,
    items: &mut Vec<TypeCheckItem>,
) {
    if matches!(access, AccessMode::Write | AccessMode::ReadWrite) {
        return;
    }
    let Some(with_clause) = item.with_clause() else {
        return;
    };
    let Some(expr_node) = item.expr() else {
        return;
    };
    let operand_ty = ctx.expr_type(&expr_node);
    if matches!(operand_ty.view, ExprView::Error(_)) {
        return;
    }
    if !matches!(operand_ty.ty, Ty::Array { .. }) {
        let map = ctx.ast_id_map();
        let with_site = require_site(site::opt_site_of(map, &with_clause), fallback, items);
        items.push(TypeCheckItem::StreamWithNonArray { with_site });
    }
}
