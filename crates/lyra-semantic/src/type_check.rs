use crate::Site;
use lyra_ast::{AstNode, ExprKind, SystemTfCall, TfArg};
use lyra_parser::SyntaxNode;
use lyra_source::NameSpan;

use crate::coerce::IntegralCtx;
use crate::enum_def::EnumId;
use crate::modport_def::PortDirection;
use crate::modport_facts::FieldAccessFacts;
use crate::system_functions::{
    BitsArgKind, SystemFnKind, classify_bits_arg, lookup_builtin, tf_arg_node,
};
use crate::type_infer::{BitVecType, BitWidth, ExprType, ExprView};
use crate::types::{SymbolType, Ty};

/// What direction a modport member was accessed in.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AccessKind {
    Read,
    Write,
}

/// Access mode propagated from the index to individual check sites.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum AccessMode {
    Read = 0,
    Write = 1,
    ReadWrite = 2,
}

/// A type-check finding.
pub enum TypeCheckItem {
    AssignTruncation {
        assign_site: Site,
        lhs_site: Site,
        rhs_site: Site,
        lhs_width: u32,
        rhs_width: u32,
    },
    BitsNonDataType {
        call_site: Site,
        arg_site: Site,
    },
    EnumAssignFromNonEnum {
        assign_site: Site,
        lhs_site: Site,
        rhs_site: Site,
        lhs_enum: EnumId,
        rhs_ty: Ty,
    },
    EnumAssignWrongEnum {
        assign_site: Site,
        lhs_site: Site,
        rhs_site: Site,
        lhs_enum: EnumId,
        rhs_enum: EnumId,
    },
    ConversionArgCategory {
        call_site: Site,
        arg_site: Site,
        fn_name: smol_str::SmolStr,
        expected: &'static str,
    },
    ConversionWidthMismatch {
        call_site: Site,
        arg_site: Site,
        fn_name: smol_str::SmolStr,
        expected_width: u32,
        actual_width: u32,
    },
    ModportDirectionViolation {
        member_name_span: NameSpan,
        direction: PortDirection,
        access: AccessKind,
    },
    ModportRefUnsupported {
        member_name_span: NameSpan,
    },
    EnumCastOutOfRange {
        cast_site: Site,
        enum_id: EnumId,
        value: i64,
    },
    StreamWithNonArray {
        with_site: Site,
    },
    ModportEmptyPortAccess {
        member_name_span: NameSpan,
    },
    ModportExprNotAssignable {
        member_name_span: NameSpan,
    },
    MethodCallError {
        call_name_span: NameSpan,
        method_name: smol_str::SmolStr,
        error_kind: crate::type_infer::ExprTypeErrorKind,
    },
    UnsupportedLhsForm {
        lhs_site: Site,
    },
    InvalidLhs {
        lhs_site: Site,
    },
    InternalError {
        detail: smol_str::SmolStr,
        site: Site,
    },
}

/// Callbacks for the type checker. No DB access -- pure.
pub trait TypeCheckCtx {
    /// The file being analyzed.
    fn file_id(&self) -> lyra_source::FileId;
    /// Infer the type of an expression node (self-determined).
    fn expr_type(&self, node: &SyntaxNode) -> ExprType;
    /// Infer the type of an expression node under an integral context.
    fn expr_type_in_ctx(&self, node: &SyntaxNode, ctx: &IntegralCtx) -> ExprType;
    /// Infer the type of an expression in statement context.
    fn expr_type_stmt(&self, node: &SyntaxNode) -> ExprType;
    /// Get the declared type of a symbol via its Declarator node.
    fn symbol_type_of_declarator(&self, declarator: &SyntaxNode) -> Option<SymbolType>;
    /// Resolve a `NameRef` node as a type (typedef/enum/struct name).
    fn resolve_type_arg(&self, name_node: &SyntaxNode) -> Option<crate::types::Ty>;
    /// Pure identity lookup -- returns the stable ID for a syntax node.
    /// O(1) map lookup, no DB calls, no allocations.
    /// Returns None for nodes without stable IDs (e.g. error-recovered trees).
    fn node_id(&self, node: &SyntaxNode) -> Option<Site>;
    /// Evaluate a constant integer expression. Returns None if non-const.
    fn const_eval_int(&self, node: &SyntaxNode) -> Option<i64>;
    /// Get sorted set of known enum member values. None if any value unknown.
    fn enum_known_value_set(&self, id: &EnumId) -> Option<std::sync::Arc<[i64]>>;
    /// Check whether a modport expression target is an lvalue.
    fn is_modport_target_lvalue(&self, expr_id: Site) -> bool;
}

/// Resolve a syntax node to its stable `Site` anchor.
///
/// On failure (error-recovered tree), emits `InternalError` anchored at
/// `fallback` and returns the fallback. Callers must not silently skip
/// findings when `node_id` returns `None`.
fn require_site(
    ctx: &dyn TypeCheckCtx,
    node: &SyntaxNode,
    fallback: Site,
    items: &mut Vec<TypeCheckItem>,
) -> Site {
    if let Some(site) = ctx.node_id(node) {
        return site;
    }
    items.push(TypeCheckItem::InternalError {
        detail: smol_str::SmolStr::new_static("node_id returned None in type checker"),
        site: fallback,
    });
    fallback
}

/// Check a `ContinuousAssign` node for type errors.
///
/// Performs only the local assignment check (truncation, enum compat, LHS
/// validity). Does not recurse into children -- the `ChecksIndex` ensures
/// nested expression sites are checked separately.
pub fn check_continuous_assign(
    node: &SyntaxNode,
    ctx: &dyn TypeCheckCtx,
    fallback: Site,
    items: &mut Vec<TypeCheckItem>,
) {
    let self_site = ctx.node_id(node).unwrap_or(fallback);
    let ca = lyra_ast::ContinuousAssign::cast(node.clone());
    let lhs = ca.as_ref().and_then(|c| c.lhs());
    let rhs = ca.as_ref().and_then(|c| c.rhs());
    if let (Some(l), Some(r)) = (&lhs, &rhs) {
        check_assignment_pair(node, l.syntax(), r.syntax(), ctx, self_site, items);
    }
}

/// Check an `AssignStmt` node for type errors.
///
/// Performs only the local assignment check. Does not recurse.
pub fn check_assign_stmt(
    node: &SyntaxNode,
    ctx: &dyn TypeCheckCtx,
    fallback: Site,
    items: &mut Vec<TypeCheckItem>,
) {
    let self_site = ctx.node_id(node).unwrap_or(fallback);
    let assign = lyra_ast::AssignStmt::cast(node.clone());
    let lhs = assign.as_ref().and_then(|a| a.lhs());
    let rhs = assign.as_ref().and_then(|a| a.rhs());
    let has_op = assign.as_ref().and_then(|a| a.assign_op()).is_some();

    if let (Some(l), Some(r)) = (&lhs, &rhs) {
        check_assignment_pair(node, l.syntax(), r.syntax(), ctx, self_site, items);
    } else if rhs.is_none()
        && !has_op
        && let Some(l) = &lhs
    {
        let _result = ctx.expr_type_stmt(l.syntax());
    }
}

/// Check a `VarDecl` node for initializer type compatibility.
pub fn check_var_decl(
    node: &SyntaxNode,
    ctx: &dyn TypeCheckCtx,
    fallback: Site,
    items: &mut Vec<TypeCheckItem>,
) {
    let decl_site = require_site(ctx, node, fallback, items);
    let Some(var_decl) = lyra_ast::VarDecl::cast(node.clone()) else {
        return;
    };
    for decl in var_decl.declarators() {
        let Some(init_expr) = decl.init_expr() else {
            continue;
        };

        let Some(sym_type) = ctx.symbol_type_of_declarator(decl.syntax()) else {
            continue;
        };

        let lhs_site = require_site(ctx, decl.syntax(), decl_site, items);
        let rhs_site = require_site(ctx, init_expr.syntax(), decl_site, items);

        let lhs_type = ExprType::from_symbol_type(&sym_type);
        let rhs_type = ctx.expr_type(init_expr.syntax());

        check_assignment_compat(&lhs_type, &rhs_type, decl_site, lhs_site, rhs_site, items);
    }
}

fn check_assignment_pair(
    stmt_node: &SyntaxNode,
    lhs: &SyntaxNode,
    rhs: &SyntaxNode,
    ctx: &dyn TypeCheckCtx,
    fallback: Site,
    items: &mut Vec<TypeCheckItem>,
) {
    let stmt_site = require_site(ctx, stmt_node, fallback, items);
    let lhs_site = require_site(ctx, lhs, stmt_site, items);
    let rhs_site = require_site(ctx, rhs, stmt_site, items);

    match crate::lhs::classify_lhs(lhs) {
        crate::lhs::LhsClass::Assignable(lhs_node) => {
            let lhs_type = ctx.expr_type(&lhs_node);
            let rhs_type = ctx.expr_type(rhs);
            check_assignment_compat(&lhs_type, &rhs_type, stmt_site, lhs_site, rhs_site, items);
        }
        crate::lhs::LhsClass::Unsupported => {
            items.push(TypeCheckItem::UnsupportedLhsForm { lhs_site });
        }
        crate::lhs::LhsClass::NotAssignable => {
            items.push(TypeCheckItem::InvalidLhs { lhs_site });
        }
    }
}

fn check_assignment_compat(
    lhs: &ExprType,
    rhs: &ExprType,
    assign_site: Site,
    lhs_site: Site,
    rhs_site: Site,
    items: &mut Vec<TypeCheckItem>,
) {
    if matches!(lhs.ty, Ty::Error)
        || matches!(rhs.ty, Ty::Error)
        || matches!(lhs.view, ExprView::Error(_))
        || matches!(rhs.view, ExprView::Error(_))
    {
        return;
    }

    // Truncation check
    if let (Some(lhs_w), Some(rhs_w)) = (bitvec_known_width(lhs), bitvec_known_width(rhs))
        && !is_context_dependent(rhs)
        && rhs_w > lhs_w
    {
        items.push(TypeCheckItem::AssignTruncation {
            assign_site,
            lhs_site,
            rhs_site,
            lhs_width: lhs_w,
            rhs_width: rhs_w,
        });
    }

    // Enum assignment compatibility check (LRM 6.19.3)
    if let Ty::Enum(ref lhs_id) = lhs.ty {
        match &rhs.ty {
            Ty::Enum(rhs_id) if lhs_id == rhs_id => {}
            Ty::Enum(rhs_id) => {
                items.push(TypeCheckItem::EnumAssignWrongEnum {
                    assign_site,
                    lhs_site,
                    rhs_site,
                    lhs_enum: *lhs_id,
                    rhs_enum: *rhs_id,
                });
            }
            _ => {
                items.push(TypeCheckItem::EnumAssignFromNonEnum {
                    assign_site,
                    lhs_site,
                    rhs_site,
                    lhs_enum: *lhs_id,
                    rhs_ty: rhs.ty.clone(),
                });
            }
        }
    }
}

/// Check a `CastExpr` for enum cast-out-of-range.
pub fn check_cast_expr(
    node: &SyntaxNode,
    ctx: &dyn TypeCheckCtx,
    fallback: Site,
    items: &mut Vec<TypeCheckItem>,
) {
    use lyra_ast::{AstNode, CastExpr};
    let Some(cast) = CastExpr::cast(node.clone()) else {
        return;
    };
    let Some(typespec) = cast.cast_type() else {
        return;
    };
    let Some(utr) = crate::type_extract::user_type_ref(typespec.syntax()) else {
        return;
    };
    let Some(ty) = ctx.resolve_type_arg(utr.resolve_node()) else {
        return;
    };
    let Ty::Enum(ref enum_id) = ty else {
        return;
    };
    let Some(inner) = cast.inner_expr() else {
        return;
    };
    let Some(value) = ctx.const_eval_int(inner.syntax()) else {
        return;
    };
    let Some(value_set) = ctx.enum_known_value_set(enum_id) else {
        return;
    };
    if value_set.binary_search(&value).is_err() {
        let cast_site = require_site(ctx, node, fallback, items);
        items.push(TypeCheckItem::EnumCastOutOfRange {
            cast_site,
            enum_id: *enum_id,
            value,
        });
    }
}

/// Check a `CallExpr` for method-call errors.
pub fn check_method_call(
    node: &SyntaxNode,
    ctx: &dyn TypeCheckCtx,
    items: &mut Vec<TypeCheckItem>,
) {
    use crate::type_infer::ExprTypeErrorKind;

    let Some(call) = lyra_ast::CallExpr::cast(node.clone()) else {
        return;
    };
    let Some(callee) = call.callee() else {
        return;
    };
    let Some(ExprKind::FieldExpr(field_expr)) = callee.classify() else {
        return;
    };
    let Some(field_tok) = field_expr.field_name() else {
        return;
    };

    let result = ctx.expr_type_stmt(node);
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
    node: &SyntaxNode,
    ctx: &dyn TypeCheckCtx,
    facts: &FieldAccessFacts,
    access: AccessMode,
    items: &mut Vec<TypeCheckItem>,
) {
    use crate::modport_facts::FieldAccessTarget;

    let Some(ast_id) = ctx.node_id(node) else {
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

/// Check a `SystemTfCall` for argument type errors.
pub fn check_system_call(
    node: &SyntaxNode,
    ctx: &dyn TypeCheckCtx,
    fallback: Site,
    items: &mut Vec<TypeCheckItem>,
) {
    let Some(stf) = SystemTfCall::cast(node.clone()) else {
        return;
    };
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
    match entry.kind {
        SystemFnKind::Bits => check_bits_call(node, &args, ctx, fallback, items),
        SystemFnKind::IntToReal => {
            check_conversion_arg_integral(node, &args, name, ctx, fallback, items);
        }
        SystemFnKind::RealToInt | SystemFnKind::RealToBits | SystemFnKind::ShortRealToBits => {
            check_conversion_arg_real(node, &args, name, ctx, fallback, items);
        }
        SystemFnKind::BitsToReal => {
            check_conversion_bits_to_real(node, &args, name, 64, ctx, fallback, items);
        }
        SystemFnKind::BitsToShortReal => {
            check_conversion_bits_to_real(node, &args, name, 32, ctx, fallback, items);
        }
        _ => {}
    }
}

fn check_bits_call(
    node: &SyntaxNode,
    args: &[TfArg],
    ctx: &dyn TypeCheckCtx,
    fallback: Site,
    items: &mut Vec<TypeCheckItem>,
) {
    let Some(first) = args.first() else {
        return;
    };
    let kind = classify_bits_arg(first, ctx.file_id(), &|n| ctx.resolve_type_arg(n));
    if let BitsArgKind::Type(ty) = kind
        && !ty.is_data_type()
    {
        let first_node = tf_arg_node(first);
        let call_site = require_site(ctx, node, fallback, items);
        let arg_site = require_site(ctx, first_node, call_site, items);
        items.push(TypeCheckItem::BitsNonDataType {
            call_site,
            arg_site,
        });
    }
}

fn check_conversion_arg_integral(
    node: &SyntaxNode,
    args: &[TfArg],
    fn_name: &str,
    ctx: &dyn TypeCheckCtx,
    fallback: Site,
    items: &mut Vec<TypeCheckItem>,
) {
    let Some(first) = args.first() else {
        return;
    };
    let first_node = tf_arg_node(first);
    let arg_ty = ctx.expr_type(first_node);
    if matches!(arg_ty.view, ExprView::Error(_)) {
        return;
    }
    if !matches!(arg_ty.ty, Ty::Integral(_) | Ty::Enum(_)) {
        let call_site = require_site(ctx, node, fallback, items);
        let arg_site = require_site(ctx, first_node, call_site, items);
        items.push(TypeCheckItem::ConversionArgCategory {
            call_site,
            arg_site,
            fn_name: smol_str::SmolStr::new(fn_name),
            expected: "integral",
        });
    }
}

fn check_conversion_arg_real(
    node: &SyntaxNode,
    args: &[TfArg],
    fn_name: &str,
    ctx: &dyn TypeCheckCtx,
    fallback: Site,
    items: &mut Vec<TypeCheckItem>,
) {
    let Some(first) = args.first() else {
        return;
    };
    let first_node = tf_arg_node(first);
    let arg_ty = ctx.expr_type(first_node);
    if matches!(arg_ty.view, ExprView::Error(_)) {
        return;
    }
    if !arg_ty.ty.is_real() {
        let call_site = require_site(ctx, node, fallback, items);
        let arg_site = require_site(ctx, first_node, call_site, items);
        items.push(TypeCheckItem::ConversionArgCategory {
            call_site,
            arg_site,
            fn_name: smol_str::SmolStr::new(fn_name),
            expected: "real",
        });
    }
}

fn check_conversion_bits_to_real(
    node: &SyntaxNode,
    args: &[TfArg],
    fn_name: &str,
    expected_width: u32,
    ctx: &dyn TypeCheckCtx,
    fallback: Site,
    items: &mut Vec<TypeCheckItem>,
) {
    let Some(first) = args.first() else {
        return;
    };
    let first_node = tf_arg_node(first);
    let arg_ty = ctx.expr_type(first_node);
    if matches!(arg_ty.view, ExprView::Error(_)) {
        return;
    }
    let call_site = require_site(ctx, node, fallback, items);
    let arg_site = require_site(ctx, first_node, call_site, items);
    if !matches!(arg_ty.ty, Ty::Integral(_)) {
        items.push(TypeCheckItem::ConversionArgCategory {
            call_site,
            arg_site,
            fn_name: smol_str::SmolStr::new(fn_name),
            expected: "integral",
        });
        return;
    }
    if let Some(actual_width) = bitvec_known_width(&arg_ty)
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

/// Check a `StreamOperandItem` for non-array `with` clause.
pub fn check_stream_operand(
    node: &SyntaxNode,
    ctx: &dyn TypeCheckCtx,
    fallback: Site,
    items: &mut Vec<TypeCheckItem>,
) {
    use lyra_ast::{AstNode, StreamOperandItem};

    let Some(item) = StreamOperandItem::cast(node.clone()) else {
        return;
    };
    let Some(with_clause) = item.with_clause() else {
        return;
    };
    let Some(expr_node) = item.expr() else {
        return;
    };
    let operand_ty = ctx.expr_type(expr_node.syntax());
    if matches!(operand_ty.view, ExprView::Error(_)) {
        return;
    }
    if !matches!(operand_ty.ty, Ty::Array { .. }) {
        let with_site = require_site(ctx, with_clause.syntax(), fallback, items);
        items.push(TypeCheckItem::StreamWithNonArray { with_site });
    }
}

fn is_context_dependent(ty: &ExprType) -> bool {
    matches!(
        ty.view,
        ExprView::BitVec(BitVecType {
            width: BitWidth::ContextDependent,
            ..
        })
    )
}

fn bitvec_known_width(ty: &ExprType) -> Option<u32> {
    match &ty.view {
        ExprView::BitVec(BitVecType {
            width: BitWidth::Known(w),
            ..
        }) => Some(*w),
        _ => None,
    }
}
