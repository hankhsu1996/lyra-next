use lyra_lexer::SyntaxKind;
use lyra_parser::SyntaxNode;
use lyra_source::TextRange;

use crate::coerce::IntegralCtx;
use crate::enum_def::EnumId;
use crate::expr_helpers::is_expression_kind;
use crate::syntax_helpers::{system_tf_args, system_tf_name};
use crate::system_functions::{
    BitsArgKind, SystemFnKind, classify_bits_arg, iter_args, lookup_builtin,
};
use crate::type_infer::{BitVecType, BitWidth, ExprType, ExprView};
use crate::types::{SymbolType, Ty};

/// A type-check finding.
pub enum TypeCheckItem {
    AssignTruncation {
        assign_range: TextRange,
        lhs_range: TextRange,
        rhs_range: TextRange,
        lhs_width: u32,
        rhs_width: u32,
    },
    BitsNonDataType {
        call_range: TextRange,
        arg_range: TextRange,
    },
    EnumAssignFromNonEnum {
        assign_range: TextRange,
        lhs_range: TextRange,
        rhs_range: TextRange,
        lhs_enum: EnumId,
        rhs_ty: Ty,
    },
    EnumAssignWrongEnum {
        assign_range: TextRange,
        lhs_range: TextRange,
        rhs_range: TextRange,
        lhs_enum: EnumId,
        rhs_enum: EnumId,
    },
    ConversionArgCategory {
        call_range: TextRange,
        arg_range: TextRange,
        fn_name: smol_str::SmolStr,
        expected: &'static str,
    },
    ConversionWidthMismatch {
        call_range: TextRange,
        arg_range: TextRange,
        fn_name: smol_str::SmolStr,
        expected_width: u32,
        actual_width: u32,
    },
}

/// Callbacks for the type checker. No DB access -- pure.
pub trait TypeCheckCtx {
    /// Infer the type of an expression node (self-determined).
    fn expr_type(&self, node: &SyntaxNode) -> ExprType;
    /// Infer the type of an expression node under an integral context.
    fn expr_type_in_ctx(&self, node: &SyntaxNode, ctx: &IntegralCtx) -> ExprType;
    /// Get the declared type of a symbol via its Declarator node.
    fn symbol_type_of_declarator(&self, declarator: &SyntaxNode) -> Option<SymbolType>;
    /// Resolve a `NameRef` node as a type (typedef/enum/struct name).
    fn resolve_type_arg(&self, name_node: &SyntaxNode) -> Option<crate::types::Ty>;
}

/// Walk a file's AST and produce type-check items.
pub fn check_types(root: &SyntaxNode, ctx: &dyn TypeCheckCtx) -> Vec<TypeCheckItem> {
    let mut items = Vec::new();
    walk_for_checks(root, ctx, &mut items);
    items
}

fn walk_for_checks(node: &SyntaxNode, ctx: &dyn TypeCheckCtx, items: &mut Vec<TypeCheckItem>) {
    match node.kind() {
        SyntaxKind::ContinuousAssign => check_continuous_assign(node, ctx, items),
        SyntaxKind::AssignStmt => check_assign_stmt(node, ctx, items),
        SyntaxKind::VarDecl => check_var_decl(node, ctx, items),
        SyntaxKind::SystemTfCall => check_system_call(node, ctx, items),
        _ => {}
    }
    for child in node.children() {
        walk_for_checks(&child, ctx, items);
    }
}

fn check_continuous_assign(
    node: &SyntaxNode,
    ctx: &dyn TypeCheckCtx,
    items: &mut Vec<TypeCheckItem>,
) {
    let exprs: Vec<SyntaxNode> = node
        .children()
        .filter(|c| is_expression_kind(c.kind()))
        .collect();
    if exprs.len() < 2 {
        return;
    }
    check_assignment_pair(node, &exprs[0], &exprs[1], ctx, items);
}

fn check_assign_stmt(node: &SyntaxNode, ctx: &dyn TypeCheckCtx, items: &mut Vec<TypeCheckItem>) {
    let exprs: Vec<SyntaxNode> = node
        .children()
        .filter(|c| is_expression_kind(c.kind()))
        .collect();
    if exprs.len() < 2 {
        return;
    }
    check_assignment_pair(node, &exprs[0], &exprs[1], ctx, items);
}

fn check_var_decl(node: &SyntaxNode, ctx: &dyn TypeCheckCtx, items: &mut Vec<TypeCheckItem>) {
    for child in node.children() {
        if child.kind() != SyntaxKind::Declarator {
            continue;
        }
        let init_expr = find_declarator_init_expr(&child);
        let Some(init_expr) = init_expr else {
            continue;
        };

        let Some(sym_type) = ctx.symbol_type_of_declarator(&child) else {
            continue;
        };

        let lhs_type = ExprType::from_symbol_type(&sym_type);
        let rhs_type = ctx.expr_type(&init_expr);

        check_assignment_compat(
            &lhs_type,
            &rhs_type,
            node.text_range(),
            child.text_range(),
            init_expr.text_range(),
            items,
        );
    }
}

fn find_declarator_init_expr(declarator: &SyntaxNode) -> Option<SyntaxNode> {
    let mut seen_assign = false;
    for child in declarator.children_with_tokens() {
        if child
            .as_token()
            .is_some_and(|t| t.kind() == SyntaxKind::Assign)
        {
            seen_assign = true;
            continue;
        }
        if seen_assign
            && let Some(node) = child.as_node()
            && is_expression_kind(node.kind())
        {
            return Some(node.clone());
        }
    }
    None
}

fn check_assignment_pair(
    stmt_node: &SyntaxNode,
    lhs: &SyntaxNode,
    rhs: &SyntaxNode,
    ctx: &dyn TypeCheckCtx,
    items: &mut Vec<TypeCheckItem>,
) {
    let Some(simple) = simple_lvalue(lhs) else {
        return;
    };
    let lhs_type = ctx.expr_type(&simple);
    let rhs_type = ctx.expr_type(rhs);

    check_assignment_compat(
        &lhs_type,
        &rhs_type,
        stmt_node.text_range(),
        lhs.text_range(),
        rhs.text_range(),
        items,
    );
}

/// Return the inner node if `lhs` is a simple lvalue (name, qualified name,
/// index expression, or parenthesized simple lvalue). Concat and other
/// compound forms return `None` -- we skip assignment checks for those.
fn simple_lvalue(lhs: &SyntaxNode) -> Option<SyntaxNode> {
    match lhs.kind() {
        SyntaxKind::NameRef | SyntaxKind::QualifiedName | SyntaxKind::IndexExpr => {
            Some(lhs.clone())
        }
        SyntaxKind::Expression | SyntaxKind::ParenExpr => {
            let inner = lhs.children().find(|c| is_expression_kind(c.kind()))?;
            simple_lvalue(&inner)
        }
        _ => None,
    }
}

fn check_assignment_compat(
    lhs: &ExprType,
    rhs: &ExprType,
    assign_range: TextRange,
    lhs_range: TextRange,
    rhs_range: TextRange,
    items: &mut Vec<TypeCheckItem>,
) {
    if matches!(lhs.ty, Ty::Error)
        || matches!(rhs.ty, Ty::Error)
        || matches!(lhs.view, ExprView::Error(_))
        || matches!(rhs.view, ExprView::Error(_))
    {
        return;
    }

    // Truncation check (existing)
    if let (Some(lhs_w), Some(rhs_w)) = (bitvec_known_width(lhs), bitvec_known_width(rhs))
        && !is_context_dependent(rhs)
        && rhs_w > lhs_w
    {
        items.push(TypeCheckItem::AssignTruncation {
            assign_range,
            lhs_range,
            rhs_range,
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
                    assign_range,
                    lhs_range,
                    rhs_range,
                    lhs_enum: lhs_id.clone(),
                    rhs_enum: rhs_id.clone(),
                });
            }
            _ => {
                items.push(TypeCheckItem::EnumAssignFromNonEnum {
                    assign_range,
                    lhs_range,
                    rhs_range,
                    lhs_enum: lhs_id.clone(),
                    rhs_ty: rhs.ty.clone(),
                });
            }
        }
    }
}

fn check_system_call(node: &SyntaxNode, ctx: &dyn TypeCheckCtx, items: &mut Vec<TypeCheckItem>) {
    let Some(tok) = system_tf_name(node) else {
        return;
    };
    let name = tok.text();
    let Some(entry) = lookup_builtin(name) else {
        return;
    };
    let Some(arg_list) = system_tf_args(node) else {
        return;
    };
    match entry.kind {
        SystemFnKind::Bits => check_bits_call(node, &arg_list, ctx, items),
        SystemFnKind::IntToReal => {
            check_conversion_arg_integral(node, &arg_list, name, ctx, items);
        }
        SystemFnKind::RealToInt | SystemFnKind::RealToBits | SystemFnKind::ShortRealToBits => {
            check_conversion_arg_real(node, &arg_list, name, ctx, items);
        }
        SystemFnKind::BitsToReal => {
            check_conversion_bits_to_real(node, &arg_list, name, 64, ctx, items);
        }
        SystemFnKind::BitsToShortReal => {
            check_conversion_bits_to_real(node, &arg_list, name, 32, ctx, items);
        }
        _ => {}
    }
}

fn check_bits_call(
    node: &SyntaxNode,
    arg_list: &SyntaxNode,
    ctx: &dyn TypeCheckCtx,
    items: &mut Vec<TypeCheckItem>,
) {
    let Some(first) = iter_args(arg_list).next() else {
        return;
    };
    let kind = classify_bits_arg(&first, &|n| ctx.resolve_type_arg(n));
    if let BitsArgKind::Type(ty) = kind
        && !ty.is_data_type()
    {
        items.push(TypeCheckItem::BitsNonDataType {
            call_range: node.text_range(),
            arg_range: first.text_range(),
        });
    }
}

fn check_conversion_arg_integral(
    node: &SyntaxNode,
    arg_list: &SyntaxNode,
    fn_name: &str,
    ctx: &dyn TypeCheckCtx,
    items: &mut Vec<TypeCheckItem>,
) {
    let Some(first) = iter_args(arg_list).next() else {
        return;
    };
    let arg_ty = ctx.expr_type(&first);
    if matches!(arg_ty.view, ExprView::Error(_)) {
        return;
    }
    if !matches!(arg_ty.ty, Ty::Integral(_) | Ty::Enum(_)) {
        items.push(TypeCheckItem::ConversionArgCategory {
            call_range: node.text_range(),
            arg_range: first.text_range(),
            fn_name: smol_str::SmolStr::new(fn_name),
            expected: "integral",
        });
    }
}

fn check_conversion_arg_real(
    node: &SyntaxNode,
    arg_list: &SyntaxNode,
    fn_name: &str,
    ctx: &dyn TypeCheckCtx,
    items: &mut Vec<TypeCheckItem>,
) {
    let Some(first) = iter_args(arg_list).next() else {
        return;
    };
    let arg_ty = ctx.expr_type(&first);
    if matches!(arg_ty.view, ExprView::Error(_)) {
        return;
    }
    if !arg_ty.ty.is_real() {
        items.push(TypeCheckItem::ConversionArgCategory {
            call_range: node.text_range(),
            arg_range: first.text_range(),
            fn_name: smol_str::SmolStr::new(fn_name),
            expected: "real",
        });
    }
}

fn check_conversion_bits_to_real(
    node: &SyntaxNode,
    arg_list: &SyntaxNode,
    fn_name: &str,
    expected_width: u32,
    ctx: &dyn TypeCheckCtx,
    items: &mut Vec<TypeCheckItem>,
) {
    let Some(first) = iter_args(arg_list).next() else {
        return;
    };
    let arg_ty = ctx.expr_type(&first);
    if matches!(arg_ty.view, ExprView::Error(_)) {
        return;
    }
    if !matches!(arg_ty.ty, Ty::Integral(_)) {
        items.push(TypeCheckItem::ConversionArgCategory {
            call_range: node.text_range(),
            arg_range: first.text_range(),
            fn_name: smol_str::SmolStr::new(fn_name),
            expected: "integral",
        });
        return;
    }
    if let Some(actual_width) = bitvec_known_width(&arg_ty)
        && actual_width != expected_width
    {
        items.push(TypeCheckItem::ConversionWidthMismatch {
            call_range: node.text_range(),
            arg_range: first.text_range(),
            fn_name: smol_str::SmolStr::new(fn_name),
            expected_width,
            actual_width,
        });
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
