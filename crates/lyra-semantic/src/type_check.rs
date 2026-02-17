use lyra_lexer::SyntaxKind;
use lyra_parser::SyntaxNode;
use lyra_source::TextRange;

use crate::type_infer::{BitVecType, BitWidth, ExprType};
use crate::types::SymbolType;

/// A type-check finding for a single assignment.
pub struct TypeCheckItem {
    /// Range of the full assignment statement/item (for primary label).
    pub assign_range: TextRange,
    /// Range of the LHS expression (for secondary label).
    pub lhs_range: TextRange,
    /// Range of the RHS expression (for secondary label).
    pub rhs_range: TextRange,
    /// The check kind.
    pub kind: TypeCheckKind,
}

pub enum TypeCheckKind {
    WidthMismatch { lhs_width: u32, rhs_width: u32 },
}

/// Callbacks for the type checker. No DB access -- pure.
pub trait TypeCheckCtx {
    /// Infer the type of an expression node.
    fn expr_type(&self, node: &SyntaxNode) -> ExprType;
    /// Get the declared type of a symbol via its Declarator node.
    fn symbol_type_of_declarator(&self, declarator: &SyntaxNode) -> Option<SymbolType>;
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

        let Some(lhs_w) = symbol_type_bitvec_width(&sym_type) else {
            continue;
        };

        let rhs_type = ctx.expr_type(&init_expr);
        let Some(rhs_w) = bitvec_known_width(&rhs_type) else {
            continue;
        };

        if lhs_w != rhs_w {
            items.push(TypeCheckItem {
                assign_range: node.text_range(),
                lhs_range: child.text_range(),
                rhs_range: init_expr.text_range(),
                kind: TypeCheckKind::WidthMismatch {
                    lhs_width: lhs_w,
                    rhs_width: rhs_w,
                },
            });
        }
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
    let Some(lhs_w) = simple_lvalue_width(lhs, ctx) else {
        return;
    };
    let rhs_type = ctx.expr_type(rhs);
    let Some(rhs_w) = bitvec_known_width(&rhs_type) else {
        return;
    };
    if lhs_w != rhs_w {
        items.push(TypeCheckItem {
            assign_range: stmt_node.text_range(),
            lhs_range: lhs.text_range(),
            rhs_range: rhs.text_range(),
            kind: TypeCheckKind::WidthMismatch {
                lhs_width: lhs_w,
                rhs_width: rhs_w,
            },
        });
    }
}

fn simple_lvalue_width(lhs: &SyntaxNode, ctx: &dyn TypeCheckCtx) -> Option<u32> {
    match lhs.kind() {
        SyntaxKind::NameRef | SyntaxKind::QualifiedName | SyntaxKind::IndexExpr => {
            bitvec_known_width(&ctx.expr_type(lhs))
        }
        SyntaxKind::Expression | SyntaxKind::ParenExpr => {
            let inner = lhs.children().find(|c| is_expression_kind(c.kind()))?;
            simple_lvalue_width(&inner, ctx)
        }
        _ => None,
    }
}

fn bitvec_known_width(ty: &ExprType) -> Option<u32> {
    match ty {
        ExprType::BitVec(BitVecType {
            width: BitWidth::Known(w),
            ..
        }) => Some(*w),
        _ => None,
    }
}

fn symbol_type_bitvec_width(st: &SymbolType) -> Option<u32> {
    bitvec_known_width(&ExprType::from_symbol_type(st))
}

fn is_expression_kind(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::Expression
            | SyntaxKind::BinExpr
            | SyntaxKind::PrefixExpr
            | SyntaxKind::ParenExpr
            | SyntaxKind::CondExpr
            | SyntaxKind::ConcatExpr
            | SyntaxKind::ReplicExpr
            | SyntaxKind::IndexExpr
            | SyntaxKind::RangeExpr
            | SyntaxKind::FieldExpr
            | SyntaxKind::CallExpr
            | SyntaxKind::NameRef
            | SyntaxKind::Literal
            | SyntaxKind::QualifiedName
    )
}
