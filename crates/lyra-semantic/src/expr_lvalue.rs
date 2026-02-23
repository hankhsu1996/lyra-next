use lyra_ast::is_expression_kind;
use lyra_ast::{AstNode, Expression, FieldExpr, IndexExpr, ParenExpr};
use lyra_lexer::SyntaxKind;
use lyra_parser::SyntaxNode;

/// Syntactic lvalue check: returns true if the node represents a valid
/// left-hand-side target (name, field access, index, part-select, or
/// parenthesized lvalue).
pub fn is_lvalue(node: &SyntaxNode) -> bool {
    match node.kind() {
        SyntaxKind::NameRef | SyntaxKind::QualifiedName => true,
        SyntaxKind::FieldExpr => FieldExpr::cast(node.clone())
            .and_then(|f| f.base_expr())
            .is_some_and(|base| is_lvalue(&base)),
        SyntaxKind::IndexExpr => IndexExpr::cast(node.clone())
            .and_then(|i| i.base_expr())
            .is_some_and(|base| is_lvalue(&base)),
        SyntaxKind::RangeExpr => node
            .children()
            .find(|c| is_expression_kind(c.kind()))
            .is_some_and(|base| is_lvalue(&base)),
        SyntaxKind::ParenExpr => ParenExpr::cast(node.clone())
            .and_then(|p| p.syntax().first_child())
            .is_some_and(|inner| is_lvalue(&inner)),
        SyntaxKind::Expression => Expression::cast(node.clone())
            .and_then(|e| e.syntax().first_child())
            .is_some_and(|inner| is_lvalue(&inner)),
        _ => false,
    }
}
