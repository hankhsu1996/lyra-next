use lyra_lexer::SyntaxKind;
use lyra_parser::{SyntaxNode, SyntaxToken};

/// Extract the system function name token from either a `SystemTfCall` or
/// a `CallExpr` node that wraps a `SystemIdent` inside a `NameRef`.
pub(crate) fn system_tf_name(node: &SyntaxNode) -> Option<SyntaxToken> {
    match node.kind() {
        SyntaxKind::SystemTfCall => node
            .children_with_tokens()
            .filter_map(|el| el.into_token())
            .find(|tok| tok.kind() == SyntaxKind::SystemIdent),
        SyntaxKind::CallExpr => {
            let name_ref = node.children().find(|c| c.kind() == SyntaxKind::NameRef)?;
            name_ref
                .children_with_tokens()
                .filter_map(|el| el.into_token())
                .find(|tok| tok.kind() == SyntaxKind::SystemIdent)
        }
        _ => None,
    }
}

/// Find the argument list node from either a `SystemTfCall` or `CallExpr`.
pub(crate) fn system_tf_args(node: &SyntaxNode) -> Option<SyntaxNode> {
    match node.kind() {
        SyntaxKind::SystemTfCall => node
            .children()
            .find(|c| c.kind() == SyntaxKind::SystemTfArgList),
        SyntaxKind::CallExpr => node.children().find(|c| c.kind() == SyntaxKind::ArgList),
        _ => None,
    }
}
