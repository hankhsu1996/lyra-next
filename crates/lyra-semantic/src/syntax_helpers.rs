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

pub(crate) fn first_ident_token(node: &SyntaxNode) -> Option<SyntaxToken> {
    node.children_with_tokens()
        .filter_map(lyra_parser::SyntaxElement::into_token)
        .find(|tok| tok.kind() == SyntaxKind::Ident)
}

pub(crate) fn port_name_ident(port_node: &SyntaxNode) -> Option<SyntaxToken> {
    // In an ANSI port declaration, the port name is the Ident token
    // that is a direct child of the Port node (not inside TypeSpec).
    // If there's a TypeSpec, the port name follows it.
    let mut last_ident = None;
    for el in port_node.children_with_tokens() {
        match el {
            lyra_parser::SyntaxElement::Token(tok) if tok.kind() == SyntaxKind::Ident => {
                last_ident = Some(tok);
            }
            _ => {}
        }
    }
    last_ident
}
