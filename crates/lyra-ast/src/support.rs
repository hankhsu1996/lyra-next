use std::marker::PhantomData;

use lyra_lexer::SyntaxKind;
use lyra_parser::{SyntaxNode, SyntaxToken};
use rowan::SyntaxNodeChildren;

use crate::node::AstNode;
use crate::tokens::{Ident, Keyword, LiteralToken};

/// Iterator over typed child nodes of a given type.
pub struct AstChildren<N: AstNode> {
    inner: SyntaxNodeChildren<lyra_parser::SvLanguage>,
    _ph: PhantomData<N>,
}

impl<N: AstNode> Iterator for AstChildren<N> {
    type Item = N;

    fn next(&mut self) -> Option<N> {
        self.inner.by_ref().find_map(N::cast)
    }
}

/// Get the first child node of type `N`.
pub(crate) fn child<N: AstNode>(parent: &SyntaxNode) -> Option<N> {
    parent.children().find_map(N::cast)
}

/// Iterate over all child nodes of type `N`.
pub(crate) fn children<N: AstNode>(parent: &SyntaxNode) -> AstChildren<N> {
    AstChildren {
        inner: parent.children(),
        _ph: PhantomData,
    }
}

/// Find the first child token with the given kind.
pub(crate) fn token(parent: &SyntaxNode, kind: SyntaxKind) -> Option<SyntaxToken> {
    parent
        .children_with_tokens()
        .filter_map(rowan::NodeOrToken::into_token)
        .find(|tok| tok.kind() == kind)
}

/// Find the first `Ident` token child.
pub(crate) fn ident(parent: &SyntaxNode) -> Option<Ident> {
    token(parent, SyntaxKind::Ident).map(|t| Ident { token: t })
}

/// Find a keyword token from a set of possible keyword kinds.
pub(crate) fn keyword(parent: &SyntaxNode, kinds: &[SyntaxKind]) -> Option<Keyword> {
    parent
        .children_with_tokens()
        .filter_map(rowan::NodeOrToken::into_token)
        .find(|tok| kinds.contains(&tok.kind()))
        .map(|t| Keyword { token: t })
}

/// Find the first literal token child.
pub(crate) fn literal_token(parent: &SyntaxNode) -> Option<LiteralToken> {
    let literal_kinds = [
        SyntaxKind::IntLiteral,
        SyntaxKind::RealLiteral,
        SyntaxKind::BasedLiteral,
        SyntaxKind::UnbasedUnsizedLiteral,
        SyntaxKind::StringLiteral,
    ];
    parent
        .children_with_tokens()
        .filter_map(rowan::NodeOrToken::into_token)
        .find(|tok| literal_kinds.contains(&tok.kind()))
        .map(|t| LiteralToken { token: t })
}
