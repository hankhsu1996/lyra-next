use std::marker::PhantomData;

use lyra_lexer::SyntaxKind;
use lyra_parser::{SyntaxNode, SyntaxToken};
use rowan::SyntaxNodeChildren;

use crate::node::AstNode;

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

/// Get the nth expression-kind child of `parent`.
pub(crate) fn expr_child(parent: &SyntaxNode, n: u8) -> Option<crate::expr::Expr> {
    parent
        .children()
        .filter(|c| crate::node::is_expression_kind(c.kind()))
        .nth(n as usize)
        .and_then(crate::expr::Expr::cast)
}

/// Iterate all expression-kind children of `parent`.
pub fn expr_children(parent: &SyntaxNode) -> impl Iterator<Item = crate::expr::Expr> {
    parent
        .children()
        .filter(|c| crate::node::is_expression_kind(c.kind()))
        .filter_map(crate::expr::Expr::cast)
}

/// Find a token from a set of possible kinds.
pub(crate) fn token_in(parent: &SyntaxNode, kinds: &[SyntaxKind]) -> Option<SyntaxToken> {
    parent
        .children_with_tokens()
        .filter_map(rowan::NodeOrToken::into_token)
        .find(|tok| kinds.contains(&tok.kind()))
}

/// Find the immediately-next non-trivia token after any anchor, if it matches `want`.
///
/// Scans direct token children only (never descends into subtrees).
/// After seeing any anchor token, the very first non-trivia token must be `want`;
/// if that token has a different kind, returns `None` immediately (does not
/// continue scanning for `want` further along).
pub(crate) fn token_after_any(
    parent: &SyntaxNode,
    anchors: &[SyntaxKind],
    want: SyntaxKind,
) -> Option<SyntaxToken> {
    let mut seen_anchor = false;
    for el in parent.children_with_tokens() {
        if let Some(tok) = el.into_token() {
            let k = tok.kind();
            if anchors.contains(&k) {
                seen_anchor = true;
                continue;
            }
            if !seen_anchor || k.is_trivia() {
                continue;
            }
            if k == want {
                return Some(tok);
            }
            return None;
        }
    }
    None
}
