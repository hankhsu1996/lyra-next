// Typed accessors for pattern syntax nodes (LRM 12.6).

use lyra_lexer::SyntaxKind;
use lyra_parser::SyntaxToken;

use crate::node::AstNode;
use crate::nodes::{
    BindPattern, ConstantPattern, ParenPattern, PatternField, StructPattern, TaggedPattern,
    WildcardPattern,
};
use crate::support;

/// Lightweight sum type over all pattern node forms.
///
/// Classification over dedicated syntax kinds, not reconstruction
/// from a generic wrapper.
#[derive(Debug, Clone)]
pub enum PatternNode {
    Wildcard(WildcardPattern),
    Bind(BindPattern),
    Paren(ParenPattern),
    Tagged(TaggedPattern),
    Struct(StructPattern),
    Constant(ConstantPattern),
}

impl PatternNode {
    /// Try to cast a raw `SyntaxNode` into a `PatternNode`.
    pub fn cast(node: lyra_parser::SyntaxNode) -> Option<Self> {
        match node.kind() {
            SyntaxKind::WildcardPattern => WildcardPattern::cast(node).map(Self::Wildcard),
            SyntaxKind::BindPattern => BindPattern::cast(node).map(Self::Bind),
            SyntaxKind::ParenPattern => ParenPattern::cast(node).map(Self::Paren),
            SyntaxKind::TaggedPattern => TaggedPattern::cast(node).map(Self::Tagged),
            SyntaxKind::StructPattern => StructPattern::cast(node).map(Self::Struct),
            SyntaxKind::ConstantPattern => ConstantPattern::cast(node).map(Self::Constant),
            _ => None,
        }
    }

    /// Whether the given kind is a pattern node.
    pub fn is_pattern_kind(kind: SyntaxKind) -> bool {
        matches!(
            kind,
            SyntaxKind::WildcardPattern
                | SyntaxKind::BindPattern
                | SyntaxKind::ParenPattern
                | SyntaxKind::TaggedPattern
                | SyntaxKind::StructPattern
                | SyntaxKind::ConstantPattern
        )
    }
}

impl BindPattern {
    /// The variable identifier token after `.`.
    pub fn name_token(&self) -> Option<SyntaxToken> {
        support::token_in(&self.syntax, &[SyntaxKind::Ident, SyntaxKind::EscapedIdent])
    }
}

impl ParenPattern {
    /// The inner pattern.
    pub fn pattern(&self) -> Option<PatternNode> {
        self.syntax.children().find_map(PatternNode::cast)
    }
}

impl TaggedPattern {
    /// The member identifier token.
    pub fn member_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, SyntaxKind::Ident)
    }

    /// The optional inner pattern after the member name.
    pub fn inner_pattern(&self) -> Option<PatternNode> {
        self.syntax.children().find_map(PatternNode::cast)
    }
}

impl StructPattern {
    /// Iterate over pattern fields.
    pub fn fields(&self) -> support::AstChildren<PatternField> {
        support::children(&self.syntax)
    }
}

impl PatternField {
    /// The field name token for named fields (`name: pattern`).
    pub fn name_token(&self) -> Option<SyntaxToken> {
        // Named field: Ident Colon Pattern
        // Only return the ident if followed by colon
        let mut it = self.syntax.children_with_tokens();
        while let Some(el) = it.next() {
            if let rowan::NodeOrToken::Token(tok) = &el
                && matches!(tok.kind(), SyntaxKind::Ident | SyntaxKind::EscapedIdent)
            {
                // Check if next non-trivia is Colon
                for next in it.by_ref() {
                    if let rowan::NodeOrToken::Token(t) = &next {
                        if t.kind().is_trivia() {
                            continue;
                        }
                        if t.kind() == SyntaxKind::Colon {
                            return Some(tok.clone());
                        }
                        return None;
                    }
                    return None;
                }
            }
        }
        None
    }

    /// Whether this is a `default :` field.
    pub fn is_default(&self) -> bool {
        support::token(&self.syntax, SyntaxKind::DefaultKw).is_some()
    }

    /// The pattern child of this field.
    pub fn pattern(&self) -> Option<PatternNode> {
        self.syntax.children().find_map(PatternNode::cast)
    }
}

impl ConstantPattern {
    /// The expression child of this constant pattern.
    pub fn expr(&self) -> Option<crate::expr::Expr> {
        support::expr_children(&self.syntax).next()
    }
}
