use lyra_parser::SyntaxNode;
use lyra_source::TextRange;

/// Trait for typed AST node wrappers.
///
/// Each wrapper corresponds to a single `SyntaxKind` node variant and provides
/// typed accessors for children and tokens.
pub trait AstNode: Sized {
    /// Returns `true` if the given `SyntaxKind` can be cast to this type.
    fn can_cast(kind: lyra_lexer::SyntaxKind) -> bool;

    /// Try to cast a raw `SyntaxNode` into this typed wrapper.
    fn cast(node: SyntaxNode) -> Option<Self>;

    /// Access the underlying `SyntaxNode`.
    ///
    /// Prefer typed accessors and `text_range()` for routine use.
    /// Direct `SyntaxNode` access is for internal/tooling use only.
    fn syntax(&self) -> &SyntaxNode;

    /// Source range of this node.
    fn text_range(&self) -> TextRange {
        self.syntax().text_range()
    }

    /// Full source text of this node (including trivia).
    fn text(&self) -> String {
        self.syntax().text().to_string()
    }
}

/// Generate a typed AST node wrapper for a single `SyntaxKind`.
macro_rules! ast_node {
    ($name:ident, $kind:expr) => {
        #[derive(Debug, Clone)]
        pub struct $name {
            pub(crate) syntax: lyra_parser::SyntaxNode,
        }

        impl $crate::node::AstNode for $name {
            fn can_cast(kind: lyra_lexer::SyntaxKind) -> bool {
                kind == $kind
            }

            fn cast(node: lyra_parser::SyntaxNode) -> Option<Self> {
                if Self::can_cast(node.kind()) {
                    Some(Self { syntax: node })
                } else {
                    None
                }
            }

            fn syntax(&self) -> &lyra_parser::SyntaxNode {
                &self.syntax
            }
        }
    };
}

pub(crate) use ast_node;
