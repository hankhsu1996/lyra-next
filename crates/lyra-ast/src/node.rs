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

/// Whether a `SyntaxKind` represents an expression node.
pub fn is_expression_kind(kind: lyra_lexer::SyntaxKind) -> bool {
    use lyra_lexer::SyntaxKind;
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
            | SyntaxKind::SystemTfCall
            | SyntaxKind::NameRef
            | SyntaxKind::Literal
            | SyntaxKind::QualifiedName
            | SyntaxKind::StreamExpr
            | SyntaxKind::CastExpr
    )
}

/// Declare typed AST node wrappers with optional generated accessors.
///
/// Each entry declares a struct, its `AstNode` impl, and (optionally)
/// typed accessor methods for child nodes and tokens.
///
/// # Field forms
///
/// | Syntax | Method signature | Implementation |
/// |--------|-----------------|----------------|
/// | `name: token(Kind)` | `fn name() -> Option<SyntaxToken>` | `support::token` |
/// | `name: token([A, B])` | `fn name() -> Option<SyntaxToken>` | `support::token_in` |
/// | `name: ChildType` | `fn name() -> Option<ChildType>` | `support::child` |
/// | `name: [ChildType]` | `fn name() -> AstChildren<ChildType>` | `support::children` |
///
/// Use `as method_name` after any field to rename the generated method.
/// Use `{ @custom }` to suppress accessor generation (hand-write them below).
/// Use `{}` for nodes with no accessors.
macro_rules! ast_nodes {
    // Entry: iterate over node definitions
    ($(
        $name:ident($kind:path) { $($body:tt)* }
    )*) => {
        $(
            ast_nodes!(@node $name, $kind);
            ast_nodes!(@accessors $name { $($body)* });
        )*
    };

    // Struct + AstNode impl
    (@node $name:ident, $kind:path) => {
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

    // @custom: no generated impl block
    (@accessors $name:ident { @custom }) => {};

    // Empty: empty impl block
    (@accessors $name:ident {}) => {
        impl $name {}
    };

    // Non-empty fields: generate impl block with accessors
    (@accessors $name:ident { $($rest:tt)+ }) => {
        impl $name {
            ast_nodes!(@fields $($rest)+);
        }
    };

    // Field iteration: peel off one field at a time.
    //
    // All token(...) arms must come before $ty:ty arms because $ty:ty
    // is a greedy fragment matcher that causes hard parse errors when
    // it partially consumes `token([...])` inputs.

    // token(Kind) as rename, more fields follow
    (@fields $fname:ident : token($kind:ident) as $method:ident, $($rest:tt)+) => {
        ast_nodes!(@field_token $method, $kind);
        ast_nodes!(@fields $($rest)+);
    };
    // token(Kind) as rename, terminal
    (@fields $fname:ident : token($kind:ident) as $method:ident $(,)?) => {
        ast_nodes!(@field_token $method, $kind);
    };
    // token(Kind), more fields follow
    (@fields $fname:ident : token($kind:ident), $($rest:tt)+) => {
        ast_nodes!(@field_token $fname, $kind);
        ast_nodes!(@fields $($rest)+);
    };
    // token(Kind), terminal
    (@fields $fname:ident : token($kind:ident) $(,)?) => {
        ast_nodes!(@field_token $fname, $kind);
    };
    // token([K1, K2, ...]) as rename, more fields follow
    (@fields $fname:ident : token([$($kind:ident),+ $(,)?]) as $method:ident, $($rest:tt)+) => {
        ast_nodes!(@field_token_in $method, $($kind),+);
        ast_nodes!(@fields $($rest)+);
    };
    // token([K1, K2, ...]) as rename, terminal
    (@fields $fname:ident : token([$($kind:ident),+ $(,)?]) as $method:ident $(,)?) => {
        ast_nodes!(@field_token_in $method, $($kind),+);
    };
    // token([K1, K2, ...]), more fields follow
    (@fields $fname:ident : token([$($kind:ident),+ $(,)?]), $($rest:tt)+) => {
        ast_nodes!(@field_token_in $fname, $($kind),+);
        ast_nodes!(@fields $($rest)+);
    };
    // token([K1, K2, ...]), terminal
    (@fields $fname:ident : token([$($kind:ident),+ $(,)?]) $(,)?) => {
        ast_nodes!(@field_token_in $fname, $($kind),+);
    };

    // [Type] as rename, more fields follow
    (@fields $fname:ident : [$ty:ty] as $method:ident, $($rest:tt)+) => {
        ast_nodes!(@field_children $method, $ty);
        ast_nodes!(@fields $($rest)+);
    };
    // [Type] as rename, terminal
    (@fields $fname:ident : [$ty:ty] as $method:ident $(,)?) => {
        ast_nodes!(@field_children $method, $ty);
    };
    // [Type], more fields follow
    (@fields $fname:ident : [$ty:ty], $($rest:tt)+) => {
        ast_nodes!(@field_children $fname, $ty);
        ast_nodes!(@fields $($rest)+);
    };
    // [Type], terminal
    (@fields $fname:ident : [$ty:ty] $(,)?) => {
        ast_nodes!(@field_children $fname, $ty);
    };

    // Type as rename, more fields follow
    (@fields $fname:ident : $ty:ty as $method:ident, $($rest:tt)+) => {
        ast_nodes!(@field_child $method, $ty);
        ast_nodes!(@fields $($rest)+);
    };
    // Type as rename, terminal
    (@fields $fname:ident : $ty:ty as $method:ident $(,)?) => {
        ast_nodes!(@field_child $method, $ty);
    };
    // Type, more fields follow
    (@fields $fname:ident : $ty:ty, $($rest:tt)+) => {
        ast_nodes!(@field_child $fname, $ty);
        ast_nodes!(@fields $($rest)+);
    };
    // Type, terminal
    (@fields $fname:ident : $ty:ty $(,)?) => {
        ast_nodes!(@field_child $fname, $ty);
    };

    // Leaf generators

    (@field_token $method:ident, $kind:ident) => {
        pub fn $method(&self) -> Option<lyra_parser::SyntaxToken> {
            $crate::support::token(&self.syntax, lyra_lexer::SyntaxKind::$kind)
        }
    };

    (@field_token_in $method:ident, $($kind:ident),+) => {
        pub fn $method(&self) -> Option<lyra_parser::SyntaxToken> {
            $crate::support::token_in(&self.syntax, &[
                $(lyra_lexer::SyntaxKind::$kind),+
            ])
        }
    };

    (@field_child $method:ident, $ty:ty) => {
        pub fn $method(&self) -> Option<$ty> {
            $crate::support::child(&self.syntax)
        }
    };

    (@field_children $method:ident, $ty:ty) => {
        pub fn $method(&self) -> $crate::support::AstChildren<$ty> {
            $crate::support::children(&self.syntax)
        }
    };
}

pub(crate) use ast_nodes;
