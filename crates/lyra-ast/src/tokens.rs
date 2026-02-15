use lyra_parser::SyntaxToken;
use lyra_source::TextRange;

/// Generate a typed token wrapper.
macro_rules! ast_token {
    ($name:ident) => {
        #[derive(Debug, Clone)]
        pub struct $name {
            pub(crate) token: SyntaxToken,
        }

        impl $name {
            /// The source text of this token.
            pub fn text(&self) -> &str {
                self.token.text()
            }

            /// The source range of this token.
            pub fn text_range(&self) -> TextRange {
                self.token.text_range()
            }
        }
    };
}

ast_token!(Ident);
ast_token!(Keyword);
ast_token!(LiteralToken);
