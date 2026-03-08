use lyra_source::{TextRange, TokenSpan};

use crate::SyntaxKind;

/// Kind-aware token payload: `SyntaxKind` + `TokenSpan`.
///
/// Lightweight lexical anchor for semantic data that needs both token
/// kind and position without carrying CST downstream. This is a
/// payload type, not an identity object.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TokenAnchor {
    kind: SyntaxKind,
    span: TokenSpan,
}

impl TokenAnchor {
    pub fn new(kind: SyntaxKind, range: TextRange) -> Self {
        Self {
            kind,
            span: TokenSpan::new(range),
        }
    }

    pub fn kind(&self) -> SyntaxKind {
        self.kind
    }

    pub fn text_range(&self) -> TextRange {
        self.span.text_range()
    }

    pub fn span(&self) -> TokenSpan {
        self.span
    }

    /// Extract the token text from the expanded source.
    pub fn text_from<'a>(&self, source: &'a str) -> &'a str {
        self.span.text_from(source)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lyra_source::{TextRange, TextSize};

    #[test]
    fn construction_and_accessors() {
        let range = TextRange::new(TextSize::new(10), TextSize::new(13));
        let anchor = TokenAnchor::new(SyntaxKind::AndKw, range);
        assert_eq!(anchor.kind(), SyntaxKind::AndKw);
        assert_eq!(anchor.text_range(), range);
        assert_eq!(anchor.span().text_range(), range);
    }

    #[test]
    fn ordering() {
        let a = TokenAnchor::new(
            SyntaxKind::Ident,
            TextRange::new(TextSize::new(0), TextSize::new(3)),
        );
        let b = TokenAnchor::new(
            SyntaxKind::Ident,
            TextRange::new(TextSize::new(5), TextSize::new(8)),
        );
        assert!(a < b);
    }

    #[test]
    fn text_recovery() {
        let source = "module test; endmodule";
        let range = TextRange::new(TextSize::new(7), TextSize::new(11));
        let anchor = TokenAnchor::new(SyntaxKind::Ident, range);
        assert_eq!(anchor.text_from(source), "test");
    }
}
