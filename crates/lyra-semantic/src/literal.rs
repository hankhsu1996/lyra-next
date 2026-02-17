use lyra_lexer::SyntaxKind;
use lyra_parser::SyntaxNode;

/// Numeric base for a literal.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Base {
    Decimal,
    Hex,
    Octal,
    Binary,
}

/// Decoded shape of a numeric literal.
///
/// Captures width, signedness, base, and structural properties without
/// evaluating the numeric value. Shared by const-eval (value extraction)
/// and type-infer (width/signedness extraction).
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct LiteralShape {
    /// Bit width: 1 for unbased unsized, 32 for unsized decimal, N for sized.
    pub width: u32,
    /// True only if 's' prefix present in based literal.
    pub signed: bool,
    /// Numeric base.
    pub base: Base,
    /// True if no size prefix (unsized literal).
    pub is_unsized: bool,
    /// True if x/z/? digits present in based literal.
    pub has_xz: bool,
}

/// Parse a Literal syntax node into its shape.
///
/// Accepts the Literal AST node directly (inspects child tokens `IntLiteral`,
/// `BasedLiteral`, `UnbasedUnsizedLiteral`, `RealLiteral`, `StringLiteral`).
/// Returns None for malformed or non-numeric literal forms (real, string).
pub(crate) fn parse_literal_shape(literal_node: &SyntaxNode) -> Option<LiteralShape> {
    let tokens: Vec<_> = literal_node
        .children_with_tokens()
        .filter_map(|el| el.into_token())
        .filter(|tok| {
            matches!(
                tok.kind(),
                SyntaxKind::IntLiteral
                    | SyntaxKind::BasedLiteral
                    | SyntaxKind::UnbasedUnsizedLiteral
            )
        })
        .collect();

    match tokens.as_slice() {
        // Pure decimal: 42, 1_000
        [tok] if tok.kind() == SyntaxKind::IntLiteral => Some(LiteralShape {
            width: 32,
            signed: true,
            base: Base::Decimal,
            is_unsized: true,
            has_xz: false,
        }),
        // Unbased unsized: '0, '1, 'x, 'z
        [tok] if tok.kind() == SyntaxKind::UnbasedUnsizedLiteral => Some(LiteralShape {
            width: 1,
            signed: false,
            base: Base::Binary,
            is_unsized: true,
            has_xz: tok
                .text()
                .chars()
                .any(|c| matches!(c, 'x' | 'X' | 'z' | 'Z')),
        }),
        // Unsized based: 'hFF, 'b1010
        [tok] if tok.kind() == SyntaxKind::BasedLiteral => parse_based_shape(None, tok.text()),
        // Sized based: 8'hFF
        [size_tok, based_tok]
            if size_tok.kind() == SyntaxKind::IntLiteral
                && based_tok.kind() == SyntaxKind::BasedLiteral =>
        {
            parse_based_shape(Some(size_tok.text()), based_tok.text())
        }
        _ => None,
    }
}

/// Parse a based literal token (with optional size prefix text) into its shape.
fn parse_based_shape(size_text: Option<&str>, based_text: &str) -> Option<LiteralShape> {
    // based_text starts with ' (tick)
    let after_tick = based_text.strip_prefix('\'')?;
    if after_tick.is_empty() {
        return None;
    }

    // Check for signed prefix
    let (rest, signed) = if after_tick
        .as_bytes()
        .first()
        .is_some_and(|&b| b == b's' || b == b'S')
    {
        (&after_tick[1..], true)
    } else {
        (after_tick, false)
    };

    if rest.is_empty() {
        return None;
    }

    let base_char = rest.as_bytes()[0];
    let digits_str = &rest[1..];

    let base = match base_char {
        b'h' | b'H' => Base::Hex,
        b'd' | b'D' => Base::Decimal,
        b'o' | b'O' => Base::Octal,
        b'b' | b'B' => Base::Binary,
        _ => return None,
    };

    let has_xz = digits_str
        .chars()
        .any(|c| matches!(c, 'x' | 'X' | 'z' | 'Z' | '?'));

    let (width, is_unsized) = match size_text {
        Some(s) => {
            let clean = s.replace('_', "");
            let w: u32 = clean.parse().ok()?;
            (w, false)
        }
        None => (32, true),
    };

    Some(LiteralShape {
        width,
        signed,
        base,
        is_unsized,
        has_xz,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_shape(src: &str) -> Option<LiteralShape> {
        let full = format!("module m; parameter P = {src}; endmodule");
        let tokens = lyra_lexer::lex(&full);
        let pp = lyra_preprocess::preprocess_identity(lyra_source::FileId(0), &tokens, &full);
        let parse = lyra_parser::parse(&pp.tokens, &pp.expanded_text);
        find_literal(&parse.syntax()).and_then(|n| parse_literal_shape(&n))
    }

    fn find_literal(node: &SyntaxNode) -> Option<SyntaxNode> {
        if node.kind() == SyntaxKind::Literal {
            return Some(node.clone());
        }
        for child in node.children() {
            if let Some(found) = find_literal(&child) {
                return Some(found);
            }
        }
        None
    }

    #[test]
    fn unsized_decimal() {
        let s = parse_shape("42").expect("should parse");
        assert_eq!(s.width, 32);
        assert!(s.signed);
        assert_eq!(s.base, Base::Decimal);
        assert!(s.is_unsized);
        assert!(!s.has_xz);
    }

    #[test]
    fn sized_hex() {
        let s = parse_shape("8'hFF").expect("should parse");
        assert_eq!(s.width, 8);
        assert!(!s.signed);
        assert_eq!(s.base, Base::Hex);
        assert!(!s.is_unsized);
        assert!(!s.has_xz);
    }

    #[test]
    fn sized_signed_hex() {
        let s = parse_shape("8'shFF").expect("should parse");
        assert_eq!(s.width, 8);
        assert!(s.signed);
        assert_eq!(s.base, Base::Hex);
        assert!(!s.is_unsized);
        assert!(!s.has_xz);
    }

    #[test]
    fn unsized_based() {
        let s = parse_shape("'hFF").expect("should parse");
        assert_eq!(s.width, 32);
        assert!(!s.signed);
        assert_eq!(s.base, Base::Hex);
        assert!(s.is_unsized);
        assert!(!s.has_xz);
    }

    #[test]
    fn unbased_unsized_one() {
        let s = parse_shape("'1").expect("should parse");
        assert_eq!(s.width, 1);
        assert!(!s.signed);
        assert!(!s.has_xz);
    }

    #[test]
    fn unbased_unsized_x() {
        let s = parse_shape("'x").expect("should parse");
        assert_eq!(s.width, 1);
        assert!(!s.signed);
        assert!(s.has_xz);
    }

    #[test]
    fn xz_digits() {
        let s = parse_shape("8'hxF").expect("should parse");
        assert!(s.has_xz);
    }

    #[test]
    fn binary_literal() {
        let s = parse_shape("4'b1010").expect("should parse");
        assert_eq!(s.width, 4);
        assert!(!s.signed);
        assert_eq!(s.base, Base::Binary);
    }

    #[test]
    fn real_literal_returns_none() {
        assert!(parse_shape("3.14").is_none());
    }
}
