use lyra_ast::{AstNode, Expr, LiteralKind};
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

/// Extract the bit width of a sized literal from an expression node.
///
/// Strips `Expression` and `ParenExpr` wrappers recursively to find a
/// `Literal` node, then checks if it is sized. Returns `Some(width)` for
/// sized literals, `None` for unsized, non-literal, or malformed expressions.
pub(crate) fn extract_sized_literal_width(expr_node: &SyntaxNode) -> Option<u32> {
    let inner = unwrap_parens(expr_node);
    if inner.kind() != SyntaxKind::Literal {
        return None;
    }
    let shape = parse_literal_shape(&inner)?;
    if shape.is_unsized {
        None
    } else {
        Some(shape.width)
    }
}

/// Strip `Expression` and `ParenExpr` wrappers to find the inner node.
fn unwrap_parens(node: &SyntaxNode) -> SyntaxNode {
    let mut current = node.clone();
    loop {
        match current.kind() {
            SyntaxKind::Expression => {
                match lyra_ast::Expression::cast(current.clone()).and_then(|e| e.inner()) {
                    Some(inner) => current = inner.syntax().clone(),
                    None => return current,
                }
            }
            SyntaxKind::ParenExpr => match Expr::cast(current.clone()).map(Expr::unwrap_parens) {
                Some(inner) => return inner.syntax().clone(),
                None => return current,
            },
            _ => return current,
        }
    }
}

/// Parse a Literal syntax node into its shape.
///
/// Accepts the Literal AST node directly and uses the typed `literal_kind()`
/// accessor to classify the literal. Returns None for malformed or
/// non-numeric literal forms (real, string).
pub(crate) fn parse_literal_shape(literal_node: &SyntaxNode) -> Option<LiteralShape> {
    let literal = lyra_ast::Literal::cast(literal_node.clone())?;
    let kind = literal.literal_kind()?;
    match kind {
        LiteralKind::Int { .. } => Some(LiteralShape {
            width: 32,
            signed: true,
            base: Base::Decimal,
            is_unsized: true,
            has_xz: false,
        }),
        LiteralKind::UnbasedUnsized { token } => Some(LiteralShape {
            width: 1,
            signed: false,
            base: Base::Binary,
            is_unsized: true,
            has_xz: token
                .text()
                .chars()
                .any(|c| matches!(c, 'x' | 'X' | 'z' | 'Z')),
        }),
        LiteralKind::Based {
            size_token,
            base_token,
        } => parse_based_shape(size_token.as_ref().map(|t| t.text()), base_token.text()),
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

    #[test]
    fn spaced_sized_hex() {
        let s = parse_shape("8 'hFF").expect("should parse");
        assert_eq!(s.width, 8);
        assert!(!s.signed);
        assert_eq!(s.base, Base::Hex);
        assert!(!s.is_unsized);
        assert!(!s.has_xz);
    }

    #[test]
    fn spaced_sized_binary() {
        let s = parse_shape("4 'b1010").expect("should parse");
        assert_eq!(s.width, 4);
        assert!(!s.signed);
        assert_eq!(s.base, Base::Binary);
        assert!(!s.is_unsized);
        assert!(!s.has_xz);
    }
}
