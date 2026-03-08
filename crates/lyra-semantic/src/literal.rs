use lyra_ast::{Expr, ExprKind, Literal, LiteralKind};

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

/// Extract the bit width of a sized literal from an expression.
///
/// Strips `Expression` and `ParenExpr` wrappers recursively to find a
/// `Literal` node, then checks if it is sized. Returns `Some(width)` for
/// sized literals, `None` for unsized, non-literal, or malformed expressions.
pub(crate) fn extract_sized_literal_width(expr: &Expr) -> Option<u32> {
    let ExprKind::Literal(lit) = expr.classify()? else {
        return None;
    };
    let shape = parse_literal_shape(&lit)?;
    if shape.is_unsized {
        None
    } else {
        Some(shape.width)
    }
}

/// Parse a `Literal` into its shape.
///
/// Uses the typed `literal_kind()` accessor to classify the literal.
/// Returns None for malformed or non-numeric literal forms (real, string).
pub(crate) fn parse_literal_shape(literal: &Literal) -> Option<LiteralShape> {
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
            prefix_token,
            digits_token,
        } => parse_based_shape(
            size_token.as_ref().map(|t| t.text()),
            prefix_token.text(),
            digits_token.as_ref().map(|t| t.text()),
        ),
        _ => None,
    }
}

/// Parse a based literal from prefix text and optional digits text into its shape.
///
/// `prefix_text` is the `BasedLiteralPrefix` token: tick + optional 's' + base char
/// (e.g. `'h`, `'sb`).
/// `digits_text` is the `BasedLiteralDigits` token (e.g. `FF`, `1010`).
/// Returns `None` if digits are missing (malformed literal).
fn parse_based_shape(
    size_text: Option<&str>,
    prefix_text: &str,
    digits_text: Option<&str>,
) -> Option<LiteralShape> {
    let after_tick = prefix_text.strip_prefix('\'')?;
    if after_tick.is_empty() {
        return None;
    }

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
    let digits_str = digits_text?;

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
    use lyra_ast::AstNode;
    use lyra_lexer::SyntaxKind;

    fn parse_shape(src: &str) -> Option<LiteralShape> {
        let full = format!("module m; parameter P = {src}; endmodule");
        let tokens = lyra_lexer::lex(&full);
        let pp = lyra_preprocess::preprocess_identity(lyra_source::FileId(0), &tokens, &full);
        let parse = lyra_parser::parse(&pp.tokens, &pp.expanded_text);
        let lit = find_literal(&parse.syntax())?;
        parse_literal_shape(&lit)
    }

    fn find_literal(node: &lyra_parser::SyntaxNode) -> Option<Literal> {
        if node.kind() == SyntaxKind::Literal {
            return Literal::cast(node.clone());
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

    #[test]
    fn spaced_prefix_digits_hex() {
        let s = parse_shape("'h FF").expect("should parse");
        assert_eq!(s.width, 32);
        assert!(!s.signed);
        assert_eq!(s.base, Base::Hex);
        assert!(s.is_unsized);
    }

    #[test]
    fn fully_spaced_sized_hex() {
        let s = parse_shape("32 'h 12ab_f001").expect("should parse");
        assert_eq!(s.width, 32);
        assert!(!s.signed);
        assert_eq!(s.base, Base::Hex);
        assert!(!s.is_unsized);
    }

    #[test]
    fn spaced_signed_decimal() {
        let s = parse_shape("16'sd 42").expect("should parse");
        assert_eq!(s.width, 16);
        assert!(s.signed);
        assert_eq!(s.base, Base::Decimal);
        assert!(!s.is_unsized);
    }

    #[test]
    fn spaced_xz_digits() {
        let s = parse_shape("'h 3x").expect("should parse");
        assert!(s.has_xz);
    }

    #[test]
    fn spaced_question_digit() {
        let s = parse_shape("16'sd ?").expect("should parse");
        assert!(s.has_xz);
        assert!(s.signed);
    }

    #[test]
    fn malformed_prefix_no_digits_hex() {
        assert!(parse_shape("'h ;").is_none());
    }

    #[test]
    fn malformed_sized_prefix_no_digits() {
        assert!(parse_shape("16'sd").is_none());
    }

    #[test]
    fn malformed_prefix_comment_no_digits() {
        assert!(parse_shape("'b /*comment*/ ;").is_none());
    }

    #[test]
    fn comment_between_prefix_and_digits() {
        let s = parse_shape("'h/*c*/FF").expect("should parse");
        assert_eq!(s.width, 32);
        assert_eq!(s.base, Base::Hex);
        assert!(s.is_unsized);
    }

    #[test]
    fn comment_between_size_prefix_digits() {
        let s = parse_shape("8 'h/*c*/FF").expect("should parse");
        assert_eq!(s.width, 8);
        assert_eq!(s.base, Base::Hex);
        assert!(!s.is_unsized);
    }

    #[test]
    fn line_comment_between_prefix_and_digits() {
        let s = parse_shape("8 'h // c\n FF").expect("should parse");
        assert_eq!(s.width, 8);
        assert_eq!(s.base, Base::Hex);
        assert!(!s.is_unsized);
    }
}
