use super::common::{lex_kinds, single};
use lyra_lexer::SyntaxKind;

#[test]
fn simple_string() {
    assert_eq!(
        single("\"hello\""),
        (SyntaxKind::StringLiteral, "\"hello\"")
    );
}

#[test]
fn string_with_escapes() {
    assert_eq!(
        single("\"hello\\nworld\""),
        (SyntaxKind::StringLiteral, "\"hello\\nworld\"")
    );
    assert_eq!(
        single("\"quote\\\"inside\""),
        (SyntaxKind::StringLiteral, "\"quote\\\"inside\"")
    );
}

#[test]
fn string_backslash_before_quote() {
    assert_eq!(
        single("\"path\\\\\""),
        (SyntaxKind::StringLiteral, "\"path\\\\\"")
    );
}

#[test]
fn triple_quoted_string() {
    let src = "\"\"\"hello \"world\" \"\"\"";
    assert_eq!(single(src), (SyntaxKind::StringLiteral, src));
}

#[test]
fn triple_quoted_string_multiline() {
    let src = "\"\"\"line1\nline2\nline3\"\"\"";
    assert_eq!(single(src), (SyntaxKind::StringLiteral, src));
}

#[test]
fn triple_quoted_string_with_escape() {
    let src = "\"\"\"hello \\\"world\\\" \"\"\"";
    assert_eq!(single(src), (SyntaxKind::StringLiteral, src));
}

#[test]
fn triple_quoted_string_unterminated_eof() {
    let src = "\"\"\"unterminated";
    assert_eq!(lex_kinds(src)[0], (SyntaxKind::StringLiteral, src));
}

#[test]
fn triple_quoted_string_partial_close() {
    let src = "\"\"\"has \"\" inside\"\"\"";
    assert_eq!(single(src), (SyntaxKind::StringLiteral, src));
}

#[test]
fn triple_quoted_empty() {
    let src = "\"\"\"\"\"\"";
    assert_eq!(single(src), (SyntaxKind::StringLiteral, src));
}

#[test]
fn unterminated_string_at_eof() {
    assert_eq!(
        lex_kinds("\"hello")[0],
        (SyntaxKind::StringLiteral, "\"hello")
    );
}
