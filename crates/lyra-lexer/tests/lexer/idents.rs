use super::common::{lex_kinds, single};
use lyra_lexer::SyntaxKind;

#[test]
fn escaped_identifiers() {
    let k = lex_kinds("\\bus+index ");
    assert_eq!(k[0], (SyntaxKind::EscapedIdent, "\\bus+index"));
    assert_eq!(k[1], (SyntaxKind::Whitespace, " "));
}

#[test]
fn escaped_ident_at_eof() {
    assert_eq!(lex_kinds("\\foo")[0], (SyntaxKind::EscapedIdent, "\\foo"));
}

#[test]
fn system_identifiers() {
    assert_eq!(single("$display"), (SyntaxKind::SystemIdent, "$display"));
    assert_eq!(single("$finish"), (SyntaxKind::SystemIdent, "$finish"));
}

#[test]
fn standalone_dollar() {
    assert_eq!(lex_kinds("$")[0], (SyntaxKind::Dollar, "$"));
}

#[test]
fn dollar_bracket() {
    let k = lex_kinds("$]");
    assert_eq!(k[0], (SyntaxKind::Dollar, "$"));
    assert_eq!(k[1], (SyntaxKind::RBracket, "]"));
}

#[test]
fn dollar_colon() {
    let k = lex_kinds("$:");
    assert_eq!(k[0], (SyntaxKind::Dollar, "$"));
    assert_eq!(k[1], (SyntaxKind::Colon, ":"));
}

#[test]
fn dollar_space() {
    let k = lex_kinds("$ ");
    assert_eq!(k[0], (SyntaxKind::Dollar, "$"));
    assert_eq!(k[1], (SyntaxKind::Whitespace, " "));
}

#[test]
fn dollar_digit() {
    let k = lex_kinds("$0");
    assert_eq!(k[0], (SyntaxKind::Dollar, "$"));
    assert_eq!(k[1], (SyntaxKind::IntLiteral, "0"));
}

#[test]
fn dollar_bits_still_system_ident() {
    assert_eq!(single("$bits"), (SyntaxKind::SystemIdent, "$bits"));
}

#[test]
fn dollar_dollar_still_system_ident() {
    assert_eq!(single("$$"), (SyntaxKind::SystemIdent, "$$"));
}

#[test]
fn dollar_foo_still_system_ident() {
    assert_eq!(single("$foo"), (SyntaxKind::SystemIdent, "$foo"));
}

#[test]
fn directives() {
    assert_eq!(single("`define"), (SyntaxKind::Directive, "`define"));
    assert_eq!(single("`ifdef"), (SyntaxKind::Directive, "`ifdef"));
}

#[test]
fn standalone_backtick() {
    assert_eq!(lex_kinds("`")[0].0, SyntaxKind::Error);
}

#[test]
fn directive_vs_keyword_precedence() {
    // Backtick form -> Directive token
    assert_eq!(single("`include"), (SyntaxKind::Directive, "`include"));
    assert_eq!(single("`ifdef"), (SyntaxKind::Directive, "`ifdef"));
    assert_eq!(single("`define"), (SyntaxKind::Directive, "`define"));
    assert_eq!(single("`incdir"), (SyntaxKind::Directive, "`incdir"));

    // Bare form -> keyword (LRM Annex B reserved words)
    assert_eq!(single("include").0, SyntaxKind::IncludeKw);
    assert_eq!(single("incdir").0, SyntaxKind::IncdirKw);
}
