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
    assert_eq!(lex_kinds("$")[0], (SyntaxKind::Ident, "$"));
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
