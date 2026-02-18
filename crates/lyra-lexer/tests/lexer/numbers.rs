use super::common::{lex_kinds, single};
use lyra_lexer::SyntaxKind;

#[test]
fn plain_decimal() {
    assert_eq!(single("0"), (SyntaxKind::IntLiteral, "0"));
    assert_eq!(single("123"), (SyntaxKind::IntLiteral, "123"));
    assert_eq!(single("456_789"), (SyntaxKind::IntLiteral, "456_789"));
    assert_eq!(single("27_195_000"), (SyntaxKind::IntLiteral, "27_195_000"));
}

#[test]
fn based_literals() {
    let k = lex_kinds("4'b1001");
    assert_eq!(k[0], (SyntaxKind::IntLiteral, "4"));
    assert_eq!(k[1], (SyntaxKind::BasedLiteral, "'b1001"));

    let k = lex_kinds("8'hFF");
    assert_eq!(k[0], (SyntaxKind::IntLiteral, "8"));
    assert_eq!(k[1], (SyntaxKind::BasedLiteral, "'hFF"));

    let k = lex_kinds("16'o777");
    assert_eq!(k[0], (SyntaxKind::IntLiteral, "16"));
    assert_eq!(k[1], (SyntaxKind::BasedLiteral, "'o777"));

    let k = lex_kinds("32'd100");
    assert_eq!(k[0], (SyntaxKind::IntLiteral, "32"));
    assert_eq!(k[1], (SyntaxKind::BasedLiteral, "'d100"));
}

#[test]
fn signed_based_literals() {
    let k = lex_kinds("4'sb1010");
    assert_eq!(k[0], (SyntaxKind::IntLiteral, "4"));
    assert_eq!(k[1], (SyntaxKind::BasedLiteral, "'sb1010"));

    let k = lex_kinds("8'shFF");
    assert_eq!(k[0], (SyntaxKind::IntLiteral, "8"));
    assert_eq!(k[1], (SyntaxKind::BasedLiteral, "'shFF"));
}

#[test]
fn unbased_unsized_literals() {
    for s in ["'0", "'1", "'x", "'z", "'X", "'Z"] {
        assert_eq!(single(s), (SyntaxKind::UnbasedUnsizedLiteral, s), "for {s}");
    }
}

#[test]
fn real_literals() {
    assert_eq!(single("1.2"), (SyntaxKind::RealLiteral, "1.2"));
    assert_eq!(single("0.1"), (SyntaxKind::RealLiteral, "0.1"));
    assert_eq!(
        single("2394.26331"),
        (SyntaxKind::RealLiteral, "2394.26331")
    );
    assert_eq!(single("1.2E12"), (SyntaxKind::RealLiteral, "1.2E12"));
    assert_eq!(single("1.30e-2"), (SyntaxKind::RealLiteral, "1.30e-2"));
    assert_eq!(single("23E10"), (SyntaxKind::RealLiteral, "23E10"));
}

#[test]
fn numeric_negative_cases() {
    assert_eq!(lex_kinds("'2")[0].0, SyntaxKind::Error);
    assert_eq!(lex_kinds("'sx")[0].0, SyntaxKind::Error);

    let k = lex_kinds("1.");
    assert_eq!(k[0], (SyntaxKind::IntLiteral, "1"));
    assert_eq!(k[1], (SyntaxKind::Dot, "."));

    let k = lex_kinds("1e");
    assert_eq!(k[0], (SyntaxKind::IntLiteral, "1"));
    assert_eq!(k[1], (SyntaxKind::Ident, "e"));

    assert_eq!(lex_kinds("'")[0].0, SyntaxKind::Error);
}

#[test]
fn underscores_in_based() {
    let k = lex_kinds("16'b0011_0101_0001_1111");
    assert_eq!(k[0], (SyntaxKind::IntLiteral, "16"));
    assert_eq!(k[1], (SyntaxKind::BasedLiteral, "'b0011_0101_0001_1111"));
}

#[test]
fn tick_brace() {
    assert_eq!(single("'{"), (SyntaxKind::TickBrace, "'{"));
}

#[test]
fn real_dot_not_followed_by_digit() {
    let k = lex_kinds("4.E3");
    assert_eq!(k[0], (SyntaxKind::IntLiteral, "4"));
    assert_eq!(k[1], (SyntaxKind::Dot, "."));
    assert_eq!(k[2], (SyntaxKind::Ident, "E3"));
}

#[test]
fn time_literals() {
    assert_eq!(single("2ns"), (SyntaxKind::TimeLiteral, "2ns"));
    assert_eq!(single("40ps"), (SyntaxKind::TimeLiteral, "40ps"));
    assert_eq!(single("1us"), (SyntaxKind::TimeLiteral, "1us"));
    assert_eq!(single("100ms"), (SyntaxKind::TimeLiteral, "100ms"));
    assert_eq!(single("3s"), (SyntaxKind::TimeLiteral, "3s"));
    assert_eq!(single("10fs"), (SyntaxKind::TimeLiteral, "10fs"));
    assert_eq!(single("2.1ns"), (SyntaxKind::TimeLiteral, "2.1ns"));
    assert_eq!(single("23E10ns"), (SyntaxKind::TimeLiteral, "23E10ns"));
}

#[test]
fn time_literal_negative_cases() {
    // Not a time unit -- 'n' alone is not valid
    let k = lex_kinds("2n");
    assert_eq!(k[0], (SyntaxKind::IntLiteral, "2"));
    assert_eq!(k[1], (SyntaxKind::Ident, "n"));

    // Ident-continue after suffix rejects time literal
    let k = lex_kinds("2nsec");
    assert_eq!(k[0], (SyntaxKind::IntLiteral, "2"));
    assert_eq!(k[1], (SyntaxKind::Ident, "nsec"));

    let k = lex_kinds("2ns0");
    assert_eq!(k[0], (SyntaxKind::IntLiteral, "2"));
    assert_eq!(k[1], (SyntaxKind::Ident, "ns0"));

    let k = lex_kinds("2step");
    assert_eq!(k[0], (SyntaxKind::IntLiteral, "2"));
    assert_eq!(k[1], (SyntaxKind::Ident, "step"));

    // Whitespace separates: not a time literal
    let k = lex_kinds("2 ns");
    assert_eq!(k[0], (SyntaxKind::IntLiteral, "2"));
    assert_eq!(k[1].0, SyntaxKind::Whitespace);
    assert_eq!(k[2], (SyntaxKind::Ident, "ns"));

    // Dot-separated: 1.ns is not a time literal
    let k = lex_kinds("1.ns");
    assert_eq!(k[0], (SyntaxKind::IntLiteral, "1"));
    assert_eq!(k[1], (SyntaxKind::Dot, "."));
    assert_eq!(k[2], (SyntaxKind::Ident, "ns"));

    // Sized literal not affected
    let k = lex_kinds("4'shA");
    assert_eq!(k[0], (SyntaxKind::IntLiteral, "4"));
    assert_eq!(k[1], (SyntaxKind::BasedLiteral, "'shA"));
}
