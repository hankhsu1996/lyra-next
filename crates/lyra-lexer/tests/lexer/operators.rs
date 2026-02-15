use super::common::{kinds, single};
use lyra_lexer::SyntaxKind;

#[test]
fn single_char_operators() {
    let cases = [
        ("!", SyntaxKind::Bang),
        ("~", SyntaxKind::Tilde),
        ("%", SyntaxKind::Percent),
        ("&", SyntaxKind::Amp),
        ("|", SyntaxKind::Pipe),
        ("^", SyntaxKind::Caret),
        ("?", SyntaxKind::Question),
        ("+", SyntaxKind::Plus),
        ("-", SyntaxKind::Minus),
        ("*", SyntaxKind::Star),
        ("/", SyntaxKind::Slash),
        ("<", SyntaxKind::Lt),
        (">", SyntaxKind::Gt),
        ("#", SyntaxKind::Hash),
        (":", SyntaxKind::Colon),
        (".", SyntaxKind::Dot),
        ("=", SyntaxKind::Assign),
    ];
    for (src, expected) in cases {
        assert_eq!(single(src).0, expected, "failed for {src:?}");
    }
}

#[test]
fn two_char_operators() {
    let cases = [
        ("==", SyntaxKind::EqEq),
        ("!=", SyntaxKind::BangEq),
        ("&&", SyntaxKind::AmpAmp),
        ("||", SyntaxKind::PipePipe),
        ("**", SyntaxKind::StarStar),
        ("<=", SyntaxKind::LtEq),
        (">=", SyntaxKind::GtEq),
        ("<<", SyntaxKind::LtLt),
        (">>", SyntaxKind::GtGt),
        ("~&", SyntaxKind::TildeAmp),
        ("~|", SyntaxKind::TildePipe),
        ("~^", SyntaxKind::TildeCaret),
        ("^~", SyntaxKind::CaretTilde),
        ("++", SyntaxKind::PlusPlus),
        ("--", SyntaxKind::MinusMinus),
        ("->", SyntaxKind::MinusGt),
        ("+=", SyntaxKind::PlusEq),
        ("-=", SyntaxKind::MinusEq),
        ("*=", SyntaxKind::StarEq),
        ("/=", SyntaxKind::SlashEq),
        ("%=", SyntaxKind::PercentEq),
        ("&=", SyntaxKind::AmpEq),
        ("|=", SyntaxKind::PipeEq),
        ("^=", SyntaxKind::CaretEq),
        ("##", SyntaxKind::HashHash),
        ("::", SyntaxKind::ColonColon),
        (":=", SyntaxKind::ColonEq),
        (":/", SyntaxKind::ColonSlash),
        (".*", SyntaxKind::DotStar),
    ];
    for (src, expected) in cases {
        assert_eq!(single(src).0, expected, "failed for {src:?}");
    }
}

#[test]
fn three_char_operators() {
    let cases = [
        ("===", SyntaxKind::EqEqEq),
        ("!==", SyntaxKind::BangEqEq),
        ("==?", SyntaxKind::EqEqQuestion),
        ("!=?", SyntaxKind::BangEqQuestion),
        ("<<<", SyntaxKind::LtLtLt),
        (">>>", SyntaxKind::GtGtGt),
        ("<<=", SyntaxKind::LtLtEq),
        (">>=", SyntaxKind::GtGtEq),
        ("->>", SyntaxKind::MinusGtGt),
        ("<->", SyntaxKind::LtMinusGt),
    ];
    for (src, expected) in cases {
        assert_eq!(single(src).0, expected, "failed for {src:?}");
    }
}

#[test]
fn four_char_operators() {
    assert_eq!(single("<<<=").0, SyntaxKind::LtLtLtEq);
    assert_eq!(single(">>>=").0, SyntaxKind::GtGtGtEq);
}

#[test]
fn lt_minus_is_two_tokens() {
    assert_eq!(kinds("<-"), vec![SyntaxKind::Lt, SyntaxKind::Minus]);
}

#[test]
fn eq_eq_not_over_consumed() {
    assert_eq!(
        kinds("== ="),
        vec![SyntaxKind::EqEq, SyntaxKind::Whitespace, SyntaxKind::Assign]
    );
}
