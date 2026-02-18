use super::common::{kinds, lex_kinds, single};
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

#[test]
fn attr_open_token() {
    assert_eq!(single("(*").0, SyntaxKind::AttrOpen);
}

#[test]
fn attr_close_inside_attr() {
    let k = lex_kinds("(* foo *)");
    assert_eq!(k[0].0, SyntaxKind::AttrOpen);
    assert!(k.iter().any(|(kind, _)| *kind == SyntaxKind::AttrClose));
}

#[test]
fn star_rparen_outside_attr_is_two_tokens() {
    let k = lex_kinds("a *)");
    assert!(k.iter().any(|(kind, _)| *kind == SyntaxKind::Star));
    assert!(k.iter().any(|(kind, _)| *kind == SyntaxKind::RParen));
    assert!(!k.iter().any(|(kind, _)| *kind == SyntaxKind::AttrClose));
}

#[test]
fn at_paren_star_paren_not_attr() {
    let k = lex_kinds("@(*)");
    assert_eq!(k[0].0, SyntaxKind::At);
    assert_eq!(k[1].0, SyntaxKind::LParen);
    assert_eq!(k[2].0, SyntaxKind::Star);
    assert_eq!(k[3].0, SyntaxKind::RParen);
}

#[test]
fn at_spaced_star_is_not_attr() {
    let k = lex_kinds("@( * )");
    assert_eq!(k[0].0, SyntaxKind::At);
    assert_eq!(k[1].0, SyntaxKind::LParen);
    assert!(!k.iter().any(|(kind, _)| *kind == SyntaxKind::AttrOpen));
    assert!(!k.iter().any(|(kind, _)| *kind == SyntaxKind::AttrClose));
}

#[test]
fn at_space_paren_star_not_attr() {
    // `@ (*)` -- space between @ and (, still event wildcard
    let k = lex_kinds("@ (*)");
    assert_eq!(k[0].0, SyntaxKind::At);
    assert!(!k.iter().any(|(kind, _)| *kind == SyntaxKind::AttrOpen));
    assert!(!k.iter().any(|(kind, _)| *kind == SyntaxKind::AttrClose));
}

#[test]
fn attr_open_star_is_open_then_star() {
    let k = lex_kinds("(**");
    assert_eq!(k[0].0, SyntaxKind::AttrOpen);
    assert_eq!(k[1].0, SyntaxKind::Star);
}

#[test]
fn nested_attr_depth_tracking() {
    let k = lex_kinds("(* a = (* b *) *)");
    let opens: usize = k
        .iter()
        .filter(|(kind, _)| *kind == SyntaxKind::AttrOpen)
        .count();
    let closes: usize = k
        .iter()
        .filter(|(kind, _)| *kind == SyntaxKind::AttrClose)
        .count();
    assert_eq!(opens, 2);
    assert_eq!(closes, 2);
}

#[test]
fn at_paren_star_foo_not_attr() {
    let k = lex_kinds("@(* foo *)");
    assert!(!k.iter().any(|(kind, _)| *kind == SyntaxKind::AttrOpen));
    assert!(!k.iter().any(|(kind, _)| *kind == SyntaxKind::AttrClose));
}

#[test]
fn attr_depth_resets_at_semicolon() {
    let k = lex_kinds("(* a ; x *)");
    assert_eq!(k[0].0, SyntaxKind::AttrOpen);
    assert!(
        !k.iter()
            .skip(1)
            .any(|(kind, _)| *kind == SyntaxKind::AttrClose)
    );
}
