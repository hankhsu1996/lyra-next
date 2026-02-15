mod keywords;
mod kind;

pub use kind::{NODE_START, SyntaxKind};

use lyra_source::TextSize;

/// A lexed token (kind + length).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token {
    pub kind: SyntaxKind,
    pub len: TextSize,
}

/// Lex the full source string into a list of tokens (including trivia).
pub fn lex(src: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut rest = src;

    while !rest.is_empty() {
        let (kind, consumed) = lex_one(rest);
        tokens.push(Token {
            kind,
            #[allow(clippy::cast_possible_truncation)]
            len: TextSize::new(consumed as u32),
        });
        rest = &rest[consumed..];
    }

    tokens.push(Token {
        kind: SyntaxKind::Eof,
        len: TextSize::new(0),
    });
    tokens
}

fn lex_one(s: &str) -> (SyntaxKind, usize) {
    let bytes = s.as_bytes();
    let c = bytes[0];

    // Whitespace
    if c.is_ascii_whitespace() {
        let n = bytes.iter().take_while(|b| b.is_ascii_whitespace()).count();
        return (SyntaxKind::Whitespace, n);
    }

    // Line comment
    if c == b'/' && bytes.get(1) == Some(&b'/') {
        let n = bytes.iter().take_while(|&&b| b != b'\n').count();
        return (SyntaxKind::LineComment, n);
    }

    // Block comment
    if c == b'/' && bytes.get(1) == Some(&b'*') {
        let end = s[2..].find("*/").map_or(s.len(), |i| i + 4);
        return (SyntaxKind::BlockComment, end);
    }

    // Operators and punctuation (longest-match decision tree)
    if let Some(r) = lex_operator(bytes) {
        return r;
    }

    // String literal
    if c == b'"' {
        return lex_string(bytes);
    }

    // Tick: unbased unsized literal, based literal, tick-brace
    if c == b'\'' {
        return lex_tick(bytes);
    }

    // Numeric literal (integer or real)
    if c.is_ascii_digit() {
        return lex_number(bytes);
    }

    // Escaped identifier
    if c == b'\\' {
        return lex_escaped_ident(bytes);
    }

    // System identifier ($display, $finish, etc.)
    if c == b'$' {
        return lex_system_ident(bytes);
    }

    // Compiler directive (`define, `ifdef, etc.)
    if c == b'`' {
        return lex_directive(bytes);
    }

    // Identifier / keyword
    if c.is_ascii_alphabetic() || c == b'_' {
        let n = bytes
            .iter()
            .take_while(|b| b.is_ascii_alphanumeric() || **b == b'_' || **b == b'$')
            .count();
        let word = &s[..n];
        let kind = keywords::classify_keyword(word);
        return (kind, n);
    }

    // Unknown -> error token, consume one byte
    (SyntaxKind::Error, 1)
}

// Operator/punctuation dispatch. Returns None for non-operator bytes.
fn lex_operator(bytes: &[u8]) -> Option<(SyntaxKind, usize)> {
    let r = match bytes[0] {
        b'!' => lex_bang(bytes),
        b'=' => lex_eq(bytes),
        b'<' => lex_lt(bytes),
        b'>' => lex_gt(bytes),
        b'&' => lex_amp(bytes),
        b'|' => lex_pipe(bytes),
        b'^' => lex_caret(bytes),
        b'~' => lex_tilde(bytes),
        b'+' => lex_plus(bytes),
        b'-' => lex_minus(bytes),
        b'*' => lex_star(bytes),
        b'/' => lex_slash(bytes),
        b'%' => lex_percent(bytes),
        b'#' => {
            if bytes.get(1) == Some(&b'#') {
                (SyntaxKind::HashHash, 2)
            } else {
                (SyntaxKind::Hash, 1)
            }
        }
        b':' => lex_colon(bytes),
        b'.' => {
            if bytes.get(1) == Some(&b'*') {
                (SyntaxKind::DotStar, 2)
            } else {
                (SyntaxKind::Dot, 1)
            }
        }
        b'?' => (SyntaxKind::Question, 1),
        b';' => (SyntaxKind::Semicolon, 1),
        b',' => (SyntaxKind::Comma, 1),
        b'(' => (SyntaxKind::LParen, 1),
        b')' => (SyntaxKind::RParen, 1),
        b'{' => (SyntaxKind::LBrace, 1),
        b'}' => (SyntaxKind::RBrace, 1),
        b'[' => (SyntaxKind::LBracket, 1),
        b']' => (SyntaxKind::RBracket, 1),
        b'@' => (SyntaxKind::At, 1),
        _ => return None,
    };
    Some(r)
}

fn lex_bang(bytes: &[u8]) -> (SyntaxKind, usize) {
    if bytes.get(1) == Some(&b'=') {
        match bytes.get(2) {
            Some(&b'=') => return (SyntaxKind::BangEqEq, 3),
            Some(&b'?') => return (SyntaxKind::BangEqQuestion, 3),
            _ => return (SyntaxKind::BangEq, 2),
        }
    }
    (SyntaxKind::Bang, 1)
}

fn lex_eq(bytes: &[u8]) -> (SyntaxKind, usize) {
    if bytes.get(1) == Some(&b'=') {
        match bytes.get(2) {
            Some(&b'=') => return (SyntaxKind::EqEqEq, 3),
            Some(&b'?') => return (SyntaxKind::EqEqQuestion, 3),
            _ => return (SyntaxKind::EqEq, 2),
        }
    }
    (SyntaxKind::Assign, 1)
}

fn lex_lt(bytes: &[u8]) -> (SyntaxKind, usize) {
    match bytes.get(1) {
        Some(&b'<') => match bytes.get(2) {
            Some(&b'<') => {
                if bytes.get(3) == Some(&b'=') {
                    return (SyntaxKind::LtLtLtEq, 4);
                }
                (SyntaxKind::LtLtLt, 3)
            }
            Some(&b'=') => (SyntaxKind::LtLtEq, 3),
            _ => (SyntaxKind::LtLt, 2),
        },
        Some(&b'=') => (SyntaxKind::LtEq, 2),
        Some(&b'-') if bytes.get(2) == Some(&b'>') => (SyntaxKind::LtMinusGt, 3),
        _ => (SyntaxKind::Lt, 1),
    }
}

fn lex_gt(bytes: &[u8]) -> (SyntaxKind, usize) {
    match bytes.get(1) {
        Some(&b'>') => match bytes.get(2) {
            Some(&b'>') => {
                if bytes.get(3) == Some(&b'=') {
                    return (SyntaxKind::GtGtGtEq, 4);
                }
                (SyntaxKind::GtGtGt, 3)
            }
            Some(&b'=') => (SyntaxKind::GtGtEq, 3),
            _ => (SyntaxKind::GtGt, 2),
        },
        Some(&b'=') => (SyntaxKind::GtEq, 2),
        _ => (SyntaxKind::Gt, 1),
    }
}

fn lex_amp(bytes: &[u8]) -> (SyntaxKind, usize) {
    match bytes.get(1) {
        Some(&b'&') => (SyntaxKind::AmpAmp, 2),
        Some(&b'=') => (SyntaxKind::AmpEq, 2),
        _ => (SyntaxKind::Amp, 1),
    }
}

fn lex_pipe(bytes: &[u8]) -> (SyntaxKind, usize) {
    match bytes.get(1) {
        Some(&b'|') => (SyntaxKind::PipePipe, 2),
        Some(&b'=') => (SyntaxKind::PipeEq, 2),
        _ => (SyntaxKind::Pipe, 1),
    }
}

fn lex_caret(bytes: &[u8]) -> (SyntaxKind, usize) {
    match bytes.get(1) {
        Some(&b'~') => (SyntaxKind::CaretTilde, 2),
        Some(&b'=') => (SyntaxKind::CaretEq, 2),
        _ => (SyntaxKind::Caret, 1),
    }
}

fn lex_tilde(bytes: &[u8]) -> (SyntaxKind, usize) {
    match bytes.get(1) {
        Some(&b'&') => (SyntaxKind::TildeAmp, 2),
        Some(&b'|') => (SyntaxKind::TildePipe, 2),
        Some(&b'^') => (SyntaxKind::TildeCaret, 2),
        _ => (SyntaxKind::Tilde, 1),
    }
}

fn lex_plus(bytes: &[u8]) -> (SyntaxKind, usize) {
    match bytes.get(1) {
        Some(&b'+') => (SyntaxKind::PlusPlus, 2),
        Some(&b'=') => (SyntaxKind::PlusEq, 2),
        _ => (SyntaxKind::Plus, 1),
    }
}

fn lex_minus(bytes: &[u8]) -> (SyntaxKind, usize) {
    match bytes.get(1) {
        Some(&b'-') => (SyntaxKind::MinusMinus, 2),
        Some(&b'>') => {
            if bytes.get(2) == Some(&b'>') {
                (SyntaxKind::MinusGtGt, 3)
            } else {
                (SyntaxKind::MinusGt, 2)
            }
        }
        Some(&b'=') => (SyntaxKind::MinusEq, 2),
        _ => (SyntaxKind::Minus, 1),
    }
}

fn lex_star(bytes: &[u8]) -> (SyntaxKind, usize) {
    match bytes.get(1) {
        Some(&b'*') => (SyntaxKind::StarStar, 2),
        Some(&b'=') => (SyntaxKind::StarEq, 2),
        _ => (SyntaxKind::Star, 1),
    }
}

fn lex_slash(bytes: &[u8]) -> (SyntaxKind, usize) {
    match bytes.get(1) {
        Some(&b'=') => (SyntaxKind::SlashEq, 2),
        _ => (SyntaxKind::Slash, 1),
    }
}

fn lex_percent(bytes: &[u8]) -> (SyntaxKind, usize) {
    match bytes.get(1) {
        Some(&b'=') => (SyntaxKind::PercentEq, 2),
        _ => (SyntaxKind::Percent, 1),
    }
}

fn lex_colon(bytes: &[u8]) -> (SyntaxKind, usize) {
    match bytes.get(1) {
        Some(&b':') => (SyntaxKind::ColonColon, 2),
        Some(&b'=') => (SyntaxKind::ColonEq, 2),
        Some(&b'/') => (SyntaxKind::ColonSlash, 2),
        _ => (SyntaxKind::Colon, 1),
    }
}

// Lex string literals (regular and triple-quoted).
fn lex_string(bytes: &[u8]) -> (SyntaxKind, usize) {
    // Check for triple-quoted string
    if bytes.get(1) == Some(&b'"') && bytes.get(2) == Some(&b'"') {
        let mut i = 3;
        loop {
            if i >= bytes.len() {
                return (SyntaxKind::StringLiteral, bytes.len());
            }
            if bytes[i] == b'\\' && i + 1 < bytes.len() {
                i += 2;
                continue;
            }
            if bytes[i] == b'"'
                && i + 1 < bytes.len()
                && bytes[i + 1] == b'"'
                && i + 2 < bytes.len()
                && bytes[i + 2] == b'"'
            {
                return (SyntaxKind::StringLiteral, i + 3);
            }
            i += 1;
        }
    }

    // Regular quoted string
    let mut i = 1;
    loop {
        if i >= bytes.len() {
            return (SyntaxKind::StringLiteral, bytes.len());
        }
        if bytes[i] == b'\\' && i + 1 < bytes.len() {
            i += 2;
            continue;
        }
        if bytes[i] == b'"' {
            return (SyntaxKind::StringLiteral, i + 1);
        }
        i += 1;
    }
}

// Lex tick-prefixed tokens: `'{`, unbased unsized literals, based literals.
fn lex_tick(bytes: &[u8]) -> (SyntaxKind, usize) {
    match bytes.get(1) {
        Some(&b'{') => (SyntaxKind::TickBrace, 2),
        Some(&(b'0' | b'1' | b'x' | b'X' | b'z' | b'Z')) => {
            if is_ident_continue(bytes.get(2)) {
                (SyntaxKind::Error, 1)
            } else {
                (SyntaxKind::UnbasedUnsizedLiteral, 2)
            }
        }
        Some(&c) if is_base_char(c) => lex_based_value(bytes, 2),
        Some(&(b's' | b'S')) => match bytes.get(2) {
            Some(&c) if is_base_char(c) => lex_based_value(bytes, 3),
            _ => (SyntaxKind::Error, 1),
        },
        _ => (SyntaxKind::Error, 1),
    }
}

fn is_base_char(c: u8) -> bool {
    matches!(c, b'b' | b'B' | b'd' | b'D' | b'h' | b'H' | b'o' | b'O')
}

fn is_ident_continue(b: Option<&u8>) -> bool {
    matches!(b, Some(&c) if c.is_ascii_alphanumeric() || c == b'_')
}

// Consume based literal value digits after the base specifier.
fn lex_based_value(bytes: &[u8], start: usize) -> (SyntaxKind, usize) {
    let mut i = start;
    while i < bytes.len() {
        let c = bytes[i];
        if c.is_ascii_hexdigit() || matches!(c, b'_' | b'x' | b'X' | b'z' | b'Z' | b'?') {
            i += 1;
        } else {
            break;
        }
    }
    (SyntaxKind::BasedLiteral, i)
}

// Lex a numeric literal starting with a digit.
fn lex_number(bytes: &[u8]) -> (SyntaxKind, usize) {
    let mut i = 1;
    while i < bytes.len() && (bytes[i].is_ascii_digit() || bytes[i] == b'_') {
        i += 1;
    }

    // Check for real literal: digits `.` digit
    if i < bytes.len() && bytes[i] == b'.' {
        if i + 1 < bytes.len() && bytes[i + 1].is_ascii_digit() {
            i += 1;
            while i < bytes.len() && (bytes[i].is_ascii_digit() || bytes[i] == b'_') {
                i += 1;
            }
            i = consume_exponent(bytes, i);
            return (SyntaxKind::RealLiteral, i);
        }
        return (SyntaxKind::IntLiteral, i);
    }

    // Check for exponent without decimal point (e.g., `23E10`)
    if i < bytes.len() && matches!(bytes[i], b'e' | b'E') {
        let has_sign = matches!(bytes.get(i + 1), Some(&(b'+' | b'-')));
        let digit_pos = if has_sign { i + 2 } else { i + 1 };
        if digit_pos < bytes.len() && bytes[digit_pos].is_ascii_digit() {
            i = consume_exponent(bytes, i);
            return (SyntaxKind::RealLiteral, i);
        }
    }

    (SyntaxKind::IntLiteral, i)
}

fn consume_exponent(bytes: &[u8], start: usize) -> usize {
    let mut i = start;
    if i < bytes.len() && matches!(bytes[i], b'e' | b'E') {
        i += 1;
        if i < bytes.len() && matches!(bytes[i], b'+' | b'-') {
            i += 1;
        }
        while i < bytes.len() && (bytes[i].is_ascii_digit() || bytes[i] == b'_') {
            i += 1;
        }
    }
    i
}

fn lex_escaped_ident(bytes: &[u8]) -> (SyntaxKind, usize) {
    let mut i = 1;
    while i < bytes.len() && !bytes[i].is_ascii_whitespace() {
        i += 1;
    }
    (SyntaxKind::EscapedIdent, i)
}

fn lex_system_ident(bytes: &[u8]) -> (SyntaxKind, usize) {
    if let Some(&c) = bytes.get(1)
        && (c.is_ascii_alphabetic() || c == b'_' || c == b'$')
    {
        let mut i = 2;
        while i < bytes.len()
            && (bytes[i].is_ascii_alphanumeric() || bytes[i] == b'_' || bytes[i] == b'$')
        {
            i += 1;
        }
        return (SyntaxKind::SystemIdent, i);
    }
    (SyntaxKind::Ident, 1)
}

fn lex_directive(bytes: &[u8]) -> (SyntaxKind, usize) {
    if let Some(&c) = bytes.get(1)
        && (c.is_ascii_alphabetic() || c == b'_')
    {
        let mut i = 2;
        while i < bytes.len() && (bytes[i].is_ascii_alphanumeric() || bytes[i] == b'_') {
            i += 1;
        }
        return (SyntaxKind::Directive, i);
    }
    (SyntaxKind::Error, 1)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex_kinds(src: &str) -> Vec<(SyntaxKind, &str)> {
        let tokens = lex(src);
        let mut result = Vec::new();
        let mut pos = 0usize;
        for tok in &tokens {
            if tok.kind == SyntaxKind::Eof {
                break;
            }
            let len: usize = tok.len.into();
            result.push((tok.kind, &src[pos..pos + len]));
            pos += len;
        }
        result
    }

    fn kinds(src: &str) -> Vec<SyntaxKind> {
        lex_kinds(src).into_iter().map(|(k, _)| k).collect()
    }

    fn single(src: &str) -> (SyntaxKind, &str) {
        lex_kinds(src)
            .into_iter()
            .find(|(k, _)| *k != SyntaxKind::Whitespace)
            .expect("expected at least one non-whitespace token")
    }

    #[test]
    fn lex_module_header() {
        assert_eq!(
            kinds("module foo;"),
            vec![
                SyntaxKind::ModuleKw,
                SyntaxKind::Whitespace,
                SyntaxKind::Ident,
                SyntaxKind::Semicolon,
            ]
        );
    }

    #[test]
    fn trivia_preserved() {
        let tokens = lex("// comment\nmodule");
        assert_eq!(tokens[0].kind, SyntaxKind::LineComment);
        assert_eq!(tokens[1].kind, SyntaxKind::Whitespace);
        assert_eq!(tokens[2].kind, SyntaxKind::ModuleKw);
    }

    // Operators

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

    // Numeric literals

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

    // Strings

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
    fn unterminated_string_at_eof() {
        assert_eq!(
            lex_kinds("\"hello")[0],
            (SyntaxKind::StringLiteral, "\"hello")
        );
    }

    // Identifiers

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

    // Keywords

    #[test]
    fn keywords_vs_identifiers() {
        assert_eq!(single("module").0, SyntaxKind::ModuleKw);
        assert_eq!(single("Module").0, SyntaxKind::Ident);
    }

    #[test]
    fn all_original_keywords() {
        let cases = [
            ("module", SyntaxKind::ModuleKw),
            ("endmodule", SyntaxKind::EndmoduleKw),
            ("input", SyntaxKind::InputKw),
            ("output", SyntaxKind::OutputKw),
            ("inout", SyntaxKind::InoutKw),
            ("wire", SyntaxKind::WireKw),
            ("reg", SyntaxKind::RegKw),
            ("logic", SyntaxKind::LogicKw),
            ("assign", SyntaxKind::AssignKw),
            ("always", SyntaxKind::AlwaysKw),
            ("initial", SyntaxKind::InitialKw),
            ("begin", SyntaxKind::BeginKw),
            ("end", SyntaxKind::EndKw),
            ("if", SyntaxKind::IfKw),
            ("else", SyntaxKind::ElseKw),
            ("parameter", SyntaxKind::ParameterKw),
        ];
        for (src, expected) in cases {
            assert_eq!(single(src).0, expected, "failed for {src:?}");
        }
    }

    #[test]
    fn new_keyword_samples() {
        let cases = [
            ("always_comb", SyntaxKind::AlwaysCombKw),
            ("always_ff", SyntaxKind::AlwaysFfKw),
            ("always_latch", SyntaxKind::AlwaysLatchKw),
            ("class", SyntaxKind::ClassKw),
            ("interface", SyntaxKind::InterfaceKw),
            ("package", SyntaxKind::PackageKw),
            ("function", SyntaxKind::FunctionKw),
            ("task", SyntaxKind::TaskKw),
            ("typedef", SyntaxKind::TypedefKw),
            ("enum", SyntaxKind::EnumKw),
            ("struct", SyntaxKind::StructKw),
            ("union", SyntaxKind::UnionKw),
            ("constraint", SyntaxKind::ConstraintKw),
            ("virtual", SyntaxKind::VirtualKw),
            ("static", SyntaxKind::StaticKw),
            ("automatic", SyntaxKind::AutomaticKw),
            ("generate", SyntaxKind::GenerateKw),
            ("localparam", SyntaxKind::LocalparamKw),
            ("foreach", SyntaxKind::ForeachKw),
            ("forever", SyntaxKind::ForeverKw),
            ("shortint", SyntaxKind::ShortintKw),
            ("longint", SyntaxKind::LongintKw),
            ("shortreal", SyntaxKind::ShortRealKw),
            ("chandle", SyntaxKind::ChandleKw),
            ("string", SyntaxKind::StringKw),
            ("null", SyntaxKind::NullKw),
            ("this", SyntaxKind::ThisKw),
            ("super", SyntaxKind::SuperKw),
            ("import", SyntaxKind::ImportKw),
            ("export", SyntaxKind::ExportKw),
        ];
        for (src, expected) in cases {
            assert_eq!(single(src).0, expected, "failed for {src:?}");
        }
    }

    // Ordinal guard

    #[test]
    fn token_ordinal_stability() {
        // Token ordinals must never change across releases.
        // If this test fails, a variant was inserted or reordered.
        assert_eq!(SyntaxKind::Whitespace as u16, 0);
        assert_eq!(SyntaxKind::Semicolon as u16, 3);
        assert_eq!(SyntaxKind::Ident as u16, 22);
        assert_eq!(SyntaxKind::ModuleKw as u16, 23);
        assert_eq!(SyntaxKind::Error as u16, 39);
        assert_eq!(SyntaxKind::Eof as u16, 40);
        // First new token appended after Eof
        assert_eq!(SyntaxKind::Bang as u16, 41);
        // NODE_START must be after all token variants
        assert!(NODE_START > SyntaxKind::XorKw as u16);
        assert!(NODE_START == SyntaxKind::__NodeStart as u16);
        assert_eq!(SyntaxKind::SourceFile as u16, NODE_START + 1);
    }

    // Directive vs keyword precedence

    #[test]
    fn directive_include_vs_keyword_include() {
        // Backtick form -> Directive token
        assert_eq!(single("`include"), (SyntaxKind::Directive, "`include"));
        assert_eq!(single("`ifdef"), (SyntaxKind::Directive, "`ifdef"));
        assert_eq!(single("`define"), (SyntaxKind::Directive, "`define"));
        assert_eq!(single("`incdir"), (SyntaxKind::Directive, "`incdir"));

        // Bare form -> keyword (LRM Annex B reserved words)
        assert_eq!(single("include").0, SyntaxKind::IncludeKw);
        assert_eq!(single("incdir").0, SyntaxKind::IncdirKw);
    }

    // Triple-quoted string edge cases

    #[test]
    fn triple_quoted_string_multiline() {
        let src = "\"\"\"line1\nline2\nline3\"\"\"";
        assert_eq!(single(src), (SyntaxKind::StringLiteral, src));
    }

    #[test]
    fn triple_quoted_string_with_escape() {
        // Backslash-quote inside triple-quoted string
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
        // Only two quotes inside -- not a terminator
        let src = "\"\"\"has \"\" inside\"\"\"";
        assert_eq!(single(src), (SyntaxKind::StringLiteral, src));
    }

    #[test]
    fn triple_quoted_empty() {
        let src = "\"\"\"\"\"\"";
        assert_eq!(single(src), (SyntaxKind::StringLiteral, src));
    }

    // Roundtrip

    #[test]
    fn roundtrip_lex_all_text() {
        let inputs = [
            "module foo; endmodule",
            "assign a = b + c;",
            "4'b1001 8'hFF 'x 'z",
            "1.2 3.14e-2 23E10",
            "\\bus+index $display `define",
            "== != === !== ==? !=? << >> <<< >>> <-> ->>",
            "\"hello\" \"\"\"triple\"\"\"",
            "// comment\n/* block */",
        ];
        for input in inputs {
            let tokens = lex(input);
            let mut reconstructed = String::new();
            let mut pos = 0usize;
            for tok in &tokens {
                if tok.kind == SyntaxKind::Eof {
                    break;
                }
                let len: usize = tok.len.into();
                reconstructed.push_str(&input[pos..pos + len]);
                pos += len;
            }
            assert_eq!(reconstructed, input, "roundtrip failed for: {input:?}");
        }
    }

    #[test]
    fn block_comment_unterminated() {
        let k = lex_kinds("/* unterminated");
        assert_eq!(k[0], (SyntaxKind::BlockComment, "/* unterminated"));
    }
}
