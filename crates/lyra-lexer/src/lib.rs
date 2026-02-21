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
    let mut prev_sig_kind = SyntaxKind::Eof;
    let mut attr_depth: u32 = 0;

    while !rest.is_empty() {
        let after_at = prev_sig_kind == SyntaxKind::At;
        let (kind, consumed) = lex_one(rest, after_at, attr_depth);

        if kind == SyntaxKind::AttrOpen {
            attr_depth += 1;
        } else if kind == SyntaxKind::AttrClose {
            attr_depth = attr_depth.saturating_sub(1);
        }

        if is_attr_depth_reset(kind) {
            attr_depth = 0;
        }

        if !is_trivia(kind) {
            prev_sig_kind = kind;
        }

        tokens.push(Token {
            kind,
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

fn is_trivia(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::Whitespace | SyntaxKind::LineComment | SyntaxKind::BlockComment
    )
}

fn is_attr_depth_reset(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::Semicolon
            | SyntaxKind::EndKw
            | SyntaxKind::EndmoduleKw
            | SyntaxKind::EndpackageKw
            | SyntaxKind::EndinterfaceKw
            | SyntaxKind::EndprogramKw
    )
}

fn lex_one(s: &str, after_at: bool, attr_depth: u32) -> (SyntaxKind, usize) {
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
    if let Some(r) = lex_operator(bytes, after_at, attr_depth) {
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

    // Dollar: system identifier ($display, $finish) or standalone dollar (queue dim)
    if c == b'$' {
        if bytes
            .get(1)
            .is_some_and(|&c| c.is_ascii_alphabetic() || c == b'_' || c == b'$')
        {
            return lex_system_ident(bytes);
        }
        return (SyntaxKind::Dollar, 1);
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
fn lex_operator(bytes: &[u8], after_at: bool, attr_depth: u32) -> Option<(SyntaxKind, usize)> {
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
        b'*' => lex_star(bytes, attr_depth),
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
        b'(' => {
            if bytes.get(1) == Some(&b'*') && !after_at {
                (SyntaxKind::AttrOpen, 2)
            } else {
                (SyntaxKind::LParen, 1)
            }
        }
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

fn lex_star(bytes: &[u8], attr_depth: u32) -> (SyntaxKind, usize) {
    match bytes.get(1) {
        Some(&b'*') => (SyntaxKind::StarStar, 2),
        Some(&b'=') => (SyntaxKind::StarEq, 2),
        Some(&b')') if attr_depth > 0 => (SyntaxKind::AttrClose, 2),
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

// Check for a time unit suffix (s, ms, us, ns, ps, fs) at `pos` in `bytes`.
// Returns the suffix length if valid, or None.
fn try_time_suffix(bytes: &[u8], pos: usize) -> Option<usize> {
    let remaining = &bytes[pos..];
    let suffix_len = match remaining.first() {
        Some(&b's') => 1,
        Some(&(b'f' | b'm' | b'n' | b'p' | b'u')) if remaining.get(1) == Some(&b's') => 2,
        _ => return None,
    };
    if is_ident_continue(remaining.get(suffix_len)) {
        return None;
    }
    Some(suffix_len)
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
            if let Some(suf) = try_time_suffix(bytes, i) {
                return (SyntaxKind::TimeLiteral, i + suf);
            }
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
            if let Some(suf) = try_time_suffix(bytes, i) {
                return (SyntaxKind::TimeLiteral, i + suf);
            }
            return (SyntaxKind::RealLiteral, i);
        }
    }

    if let Some(suf) = try_time_suffix(bytes, i) {
        return (SyntaxKind::TimeLiteral, i + suf);
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
