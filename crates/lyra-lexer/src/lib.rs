use lyra_source::TextSize;

/// Token kinds for `SystemVerilog`.
///
/// Layout: tokens `0..NODE_START`, nodes `NODE_START..`.
/// **Never reorder existing variants** -- ordinal stability matters for rowan.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
pub enum SyntaxKind {
    // Trivia
    Whitespace = 0,
    LineComment,
    BlockComment,

    // Punctuation
    Semicolon,
    Comma,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Colon,
    Dot,
    Assign,
    At,
    Hash,

    // Operators
    Plus,
    Minus,
    Star,
    Slash,

    // Literals
    IntLiteral,
    StringLiteral,

    // Identifiers
    Ident,

    // Keywords
    ModuleKw,
    EndmoduleKw,
    InputKw,
    OutputKw,
    InoutKw,
    WireKw,
    RegKw,
    LogicKw,
    AssignKw,
    AlwaysKw,
    InitialKw,
    BeginKw,
    EndKw,
    IfKw,
    ElseKw,
    ParameterKw,

    // Special
    Error,
    Eof,

    // Node boundary
    #[doc(hidden)]
    __NodeStart,

    // Nodes (used by parser/rowan)
    SourceFile,
    ModuleDecl,
    PortList,
    Port,
    ModuleBody,
    ContinuousAssign,
    Expression,
    ParamDecl,
}

/// First node kind value -- tokens are `< NODE_START`, nodes `>= NODE_START`.
pub const NODE_START: u16 = SyntaxKind::__NodeStart as u16;

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
    if bytes.len() >= 2 && c == b'/' && bytes[1] == b'/' {
        let n = bytes.iter().take_while(|&&b| b != b'\n').count();
        return (SyntaxKind::LineComment, n);
    }

    // Block comment
    if bytes.len() >= 2 && c == b'/' && bytes[1] == b'*' {
        let end = s[2..].find("*/").map_or(s.len(), |i| i + 4);
        return (SyntaxKind::BlockComment, end);
    }

    // Single-char punctuation
    let punct = match c {
        b';' => Some(SyntaxKind::Semicolon),
        b',' => Some(SyntaxKind::Comma),
        b'(' => Some(SyntaxKind::LParen),
        b')' => Some(SyntaxKind::RParen),
        b'{' => Some(SyntaxKind::LBrace),
        b'}' => Some(SyntaxKind::RBrace),
        b'[' => Some(SyntaxKind::LBracket),
        b']' => Some(SyntaxKind::RBracket),
        b':' => Some(SyntaxKind::Colon),
        b'.' => Some(SyntaxKind::Dot),
        b'=' => Some(SyntaxKind::Assign),
        b'@' => Some(SyntaxKind::At),
        b'#' => Some(SyntaxKind::Hash),
        b'+' => Some(SyntaxKind::Plus),
        b'-' => Some(SyntaxKind::Minus),
        b'*' => Some(SyntaxKind::Star),
        _ => None,
    };
    if let Some(kind) = punct {
        return (kind, 1);
    }

    // Slash (when not a comment)
    if c == b'/' {
        return (SyntaxKind::Slash, 1);
    }

    // String literal
    if c == b'"' {
        let n = 1 + bytes[1..].iter().take_while(|&&b| b != b'"').count() + 1;
        let n = n.min(s.len());
        return (SyntaxKind::StringLiteral, n);
    }

    // Integer literal
    if c.is_ascii_digit() {
        let n = bytes
            .iter()
            .take_while(|b| b.is_ascii_alphanumeric() || **b == b'_' || **b == b'\'')
            .count();
        return (SyntaxKind::IntLiteral, n);
    }

    // Identifier / keyword
    if c.is_ascii_alphabetic() || c == b'_' || c == b'$' {
        let n = bytes
            .iter()
            .take_while(|b| b.is_ascii_alphanumeric() || **b == b'_' || **b == b'$')
            .count();
        let word = &s[..n];
        let kind = match word {
            "module" => SyntaxKind::ModuleKw,
            "endmodule" => SyntaxKind::EndmoduleKw,
            "input" => SyntaxKind::InputKw,
            "output" => SyntaxKind::OutputKw,
            "inout" => SyntaxKind::InoutKw,
            "wire" => SyntaxKind::WireKw,
            "reg" => SyntaxKind::RegKw,
            "logic" => SyntaxKind::LogicKw,
            "assign" => SyntaxKind::AssignKw,
            "always" => SyntaxKind::AlwaysKw,
            "initial" => SyntaxKind::InitialKw,
            "begin" => SyntaxKind::BeginKw,
            "end" => SyntaxKind::EndKw,
            "if" => SyntaxKind::IfKw,
            "else" => SyntaxKind::ElseKw,
            "parameter" => SyntaxKind::ParameterKw,
            _ => SyntaxKind::Ident,
        };
        return (kind, n);
    }

    // Unknown -> error token, consume one byte
    (SyntaxKind::Error, 1)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lex_module_header() {
        let tokens = lex("module foo;");
        let kinds: Vec<_> = tokens.iter().map(|t| t.kind).collect();
        assert_eq!(
            kinds,
            vec![
                SyntaxKind::ModuleKw,
                SyntaxKind::Whitespace,
                SyntaxKind::Ident,
                SyntaxKind::Semicolon,
                SyntaxKind::Eof,
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
}
