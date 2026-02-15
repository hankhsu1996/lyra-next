use lyra_lexer::{SyntaxKind, lex};

/// Lex source into (kind, text) pairs, excluding Eof.
pub fn lex_kinds(src: &str) -> Vec<(SyntaxKind, &str)> {
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

/// Collect just the kinds (no text).
pub fn kinds(src: &str) -> Vec<SyntaxKind> {
    lex_kinds(src).into_iter().map(|(k, _)| k).collect()
}

/// Return the first non-whitespace token.
pub fn single(src: &str) -> (SyntaxKind, &str) {
    lex_kinds(src)
        .into_iter()
        .find(|(k, _)| *k != SyntaxKind::Whitespace)
        .expect("expected at least one non-whitespace token")
}
