//! Identifier semantic spelling vs source spelling.
//!
//! `SystemVerilog` has two identifier forms (LRM 5.6, 5.6.1):
//!
//! - **Plain identifiers**: `cpu3`, `bus_index` -- token text IS the semantic name.
//! - **Escaped identifiers**: `\cpu3 `, `\bus+index ` -- token text includes the
//!   leading backslash, but the **semantic name** is the text after stripping it.
//!   Per LRM 5.6.1, `\cpu3` and `cpu3` denote the same identifier.
//!
//! This module provides the canonical entry point for extracting the **semantic
//! spelling** of an identifier token -- the name used for symbol tables, scope
//! lookup, member matching, import/export keys, and any other semantic comparison.
//!
//! Source spelling (the exact token text including `\`) is preserved separately
//! via `SyntaxToken::text()` and is used for CST roundtrip, diagnostics rendering,
//! and source echo.
//!
//! Semantic-name producers must call `semantic_spelling` instead of raw
//! `SmolStr::new(tok.text())` for identifier tokens. The policy check
//! `tools/policy/check_semantic_spelling.py` enforces this.

use lyra_lexer::SyntaxKind;
use lyra_parser::SyntaxToken;
use smol_str::SmolStr;

/// Extract the semantic (canonical) spelling of an identifier token.
///
/// For `Ident` tokens, returns the token text unchanged.
/// For `EscapedIdent` tokens, strips the leading backslash.
///
/// Returns `None` if the token is not an identifier kind (`Ident` or
/// `EscapedIdent`). Callers at known-identifier sites should use
/// `semantic_spelling` for the infallible variant.
pub(crate) fn try_semantic_spelling(tok: &SyntaxToken) -> Option<SmolStr> {
    match tok.kind() {
        SyntaxKind::Ident => Some(SmolStr::new(tok.text())),
        SyntaxKind::EscapedIdent => {
            let text = tok.text();
            // Escaped identifiers always start with backslash per lexer contract.
            let stripped = &text[1..];
            Some(SmolStr::new(stripped))
        }
        _ => None,
    }
}

/// Extract the semantic spelling of a token that is known to be an identifier.
///
/// Equivalent to `try_semantic_spelling(tok).unwrap()` but with a clearer
/// panic message. Use this at sites where the caller has already matched
/// the token kind to `Ident` or `EscapedIdent`.
pub fn semantic_spelling(tok: &SyntaxToken) -> SmolStr {
    match try_semantic_spelling(tok) {
        Some(name) => name,
        None => unreachable!(
            "semantic_spelling called on non-identifier token {:?}",
            tok.kind()
        ),
    }
}
