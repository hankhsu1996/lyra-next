use lyra_lexer::{LexMode, SyntaxKind, Token};
use lyra_source::{Span, TextRange, TextSize};
use smol_str::SmolStr;

use crate::env::MacroTok;

/// Map an expansion-relative range to an origin span via the call site.
/// Both endpoints are clamped to the call site range.
pub(crate) fn expansion_span(call_site: Span, exp_range: TextRange) -> Span {
    let site_len = u32::from(call_site.range.len());
    let max = TextSize::new(site_len.saturating_sub(1));
    let start = call_site.range.start() + std::cmp::min(exp_range.start(), max);
    let end = call_site.range.start() + std::cmp::min(exp_range.end(), max);
    Span {
        file: call_site.file,
        range: TextRange::new(start, std::cmp::max(start, end)),
    }
}

/// Check whether a token sequence contains any macro operator tokens.
pub(crate) fn has_macro_operators(tokens: &[Token]) -> bool {
    tokens.iter().any(|t| {
        matches!(
            t.kind,
            SyntaxKind::MacroStringify
                | SyntaxKind::MacroTripleStringify
                | SyntaxKind::MacroConcat
                | SyntaxKind::MacroEscapedQuote
        )
    })
}

/// Errors produced by the macro operator resolver.
///
/// `exp_range` is the range of the error token in the pre-resolution
/// expanded text. The engine maps this through the call site to
/// produce an origin-space diagnostic span.
pub(crate) struct OperatorError {
    pub message: SmolStr,
    pub exp_range: TextRange,
}

/// Delimiter kind for the stringify operator.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StringifyDelimiter {
    Regular,
    Triple,
}

fn emit_stringified_literal(delimiter: StringifyDelimiter, buf: &str) -> MacroTok {
    let literal = match delimiter {
        StringifyDelimiter::Regular => format!("\"{buf}\""),
        StringifyDelimiter::Triple => format!("\"\"\"{buf}\"\"\""),
    };
    MacroTok {
        token: Token {
            kind: SyntaxKind::StringLiteral,
            len: TextSize::new(literal.len() as u32),
        },
        text: SmolStr::from(&literal),
    }
}

struct OperatorResolver {
    out: Vec<MacroTok>,
    errors: Vec<OperatorError>,
    active_stringify: Option<StringifyDelimiter>,
    stringify_buf: String,
    pending_concat: bool,
    byte_offset: u32,
    concat_range: TextRange,
}

impl OperatorResolver {
    fn new() -> Self {
        Self {
            out: Vec::new(),
            errors: Vec::new(),
            active_stringify: None,
            stringify_buf: String::new(),
            pending_concat: false,
            byte_offset: 0,
            concat_range: TextRange::empty(TextSize::new(0)),
        }
    }

    fn handle_stringify(&mut self, tok: &MacroTok) {
        let this_delim = if tok.token.kind == SyntaxKind::MacroTripleStringify {
            StringifyDelimiter::Triple
        } else {
            StringifyDelimiter::Regular
        };
        if let Some(delim) = self.active_stringify {
            if delim == this_delim {
                let lit_tok = emit_stringified_literal(delim, &self.stringify_buf);
                if self.pending_concat {
                    apply_concat(&mut self.out, lit_tok, &mut self.errors, self.concat_range);
                    self.pending_concat = false;
                } else {
                    self.out.push(lit_tok);
                }
                self.active_stringify = None;
            } else {
                self.stringify_buf.push_str(&tok.text);
            }
        } else {
            self.active_stringify = Some(this_delim);
            self.stringify_buf.clear();
        }
    }

    fn handle_escaped_quote(&mut self, tok_range: TextRange) {
        if self.active_stringify.is_some() {
            self.stringify_buf.push_str("\\\"");
        } else {
            self.errors.push(OperatorError {
                message: SmolStr::from(
                    "escaped quote operator `\\`\" used outside stringify context",
                ),
                exp_range: tok_range,
            });
        }
    }

    fn handle_concat(&mut self, tok_range: TextRange) {
        if self.active_stringify.is_some() {
            let trimmed_len = self.stringify_buf.trim_end().len();
            self.stringify_buf.truncate(trimmed_len);
        } else {
            while self.out.last().is_some_and(|t| t.token.kind.is_trivia()) {
                self.out.pop();
            }
        }
        self.concat_range = tok_range;
        self.pending_concat = true;
    }

    fn handle_content(&mut self, tok: &MacroTok) -> bool {
        if self.active_stringify.is_some() {
            if self.pending_concat {
                if tok.token.kind.is_trivia() {
                    return true;
                }
                self.stringify_buf.push_str(&tok.text);
                self.pending_concat = false;
            } else {
                self.stringify_buf.push_str(&tok.text);
            }
        } else if self.pending_concat {
            if tok.token.kind.is_trivia() {
                return true;
            }
            apply_concat(
                &mut self.out,
                tok.clone(),
                &mut self.errors,
                self.concat_range,
            );
            self.pending_concat = false;
        } else {
            self.out.push(tok.clone());
        }
        false
    }

    fn finalize(mut self) -> (Vec<MacroTok>, Vec<OperatorError>) {
        if let Some(delim) = self.active_stringify {
            let end_pos = TextSize::new(self.byte_offset);
            let label = match delim {
                StringifyDelimiter::Regular => "unterminated macro stringify",
                StringifyDelimiter::Triple => "unterminated macro triple-quote stringify",
            };
            self.errors.push(OperatorError {
                message: SmolStr::from(label),
                exp_range: TextRange::empty(end_pos),
            });
            self.out
                .push(emit_stringified_literal(delim, &self.stringify_buf));
        }
        if self.pending_concat {
            self.errors.push(OperatorError {
                message: SmolStr::from("token concatenation operator at end of macro body"),
                exp_range: self.concat_range,
            });
        }
        (self.out, self.errors)
    }
}

/// Resolve macro operators (stringify, concat, escaped quote) in a
/// post-expansion token stream. Returns the resolved tokens and text,
/// or accumulates diagnostics for malformed usage.
///
/// Operates on `MacroTok` (token + text pairs) so it can produce
/// correct output text without access to a source string.
pub(crate) fn resolve_macro_operators(tokens: &[MacroTok]) -> (Vec<MacroTok>, Vec<OperatorError>) {
    let mut r = OperatorResolver::new();
    for tok in tokens {
        let tok_len = u32::from(tok.token.len);
        let tok_range = TextRange::new(
            TextSize::new(r.byte_offset),
            TextSize::new(r.byte_offset + tok_len),
        );
        match tok.token.kind {
            SyntaxKind::MacroStringify | SyntaxKind::MacroTripleStringify => {
                r.handle_stringify(tok);
            }
            SyntaxKind::MacroEscapedQuote => r.handle_escaped_quote(tok_range),
            SyntaxKind::MacroConcat => r.handle_concat(tok_range),
            _ => {
                if r.handle_content(tok) {
                    r.byte_offset += tok_len;
                    continue;
                }
            }
        }
        r.byte_offset += tok_len;
    }
    r.finalize()
}

/// Apply token concatenation: pop trailing trivia and the last
/// non-trivia token from output, fuse it with the rhs token,
/// retokenize the fused string, and append.
///
/// Self-contained: handles its own trivia cleanup so concat
/// semantics are robust regardless of caller invariants.
fn apply_concat(
    out: &mut Vec<MacroTok>,
    rhs: MacroTok,
    errors: &mut Vec<OperatorError>,
    concat_range: TextRange,
) {
    // Defensively pop trailing trivia to find the real lhs.
    while out.last().is_some_and(|t| t.token.kind.is_trivia()) {
        out.pop();
    }

    let Some(lhs) = out.pop() else {
        errors.push(OperatorError {
            message: SmolStr::from("token concatenation operator with no left-hand operand"),
            exp_range: concat_range,
        });
        out.push(rhs);
        return;
    };

    let fused = format!("{}{}", lhs.text, rhs.text);
    let retokenized = lyra_lexer::lex_with_mode(&fused, LexMode::Preprocess);

    let non_trivia_count = retokenized
        .iter()
        .filter(|t| t.kind != SyntaxKind::Eof && !t.kind.is_trivia())
        .count();
    if non_trivia_count == 0 {
        errors.push(OperatorError {
            message: SmolStr::from(format!(
                "token concatenation of '{}' and '{}' produced no significant tokens",
                lhs.text, rhs.text,
            )),
            exp_range: concat_range,
        });
        return;
    }
    if non_trivia_count > 1 {
        errors.push(OperatorError {
            message: SmolStr::from(format!(
                "token concatenation of '{}' and '{}' produced {non_trivia_count} tokens",
                lhs.text, rhs.text,
            )),
            exp_range: concat_range,
        });
    }

    let mut cursor = 0usize;
    for t in &retokenized {
        if t.kind == SyntaxKind::Eof {
            break;
        }
        let t_len: usize = t.len.into();
        out.push(MacroTok {
            token: *t,
            text: SmolStr::from(&fused[cursor..cursor + t_len]),
        });
        cursor += t_len;
    }
}

/// Convert `(&[Token], &str)` to `Vec<MacroTok>` by slicing text
/// at token boundaries.
pub(crate) fn tokens_to_macro_toks(tokens: &[Token], text: &str) -> Vec<MacroTok> {
    let mut result = Vec::with_capacity(tokens.len());
    let mut cursor = 0usize;
    for t in tokens {
        let t_len: usize = t.len.into();
        result.push(MacroTok {
            token: *t,
            text: SmolStr::from(&text[cursor..cursor + t_len]),
        });
        cursor += t_len;
    }
    result
}

/// Remove preprocess-only tokens from a `(tokens, text)` pair,
/// rebuilding the text to match. Defensive repair if operator
/// resolution fails to eliminate all operators.
pub(crate) fn strip_preprocess_only(tokens: &mut Vec<Token>, text: &mut String) {
    let mut kept_tokens = Vec::new();
    let mut kept_text = String::new();
    let mut cursor = 0usize;
    for t in tokens.iter() {
        let len: usize = t.len.into();
        if !matches!(
            t.kind,
            SyntaxKind::MacroStringify
                | SyntaxKind::MacroTripleStringify
                | SyntaxKind::MacroConcat
                | SyntaxKind::MacroEscapedQuote
        ) {
            kept_tokens.push(*t);
            kept_text.push_str(&text[cursor..cursor + len]);
        }
        cursor += len;
    }
    *tokens = kept_tokens;
    *text = kept_text;
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_tok(kind: SyntaxKind, text: &str) -> MacroTok {
        MacroTok {
            token: Token {
                kind,
                len: TextSize::new(text.len() as u32),
            },
            text: SmolStr::from(text),
        }
    }

    #[test]
    fn has_operators_detects_stringify() {
        let tokens = vec![Token {
            kind: SyntaxKind::MacroStringify,
            len: TextSize::new(2),
        }];
        assert!(has_macro_operators(&tokens));
    }

    #[test]
    fn has_operators_negative() {
        let tokens = vec![Token {
            kind: SyntaxKind::Ident,
            len: TextSize::new(3),
        }];
        assert!(!has_macro_operators(&tokens));
    }

    #[test]
    fn stringify_basic() {
        let toks = vec![
            make_tok(SyntaxKind::MacroStringify, "`\""),
            make_tok(SyntaxKind::Ident, "hello"),
            make_tok(SyntaxKind::Whitespace, " "),
            make_tok(SyntaxKind::Ident, "world"),
            make_tok(SyntaxKind::MacroStringify, "`\""),
        ];
        let (out, errs) = resolve_macro_operators(&toks);
        assert!(errs.is_empty());
        assert_eq!(out.len(), 1);
        assert_eq!(out[0].text.as_str(), "\"hello world\"");
    }

    #[test]
    fn stringify_escaped_quote() {
        let toks = vec![
            make_tok(SyntaxKind::MacroStringify, "`\""),
            make_tok(SyntaxKind::Ident, "a"),
            make_tok(SyntaxKind::MacroEscapedQuote, "`\\`\""),
            make_tok(SyntaxKind::Ident, "b"),
            make_tok(SyntaxKind::MacroStringify, "`\""),
        ];
        let (out, errs) = resolve_macro_operators(&toks);
        assert!(errs.is_empty());
        assert_eq!(out.len(), 1);
        assert_eq!(out[0].text.as_str(), "\"a\\\"b\"");
    }

    #[test]
    fn concat_basic() {
        let toks = vec![
            make_tok(SyntaxKind::Ident, "clock"),
            make_tok(SyntaxKind::MacroConcat, "``"),
            make_tok(SyntaxKind::Ident, "_primary"),
        ];
        let (out, errs) = resolve_macro_operators(&toks);
        assert!(errs.is_empty());
        assert_eq!(out.len(), 1);
        assert_eq!(out[0].text.as_str(), "clock_primary");
    }

    #[test]
    fn concat_strips_whitespace() {
        let toks = vec![
            make_tok(SyntaxKind::Ident, "foo"),
            make_tok(SyntaxKind::Whitespace, " "),
            make_tok(SyntaxKind::MacroConcat, "``"),
            make_tok(SyntaxKind::Whitespace, " "),
            make_tok(SyntaxKind::Ident, "bar"),
        ];
        let (out, errs) = resolve_macro_operators(&toks);
        assert!(errs.is_empty());
        assert_eq!(out.len(), 1);
        assert_eq!(out[0].text.as_str(), "foobar");
    }

    #[test]
    fn escaped_quote_outside_stringify() {
        let toks = vec![make_tok(SyntaxKind::MacroEscapedQuote, "`\\`\"")];
        let (_, errs) = resolve_macro_operators(&toks);
        assert_eq!(errs.len(), 1);
        assert!(errs[0].message.contains("outside stringify"));
    }

    #[test]
    fn unterminated_stringify() {
        let toks = vec![
            make_tok(SyntaxKind::MacroStringify, "`\""),
            make_tok(SyntaxKind::Ident, "hello"),
        ];
        let (out, errs) = resolve_macro_operators(&toks);
        assert_eq!(errs.len(), 1);
        assert!(errs[0].message.contains("unterminated"));
        assert_eq!(out.len(), 1);
        assert_eq!(out[0].text.as_str(), "\"hello\"");
    }

    #[test]
    fn concat_missing_rhs() {
        let toks = vec![
            make_tok(SyntaxKind::Ident, "foo"),
            make_tok(SyntaxKind::MacroConcat, "``"),
        ];
        let (out, errs) = resolve_macro_operators(&toks);
        assert_eq!(errs.len(), 1);
        assert!(errs[0].message.contains("end of macro body"));
        assert_eq!(out.len(), 1);
        assert_eq!(out[0].text.as_str(), "foo");
    }

    #[test]
    fn concat_missing_lhs() {
        let toks = vec![
            make_tok(SyntaxKind::MacroConcat, "``"),
            make_tok(SyntaxKind::Ident, "bar"),
        ];
        let (out, errs) = resolve_macro_operators(&toks);
        assert_eq!(errs.len(), 1);
        assert!(errs[0].message.contains("no left-hand"));
        assert_eq!(out.len(), 1);
        assert_eq!(out[0].text.as_str(), "bar");
    }

    #[test]
    fn stringify_empty() {
        let toks = vec![
            make_tok(SyntaxKind::MacroStringify, "`\""),
            make_tok(SyntaxKind::MacroStringify, "`\""),
        ];
        let (out, errs) = resolve_macro_operators(&toks);
        assert!(errs.is_empty());
        assert_eq!(out.len(), 1);
        assert_eq!(out[0].text.as_str(), "\"\"");
    }

    #[test]
    fn concat_in_stringify() {
        let toks = vec![
            make_tok(SyntaxKind::MacroStringify, "`\""),
            make_tok(SyntaxKind::Ident, "my_"),
            make_tok(SyntaxKind::MacroConcat, "``"),
            make_tok(SyntaxKind::Ident, "field"),
            make_tok(SyntaxKind::MacroStringify, "`\""),
        ];
        let (out, errs) = resolve_macro_operators(&toks);
        assert!(errs.is_empty());
        assert_eq!(out.len(), 1);
        assert_eq!(out[0].text.as_str(), "\"my_field\"");
    }

    #[test]
    fn concat_zero_significant_tokens() {
        // "/" + "/" -> "//" (line comment = trivia) -> 0 significant tokens
        let toks = vec![
            make_tok(SyntaxKind::Slash, "/"),
            make_tok(SyntaxKind::MacroConcat, "``"),
            make_tok(SyntaxKind::Slash, "/"),
        ];
        let (out, errs) = resolve_macro_operators(&toks);
        assert_eq!(errs.len(), 1);
        assert!(errs[0].message.contains("no significant tokens"));
        assert!(out.is_empty());
    }

    #[test]
    fn concat_multi_token_diagnosed() {
        // "1" + "+2" -> "1", "+", "2" = 3 significant tokens
        let toks = vec![
            make_tok(SyntaxKind::IntLiteral, "1"),
            make_tok(SyntaxKind::MacroConcat, "``"),
            make_tok(SyntaxKind::Ident, "+2"),
        ];
        let (out, errs) = resolve_macro_operators(&toks);
        assert_eq!(errs.len(), 1);
        assert!(errs[0].message.contains("produced"));
        // All retokenized tokens are emitted (deterministic fallback)
        assert!(out.len() >= 2);
    }

    #[test]
    fn error_exp_range_tracks_position() {
        // Verify that exp_range points to the error token position
        let toks = vec![
            make_tok(SyntaxKind::Ident, "abc"),
            make_tok(SyntaxKind::MacroEscapedQuote, "`\\`\""),
        ];
        let (_, errs) = resolve_macro_operators(&toks);
        assert_eq!(errs.len(), 1);
        // "abc" is 3 bytes, so escaped quote starts at offset 3
        assert_eq!(u32::from(errs[0].exp_range.start()), 3);
        assert_eq!(u32::from(errs[0].exp_range.end()), 7);
    }

    #[test]
    fn expansion_span_maps_full_range() {
        use lyra_source::FileId;
        // Call site at bytes 10..20 in some file
        let call_site = Span {
            file: FileId(1),
            range: TextRange::new(TextSize::new(10), TextSize::new(20)),
        };
        // Expansion-relative range 2..6 should map to origin 12..16
        let exp_range = TextRange::new(TextSize::new(2), TextSize::new(6));
        let result = expansion_span(call_site, exp_range);
        assert_eq!(result.file, FileId(1));
        assert_eq!(u32::from(result.range.start()), 12);
        assert_eq!(u32::from(result.range.end()), 16);
    }

    #[test]
    fn expansion_span_clamps_to_call_site() {
        use lyra_source::FileId;
        let call_site = Span {
            file: FileId(1),
            range: TextRange::new(TextSize::new(10), TextSize::new(15)),
        };
        // Expansion range exceeds call site length (5 bytes)
        let exp_range = TextRange::new(TextSize::new(3), TextSize::new(20));
        let result = expansion_span(call_site, exp_range);
        assert_eq!(u32::from(result.range.start()), 13);
        // End clamped to max (site_len - 1 = 4) -> 10 + 4 = 14
        assert_eq!(u32::from(result.range.end()), 14);
    }
}
