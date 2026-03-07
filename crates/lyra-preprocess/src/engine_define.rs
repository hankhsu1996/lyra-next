use std::collections::HashSet;
use std::sync::Arc;

use lyra_lexer::{SyntaxKind, Token};
use lyra_source::{TextRange, TextSize};
use smol_str::SmolStr;

use crate::engine::{Preprocessor, token_carries_newline};
use crate::env::{MacroTemplate, MacroTok, MacroTokenSeq, MacroValue};

enum DefineParamsResult {
    Ok {
        param_names: Vec<SmolStr>,
        end_j: usize,
        end_cursor: usize,
    },
    Error {
        message: SmolStr,
        range: TextRange,
        end_j: usize,
        end_cursor: usize,
    },
}

impl Preprocessor<'_> {
    pub(crate) fn handle_define(&mut self, idx: usize) -> usize {
        let dir_len: usize = self.tokens[idx].len.into();
        let dir_start = self.src_cursor;

        let mut j = idx + 1;
        let mut cursor = self.src_cursor + dir_len;

        // Skip non-newline trivia (whitespace and comments)
        while j < self.tokens.len() && self.tokens[j].kind.is_trivia() {
            let t_text = self.tok_text_at(j, cursor);
            if token_carries_newline(self.tokens[j].kind, t_text) {
                break;
            }
            cursor += t_text.len();
            j += 1;
        }

        // Check for missing name
        if !self.has_non_ws_token_on_line(j, cursor) {
            let consumed = self.strip_directive_line(idx);
            if self.cond.currently_emitting() {
                self.push_error(
                    TextRange::new(
                        TextSize::new(dir_start as u32),
                        TextSize::new((dir_start + dir_len) as u32),
                    ),
                    SmolStr::from("`define missing macro name"),
                );
            }
            return consumed;
        }

        let name = SmolStr::from(self.tok_text_at(j, cursor));
        cursor += usize::from(self.tokens[j].len);
        j += 1;

        // Adjacency: immediate LParen (no trivia skip) means function-like.
        let mut params: Option<Vec<SmolStr>> = None;
        if j < self.tokens.len() && self.tokens[j].kind == SyntaxKind::LParen {
            match self.parse_define_params(j, cursor) {
                DefineParamsResult::Ok {
                    param_names,
                    end_j,
                    end_cursor,
                } => {
                    params = Some(param_names);
                    j = end_j;
                    cursor = end_cursor;
                }
                DefineParamsResult::Error {
                    message,
                    range,
                    end_j,
                    end_cursor,
                } => {
                    self.flush_identity();
                    self.src_cursor = end_cursor;
                    self.flush_start = end_cursor;
                    if self.cond.currently_emitting() {
                        self.push_error(range, message);
                    }
                    return end_j - idx;
                }
            }
        }

        let (value_toks, body_end_j, body_end_cursor) = self.collect_define_body(j, cursor);

        // Consume the directive line (flushing identity, advancing cursors)
        self.flush_identity();
        self.src_cursor = body_end_cursor;
        self.flush_start = body_end_cursor;
        let consumed = body_end_j - idx;

        if self.cond.currently_emitting() {
            let value = match params {
                Some(param_names) => {
                    let body = MacroTokenSeq::from_vec(value_toks);
                    let template = MacroTemplate::compile(&body, &param_names);
                    MacroValue::FunctionLike {
                        params: param_names,
                        body: Arc::new(template),
                    }
                }
                None => {
                    if value_toks.is_empty() {
                        MacroValue::Flag
                    } else {
                        MacroValue::ObjectLike(Arc::new(MacroTokenSeq::from_vec(value_toks)))
                    }
                }
            };
            self.env.define(name, value);
        }

        consumed
    }

    pub(crate) fn handle_undef(&mut self, idx: usize) -> usize {
        let dir_len: usize = self.tokens[idx].len.into();
        let dir_start = self.src_cursor;

        let mut j = idx + 1;
        let mut cursor = self.src_cursor + dir_len;

        // Skip non-newline trivia (whitespace and comments)
        while j < self.tokens.len() && self.tokens[j].kind.is_trivia() {
            let t_text = self.tok_text_at(j, cursor);
            if token_carries_newline(self.tokens[j].kind, t_text) {
                break;
            }
            cursor += t_text.len();
            j += 1;
        }

        if !self.has_non_ws_token_on_line(j, cursor) {
            let consumed = self.strip_directive_line(idx);
            if self.cond.currently_emitting() {
                self.push_error(
                    TextRange::new(
                        TextSize::new(dir_start as u32),
                        TextSize::new((dir_start + dir_len) as u32),
                    ),
                    SmolStr::from("`undef missing macro name"),
                );
            }
            return consumed;
        }

        let name_len: usize = self.tokens[j].len.into();
        let name = &self.text[cursor..cursor + name_len];

        // Consume directive before modifying env (src_cursor must be current for flush)
        let name_owned = SmolStr::from(name);
        cursor += name_len;
        let end_j = j + 1;

        self.flush_identity();
        self.src_cursor = cursor;
        self.flush_start = cursor;
        // Scan past remaining tokens on line (`undef FOO extra`)
        let extra = self.scan_to_line_end(end_j, cursor);
        self.src_cursor = extra.1;
        self.flush_start = extra.1;
        let consumed = extra.0 - idx;

        if self.cond.currently_emitting() {
            self.env.undef(&name_owned);
        }

        consumed
    }

    /// Skip trivia and collect body tokens until newline or EOF.
    /// Handles backslash-newline line continuations (LRM 5.6.4).
    ///
    /// Token-opacity invariant: only trivia tokens (`is_trivia()`) are
    /// inspected for newline characters. Non-trivia tokens such as
    /// `StringLiteral` (including triple-quoted literals that embed
    /// newlines) are collected verbatim without examining their content.
    /// This ensures triple-quoted string literals (LRM 22.5.1) do not
    /// prematurely terminate a macro body.
    ///
    /// Returns `(body_tokens, end_token_idx, end_byte_cursor)`.
    fn collect_define_body(
        &self,
        start_j: usize,
        start_cursor: usize,
    ) -> (Vec<MacroTok>, usize, usize) {
        let mut j = start_j;
        let mut cursor = start_cursor;

        while j < self.tokens.len() && self.tokens[j].kind.is_trivia() {
            let t_text = self.tok_text_at(j, cursor);
            if token_carries_newline(self.tokens[j].kind, t_text) {
                break;
            }
            cursor += t_text.len();
            j += 1;
        }

        let mut toks: Vec<MacroTok> = Vec::new();
        while j < self.tokens.len() {
            let t = self.tokens[j];
            if t.kind == SyntaxKind::Eof {
                break;
            }
            let t_text = self.tok_text_at(j, cursor);
            if token_carries_newline(t.kind, t_text) {
                if self.is_line_continuation(cursor, t_text) {
                    // Pop the trailing backslash token from body
                    if let Some(last) = toks.last()
                        && last.token.kind == SyntaxKind::EscapedIdent
                        && last.text == "\\"
                    {
                        toks.pop();
                    }
                    // Keep post-newline indentation as body content
                    let remaining = text_after_first_newline(t_text);
                    if !remaining.is_empty() {
                        if remaining.contains('\n') {
                            break;
                        }
                        toks.push(MacroTok {
                            token: Token {
                                kind: SyntaxKind::Whitespace,
                                len: (remaining.len() as u32).into(),
                            },
                            text: SmolStr::from(remaining),
                        });
                    }
                    cursor += t_text.len();
                    j += 1;
                    continue;
                }
                break;
            }
            toks.push(MacroTok {
                token: t,
                text: SmolStr::from(t_text),
            });
            cursor += t_text.len();
            j += 1;
        }

        (toks, j, cursor)
    }

    /// Parse a function-like macro parameter list starting at an `LParen`
    /// token. Returns the parameter names and position after the closing
    /// `RParen`, or an error with diagnostic info.
    fn parse_define_params(&self, lparen_idx: usize, lparen_cursor: usize) -> DefineParamsResult {
        let lparen_len: usize = self.tokens[lparen_idx].len.into();
        let lparen_range = TextRange::new(
            TextSize::new(lparen_cursor as u32),
            TextSize::new((lparen_cursor + lparen_len) as u32),
        );
        let mut params: Vec<SmolStr> = Vec::new();
        let mut seen: HashSet<SmolStr> = HashSet::new();
        let mut j = lparen_idx + 1;
        let mut cursor = lparen_cursor + lparen_len;
        let mut want_param = true;

        loop {
            while j < self.tokens.len() && self.tokens[j].kind.is_trivia() {
                let t_text = self.tok_text_at(j, cursor);
                if token_carries_newline(self.tokens[j].kind, t_text) {
                    if self.is_line_continuation(cursor, t_text) {
                        cursor += t_text.len();
                        j += 1;
                        continue;
                    }
                    let (end_j, end_cursor) = self.scan_to_line_end(j, cursor);
                    return unterminated_params(lparen_range, end_j, end_cursor);
                }
                cursor += t_text.len();
                j += 1;
            }

            if j >= self.tokens.len() || self.tokens[j].kind == SyntaxKind::Eof {
                return unterminated_params(lparen_range, j, cursor);
            }

            // Skip continuation backslash before next newline trivia
            let t = self.tokens[j];
            let t_len: usize = t.len.into();
            let t_text = self.tok_text_at(j, cursor);
            if t.kind == SyntaxKind::EscapedIdent && t_text == "\\" {
                let next_cursor = cursor + t_len;
                if j + 1 < self.tokens.len() {
                    let next_text = self.tok_text_at(j + 1, next_cursor);
                    if token_carries_newline(self.tokens[j + 1].kind, next_text) {
                        cursor = next_cursor + next_text.len();
                        j += 2;
                        continue;
                    }
                }
            }

            if t.kind == SyntaxKind::RParen {
                cursor += t_len;
                j += 1;
                return DefineParamsResult::Ok {
                    param_names: params,
                    end_j: j,
                    end_cursor: cursor,
                };
            }

            if want_param {
                if t.kind != SyntaxKind::Ident {
                    return self.define_param_error(
                        format!("expected parameter name, found '{t_text}'"),
                        cursor,
                        t_len,
                        lparen_idx,
                        lparen_cursor,
                    );
                }
                let name = SmolStr::from(t_text);
                if !seen.insert(name.clone()) {
                    return self.define_param_error(
                        format!("duplicate parameter name '{name}' in macro definition"),
                        cursor,
                        t_len,
                        lparen_idx,
                        lparen_cursor,
                    );
                }
                params.push(name);
                cursor += t_len;
                j += 1;
                want_param = false;
            } else if t.kind != SyntaxKind::Comma {
                return self.define_param_error(
                    format!("expected ',' or ')' after parameter, found '{t_text}'"),
                    cursor,
                    t_len,
                    lparen_idx,
                    lparen_cursor,
                );
            } else {
                cursor += t_len;
                j += 1;
                want_param = true;
            }
        }
    }

    fn define_param_error(
        &self,
        msg: String,
        cursor: usize,
        t_len: usize,
        lparen_idx: usize,
        lparen_cursor: usize,
    ) -> DefineParamsResult {
        let (end_j, end_cursor) = self.scan_to_line_end(lparen_idx, lparen_cursor);
        DefineParamsResult::Error {
            message: SmolStr::from(msg),
            range: TextRange::new(
                TextSize::new(cursor as u32),
                TextSize::new((cursor + t_len) as u32),
            ),
            end_j,
            end_cursor,
        }
    }
}

fn unterminated_params(
    lparen_range: TextRange,
    end_j: usize,
    end_cursor: usize,
) -> DefineParamsResult {
    DefineParamsResult::Error {
        message: SmolStr::from("unterminated parameter list in macro definition"),
        range: lparen_range,
        end_j,
        end_cursor,
    }
}

/// Return the portion of `text` after the first `\n`.
/// For `\r\n` sequences the `\r` precedes `\n`, so slicing after `\n`
/// correctly skips the entire line ending.
/// If `text` contains no newline, returns the entire string.
fn text_after_first_newline(text: &str) -> &str {
    let bytes = text.as_bytes();
    for i in 0..bytes.len() {
        if bytes[i] == b'\n' {
            return &text[i + 1..];
        }
    }
    text
}
