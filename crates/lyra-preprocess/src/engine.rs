use lyra_lexer::{SyntaxKind, Token};
use lyra_source::{FileId, Span, TextRange, TextSize};
use smol_str::SmolStr;

use crate::env::{MacroEnv, MacroValue};
use crate::source_map::{Segment, SourceMap};
use crate::{
    DirectiveEvent, DirectiveEventKind, IncludeGraph, IncludeProvider, PreprocError, PreprocOutput,
    PreprocessInputs, find_include_path,
};

struct CondFrame {
    allow_emit: bool,
    taken: bool,
    saw_else: bool,
}

pub(crate) struct Preprocessor<'a> {
    file: FileId,
    tokens: &'a [Token],
    text: &'a str,
    provider: &'a dyn IncludeProvider,

    env: MacroEnv,
    cond_stack: Vec<CondFrame>,

    out_tokens: Vec<Token>,
    expanded_text: String,
    segments: Vec<Segment>,
    includes: IncludeGraph,
    errors: Vec<PreprocError>,
    directive_events: Vec<DirectiveEvent>,

    src_cursor: usize,
    flush_start: usize,
}

impl<'a> Preprocessor<'a> {
    pub(crate) fn new(inputs: &PreprocessInputs<'a>) -> Self {
        Self {
            file: inputs.file,
            tokens: inputs.tokens,
            text: inputs.text,
            provider: inputs.provider,
            env: inputs.starting_env.clone(),
            cond_stack: Vec::new(),
            out_tokens: Vec::with_capacity(inputs.tokens.len()),
            expanded_text: String::with_capacity(inputs.text.len()),
            segments: Vec::new(),
            includes: IncludeGraph::default(),
            errors: Vec::new(),
            directive_events: Vec::new(),
            src_cursor: 0,
            flush_start: 0,
        }
    }

    pub(crate) fn run(mut self) -> PreprocOutput {
        let mut i = 0;
        while i < self.tokens.len() {
            let tok = self.tokens[i];

            if tok.kind == SyntaxKind::Eof {
                self.flush_identity();
                self.out_tokens.push(tok);
                break;
            }

            let tok_len: usize = tok.len.into();
            let tok_text = &self.text[self.src_cursor..self.src_cursor + tok_len];

            if tok.kind == SyntaxKind::Directive {
                let advance = self.handle_directive(i, tok_text);
                i += advance;
                continue;
            }

            if self.currently_emitting() {
                self.out_tokens.push(tok);
            } else {
                self.flush_identity();
                self.flush_start = self.src_cursor + tok_len;
            }

            self.src_cursor += tok_len;
            i += 1;
        }

        if !self.cond_stack.is_empty() {
            let eof = TextSize::new(self.text.len() as u32);
            self.push_error(
                TextRange::empty(eof),
                SmolStr::from("unterminated conditional directive"),
            );
        }

        self.flush_identity();

        let expanded_len = TextSize::new(self.expanded_text.len() as u32);
        PreprocOutput {
            tokens: self.out_tokens,
            expanded_text: self.expanded_text,
            source_map: SourceMap::new(self.file, self.segments, expanded_len),
            includes: self.includes,
            errors: self.errors,
            final_env: self.env,
            directive_events: self.directive_events,
        }
    }

    fn currently_emitting(&self) -> bool {
        self.cond_stack.last().is_none_or(|f| f.allow_emit)
    }

    fn push_error(&mut self, range: TextRange, message: SmolStr) {
        self.errors.push(PreprocError {
            span: Span {
                file: self.file,
                range,
            },
            message,
        });
    }

    fn handle_directive(&mut self, idx: usize, directive_text: &str) -> usize {
        match directive_text {
            "`ifdef" => self.handle_ifdef(idx, false),
            "`ifndef" => self.handle_ifdef(idx, true),
            "`elsif" => self.handle_elsif(idx),
            "`else" => self.handle_else(idx),
            "`endif" => self.handle_endif(idx),
            "`define" => self.handle_define(idx),
            "`undef" => self.handle_undef(idx),
            "`include" => self.handle_include(idx),
            _ => self.handle_unknown(idx, directive_text),
        }
    }

    fn handle_ifdef(&mut self, idx: usize, invert: bool) -> usize {
        let dir_len: usize = self.tokens[idx].len.into();
        let dir_start = self.src_cursor;

        // Extract name before consuming the line
        let name = self.extract_directive_name(idx, self.src_cursor, dir_len);
        let consumed = self.strip_directive_line(idx);

        let predicate = if let Some(n) = &name {
            let defined = self.env.is_defined(n);
            if invert { !defined } else { defined }
        } else {
            let label = if invert { "`ifndef" } else { "`ifdef" };
            self.push_error(
                TextRange::new(
                    TextSize::new(dir_start as u32),
                    TextSize::new((dir_start + dir_len) as u32),
                ),
                SmolStr::from(format!("{label} missing macro name")),
            );
            false
        };

        let parent = self.currently_emitting();
        self.cond_stack.push(CondFrame {
            allow_emit: parent && predicate,
            taken: predicate,
            saw_else: false,
        });

        consumed
    }

    fn handle_elsif(&mut self, idx: usize) -> usize {
        let dir_len: usize = self.tokens[idx].len.into();
        let dir_start = self.src_cursor;

        let name = self.extract_directive_name(idx, self.src_cursor, dir_len);
        let consumed = self.strip_directive_line(idx);

        let dir_range = TextRange::new(
            TextSize::new(dir_start as u32),
            TextSize::new((dir_start + dir_len) as u32),
        );

        let Some((frame, parent_frames)) = self.cond_stack.split_last_mut() else {
            self.push_error(
                dir_range,
                SmolStr::from("`elsif without matching `ifdef/`ifndef"),
            );
            return consumed;
        };

        let parent_emit = match parent_frames.last() {
            Some(p) => p.allow_emit,
            None => true,
        };

        if frame.saw_else {
            self.push_error(dir_range, SmolStr::from("`elsif after `else"));
            return consumed;
        }

        let mut missing_name = false;
        let predicate = if let Some(n) = &name {
            self.env.is_defined(n)
        } else {
            missing_name = true;
            false
        };

        if frame.taken {
            frame.allow_emit = false;
        } else {
            frame.allow_emit = parent_emit && predicate;
            if predicate {
                frame.taken = true;
            }
        }

        if missing_name {
            self.push_error(dir_range, SmolStr::from("`elsif missing macro name"));
        }

        consumed
    }

    fn handle_else(&mut self, idx: usize) -> usize {
        let dir_len: usize = self.tokens[idx].len.into();
        let dir_start = self.src_cursor;
        let consumed = self.strip_directive_line(idx);

        let dir_range = TextRange::new(
            TextSize::new(dir_start as u32),
            TextSize::new((dir_start + dir_len) as u32),
        );

        let Some((frame, parent_frames)) = self.cond_stack.split_last_mut() else {
            self.push_error(
                dir_range,
                SmolStr::from("`else without matching `ifdef/`ifndef"),
            );
            return consumed;
        };

        let parent_emit = match parent_frames.last() {
            Some(p) => p.allow_emit,
            None => true,
        };

        if frame.saw_else {
            self.push_error(dir_range, SmolStr::from("duplicate `else"));
            return consumed;
        }

        frame.saw_else = true;
        frame.allow_emit = parent_emit && !frame.taken;

        consumed
    }

    fn handle_endif(&mut self, idx: usize) -> usize {
        let dir_len: usize = self.tokens[idx].len.into();
        let dir_start = self.src_cursor;
        let consumed = self.strip_directive_line(idx);

        if self.cond_stack.pop().is_none() {
            self.push_error(
                TextRange::new(
                    TextSize::new(dir_start as u32),
                    TextSize::new((dir_start + dir_len) as u32),
                ),
                SmolStr::from("`endif without matching `ifdef/`ifndef"),
            );
        }

        consumed
    }

    fn handle_define(&mut self, idx: usize) -> usize {
        let dir_len: usize = self.tokens[idx].len.into();
        let dir_start = self.src_cursor;
        let dir_cursor = self.src_cursor;

        let mut j = idx + 1;
        let mut cursor = dir_cursor + dir_len;

        // Skip non-newline whitespace
        while j < self.tokens.len() && self.tokens[j].kind == SyntaxKind::Whitespace {
            let ws_len: usize = self.tokens[j].len.into();
            if self.text[cursor..cursor + ws_len].contains('\n') {
                break;
            }
            cursor += ws_len;
            j += 1;
        }

        // Check for missing name
        if !self.has_non_ws_token_on_line(j, cursor) {
            let consumed = self.strip_directive_line(idx);
            if self.currently_emitting() {
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

        let name_tok = self.tokens[j];
        let name_len: usize = name_tok.len.into();
        let name = SmolStr::from(&self.text[cursor..cursor + name_len]);
        cursor += name_len;
        j += 1;

        // Skip one optional non-newline whitespace token
        if j < self.tokens.len() && self.tokens[j].kind == SyntaxKind::Whitespace {
            let ws_len: usize = self.tokens[j].len.into();
            if !self.text[cursor..cursor + ws_len].contains('\n') {
                cursor += ws_len;
                j += 1;
            }
        }

        // Collect value tokens until newline or EOF
        let mut value_parts: Vec<&str> = Vec::new();
        while j < self.tokens.len() {
            let t = self.tokens[j];
            if t.kind == SyntaxKind::Eof {
                break;
            }
            let t_len: usize = t.len.into();
            if t.kind == SyntaxKind::Whitespace && self.text[cursor..cursor + t_len].contains('\n')
            {
                break;
            }
            value_parts.push(&self.text[cursor..cursor + t_len]);
            cursor += t_len;
            j += 1;
        }

        // Now consume the directive line (flushing identity, advancing cursors)
        self.flush_identity();
        self.src_cursor = cursor;
        self.flush_start = cursor;
        let consumed = j - idx;

        if self.currently_emitting() {
            let value = if value_parts.is_empty() {
                MacroValue::Flag
            } else {
                let joined: String = value_parts.concat();
                MacroValue::ObjectLike(SmolStr::from(joined))
            };
            self.env.define(name, value);
        }

        consumed
    }

    fn handle_undef(&mut self, idx: usize) -> usize {
        let dir_len: usize = self.tokens[idx].len.into();
        let dir_start = self.src_cursor;
        let dir_cursor = self.src_cursor;

        let mut j = idx + 1;
        let mut cursor = dir_cursor + dir_len;

        // Skip non-newline whitespace
        while j < self.tokens.len() && self.tokens[j].kind == SyntaxKind::Whitespace {
            let ws_len: usize = self.tokens[j].len.into();
            if self.text[cursor..cursor + ws_len].contains('\n') {
                break;
            }
            cursor += ws_len;
            j += 1;
        }

        if !self.has_non_ws_token_on_line(j, cursor) {
            let consumed = self.strip_directive_line(idx);
            if self.currently_emitting() {
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

        // Consume the directive before modifying env (so src_cursor is
        // at the right place for flush_identity)
        let name_owned = SmolStr::from(name);
        cursor += name_len;
        let end_j = j + 1;

        self.flush_identity();
        self.src_cursor = cursor;
        self.flush_start = cursor;
        // Scan past any remaining tokens on the line (after name)
        // to handle `undef FOO extra` -- just consume the whole line
        let extra = self.scan_to_line_end(end_j, cursor);
        self.src_cursor = extra.1;
        self.flush_start = extra.1;
        let consumed = extra.0 - idx;

        if self.currently_emitting() {
            self.env.undef(&name_owned);
        }

        consumed
    }

    fn handle_include(&mut self, idx: usize) -> usize {
        if !self.currently_emitting() {
            return self.strip_directive_line(idx);
        }

        if let Some((inner_range, path_range, skip_count)) =
            find_include_path(self.tokens, idx, self.src_cursor)
        {
            let path = &self.text[usize::from(inner_range.start())..usize::from(inner_range.end())];
            if let Some(resolved) = self.provider.resolve(path) {
                self.flush_identity();

                let inc_start = TextSize::new(self.expanded_text.len() as u32);
                let inc_len = resolved.text.len();

                for inc_tok in resolved.tokens {
                    if inc_tok.kind == SyntaxKind::Eof {
                        break;
                    }
                    self.out_tokens.push(*inc_tok);
                }
                self.expanded_text.push_str(resolved.text);

                let inc_end = TextSize::new(self.expanded_text.len() as u32);

                let skip_bytes: usize = self.tokens[idx..idx + skip_count]
                    .iter()
                    .map(|t| usize::from(t.len))
                    .sum();
                let call_site = Span {
                    file: self.file,
                    range: TextRange::new(
                        TextSize::new(self.src_cursor as u32),
                        TextSize::new((self.src_cursor + skip_bytes) as u32),
                    ),
                };

                if inc_len > 0 {
                    self.segments.push(Segment {
                        expanded_range: TextRange::new(inc_start, inc_end),
                        origin: Span {
                            file: resolved.file_id,
                            range: TextRange::new(TextSize::new(0), TextSize::new(inc_len as u32)),
                        },
                        call_site,
                    });
                }

                self.includes.push(resolved.file_id);
                self.src_cursor += skip_bytes;
                self.flush_start = self.src_cursor;
                return skip_count;
            }
            // Unresolved include: error points at the string literal in origin space
            self.push_error(
                path_range,
                SmolStr::from(format!(
                    "unresolved include: \"{}\"",
                    &self.text[usize::from(inner_range.start())..usize::from(inner_range.end())]
                )),
            );
        }

        self.strip_directive_line(idx)
    }

    fn handle_unknown(&mut self, idx: usize, directive_text: &str) -> usize {
        let dir_len: usize = self.tokens[idx].len.into();
        let dir_start = self.src_cursor;
        if self.currently_emitting() {
            self.directive_events.push(DirectiveEvent {
                span: Span {
                    file: self.file,
                    range: TextRange::new(
                        TextSize::new(dir_start as u32),
                        TextSize::new((dir_start + dir_len) as u32),
                    ),
                },
                kind: DirectiveEventKind::Unknown(SmolStr::from(directive_text)),
            });
        }
        self.strip_directive_line(idx)
    }

    /// Strip a directive line from output: flush pending identity,
    /// advance past all tokens on this line. Returns token count consumed.
    fn strip_directive_line(&mut self, start_idx: usize) -> usize {
        self.flush_identity();
        let (end_idx, end_cursor) = self.scan_to_line_end(start_idx, self.src_cursor);
        self.src_cursor = end_cursor;
        self.flush_start = end_cursor;
        end_idx - start_idx
    }

    /// Scan forward from `(start_idx, start_cursor)` to the end of the
    /// current logical directive line (newline-containing whitespace or
    /// EOF). Returns `(end_idx, end_cursor)` -- pure scan, no mutation.
    fn scan_to_line_end(&self, start_idx: usize, start_cursor: usize) -> (usize, usize) {
        let mut j = start_idx;
        let mut cursor = start_cursor;

        while j < self.tokens.len() {
            let t = self.tokens[j];
            if t.kind == SyntaxKind::Eof {
                break;
            }
            let t_len: usize = t.len.into();
            if t.kind == SyntaxKind::Whitespace && self.text[cursor..cursor + t_len].contains('\n')
            {
                break;
            }
            cursor += t_len;
            j += 1;
        }

        (j, cursor)
    }

    /// Extract the first identifier-like token after the directive,
    /// skipping non-newline whitespace. `dir_cursor` is the byte
    /// position of the directive token in `text`.
    fn extract_directive_name(
        &self,
        dir_idx: usize,
        dir_cursor: usize,
        dir_len: usize,
    ) -> Option<SmolStr> {
        let mut j = dir_idx + 1;
        let mut cursor = dir_cursor + dir_len;

        while j < self.tokens.len() && self.tokens[j].kind == SyntaxKind::Whitespace {
            let ws_len: usize = self.tokens[j].len.into();
            if self.text[cursor..cursor + ws_len].contains('\n') {
                return None;
            }
            cursor += ws_len;
            j += 1;
        }

        if self.has_non_ws_token_on_line(j, cursor) {
            let t_len: usize = self.tokens[j].len.into();
            Some(SmolStr::from(&self.text[cursor..cursor + t_len]))
        } else {
            None
        }
    }

    /// Check if token at index `j` (at byte position `cursor`) is a
    /// non-whitespace, non-EOF token still on the current line.
    fn has_non_ws_token_on_line(&self, j: usize, cursor: usize) -> bool {
        if j >= self.tokens.len() {
            return false;
        }
        let t = self.tokens[j];
        if t.kind == SyntaxKind::Eof {
            return false;
        }
        if t.kind == SyntaxKind::Whitespace {
            let t_len: usize = t.len.into();
            if self.text[cursor..cursor + t_len].contains('\n') {
                return false;
            }
        }
        true
    }

    /// Flush pending identity bytes from the primary file into the
    /// expanded output, recording an identity segment.
    fn flush_identity(&mut self) {
        let pending = &self.text[self.flush_start..self.src_cursor];
        if pending.is_empty() {
            return;
        }

        let exp_start = TextSize::new(self.expanded_text.len() as u32);
        self.expanded_text.push_str(pending);
        let exp_end = TextSize::new(self.expanded_text.len() as u32);

        let origin_start = TextSize::new(self.flush_start as u32);
        let origin_end = TextSize::new(self.src_cursor as u32);

        self.segments.push(Segment {
            expanded_range: TextRange::new(exp_start, exp_end),
            origin: Span {
                file: self.file,
                range: TextRange::new(origin_start, origin_end),
            },
            call_site: Span {
                file: self.file,
                range: TextRange::empty(TextSize::new(0)),
            },
        });

        self.flush_start = self.src_cursor;
    }
}
