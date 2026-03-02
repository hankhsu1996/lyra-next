use std::sync::Arc;

use lyra_lexer::{SyntaxKind, Token};
use lyra_source::{FileId, Span, TextRange, TextSize};
use smol_str::SmolStr;

use crate::directive::{DirectiveClass, DirectiveKeyword, classify_directive};
use crate::env::{MacroEnv, MacroTok, MacroTokenSeq, MacroValue};
use crate::source_map::{Segment, SegmentKind, SourceMap};
use crate::{
    DirectiveEvent, DirectiveEventKind, DirectiveEventOrigin, IncludeGraph, IncludeProvider,
    PreprocError, PreprocOutput, PreprocessInputs, find_include_path,
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
    macro_recursion_limit: usize,

    env: MacroEnv,
    cond_stack: Vec<CondFrame>,

    out_tokens: Vec<Token>,
    expanded_text: String,
    segments: Vec<Segment>,
    includes: IncludeGraph,
    errors: Vec<PreprocError>,
    directive_events: Vec<DirectiveEvent>,
    event_seq_counter: u32,

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
            macro_recursion_limit: inputs.macro_recursion_limit,
            env: inputs.starting_env.clone(),
            cond_stack: Vec::new(),
            out_tokens: Vec::with_capacity(inputs.tokens.len()),
            expanded_text: String::with_capacity(inputs.text.len()),
            segments: Vec::new(),
            includes: IncludeGraph::default(),
            errors: Vec::new(),
            directive_events: Vec::new(),
            event_seq_counter: 0,
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

    fn next_event_seq(&mut self) -> u32 {
        let seq = self.event_seq_counter;
        self.event_seq_counter += 1;
        seq
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
            _ => match classify_directive(directive_text) {
                DirectiveClass::MacroInvoke { name } => {
                    if self.currently_emitting() {
                        self.handle_macro_use(idx, name)
                    } else {
                        self.strip_macro_token(idx)
                    }
                }
                DirectiveClass::Keyword(kw) => self.handle_known_directive(idx, kw),
                DirectiveClass::Other { raw } => self.handle_unrecognized_directive(idx, raw),
            },
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

        let name = SmolStr::from(self.tok_text_at(j, cursor));
        cursor += usize::from(self.tokens[j].len);
        j += 1;

        // Skip non-newline trivia between name and value
        while j < self.tokens.len() && self.tokens[j].kind.is_trivia() {
            let t_text = self.tok_text_at(j, cursor);
            if token_carries_newline(self.tokens[j].kind, t_text) {
                break;
            }
            cursor += t_text.len();
            j += 1;
        }

        // Collect value tokens until newline or EOF
        let mut value_toks: Vec<MacroTok> = Vec::new();
        while j < self.tokens.len() {
            let t = self.tokens[j];
            if t.kind == SyntaxKind::Eof {
                break;
            }
            let t_text = self.tok_text_at(j, cursor);
            if token_carries_newline(t.kind, t_text) {
                break;
            }
            value_toks.push(MacroTok {
                token: t,
                text: SmolStr::from(t_text),
            });
            cursor += t_text.len();
            j += 1;
        }

        // Consume the directive line (flushing identity, advancing cursors)
        self.flush_identity();
        self.src_cursor = cursor;
        self.flush_start = cursor;
        let consumed = j - idx;

        if self.currently_emitting() {
            let value = if value_toks.is_empty() {
                MacroValue::Flag
            } else {
                MacroValue::ObjectLike(Arc::new(MacroTokenSeq::from_vec(value_toks)))
            };
            self.env.define(name, value);
        }

        consumed
    }

    fn handle_undef(&mut self, idx: usize) -> usize {
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
                        kind: SegmentKind::Include { call_site },
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

    fn handle_known_directive(&mut self, idx: usize, kw: DirectiveKeyword) -> usize {
        let dir_len: usize = self.tokens[idx].len.into();
        let dir_start = self.src_cursor;
        self.flush_identity();
        if self.currently_emitting() {
            let event_seq = self.next_event_seq();
            self.directive_events.push(DirectiveEvent {
                span: Span {
                    file: self.file,
                    range: TextRange::new(
                        TextSize::new(dir_start as u32),
                        TextSize::new((dir_start + dir_len) as u32),
                    ),
                },
                kind: DirectiveEventKind::KnownDirective(kw),
                origin: DirectiveEventOrigin::TopLevel,
                expanded_offset: TextSize::new(self.expanded_text.len() as u32),
                event_seq,
            });
        }
        self.strip_directive_line(idx)
    }

    fn handle_unrecognized_directive(&mut self, idx: usize, raw: &str) -> usize {
        let dir_len: usize = self.tokens[idx].len.into();
        let dir_start = self.src_cursor;
        self.flush_identity();
        if self.currently_emitting() {
            let event_seq = self.next_event_seq();
            self.directive_events.push(DirectiveEvent {
                span: Span {
                    file: self.file,
                    range: TextRange::new(
                        TextSize::new(dir_start as u32),
                        TextSize::new((dir_start + dir_len) as u32),
                    ),
                },
                kind: DirectiveEventKind::UnrecognizedDirective(SmolStr::from(raw)),
                origin: DirectiveEventOrigin::TopLevel,
                expanded_offset: TextSize::new(self.expanded_text.len() as u32),
                event_seq,
            });
        }
        self.strip_directive_line(idx)
    }

    fn handle_macro_use(&mut self, idx: usize, name: &str) -> usize {
        let dir_len: usize = self.tokens[idx].len.into();
        let call_site = Span {
            file: self.file,
            range: TextRange::new(
                TextSize::new(self.src_cursor as u32),
                TextSize::new((self.src_cursor + dir_len) as u32),
            ),
        };

        let value = self.env.get(name).map(|d| d.value.clone());

        match value {
            None => {
                self.flush_identity();
                let event_seq = self.next_event_seq();
                self.directive_events.push(DirectiveEvent {
                    span: call_site,
                    kind: DirectiveEventKind::UndefinedMacro(SmolStr::from(name)),
                    origin: DirectiveEventOrigin::TopLevel,
                    expanded_offset: TextSize::new(self.expanded_text.len() as u32),
                    event_seq,
                });
                self.src_cursor += dir_len;
                self.flush_start = self.src_cursor;
                1
            }
            Some(MacroValue::Flag) => {
                self.flush_identity();
                self.src_cursor += dir_len;
                self.flush_start = self.src_cursor;
                1
            }
            Some(MacroValue::ObjectLike(seq)) => self.expand_macro_tokens(idx, call_site, &seq),
        }
    }

    fn expand_macro_tokens(&mut self, _idx: usize, call_site: Span, seq: &MacroTokenSeq) -> usize {
        debug_assert!(
            self.currently_emitting(),
            "expand_macro_tokens called while not emitting",
        );
        self.flush_identity();
        let base_offset = TextSize::new(self.expanded_text.len() as u32);

        let mut tmp_tokens = Vec::new();
        let mut tmp_text = String::new();
        let mut sink = ExpansionSink {
            out_tokens: &mut tmp_tokens,
            out_text: &mut tmp_text,
            events: &mut self.directive_events,
            next_event_seq: &mut self.event_seq_counter,
        };
        let result = expand_seq_into(
            &self.env,
            call_site,
            seq,
            0,
            self.macro_recursion_limit,
            &mut sink,
            base_offset,
        );

        let dir_len = usize::from(call_site.range.len());

        if let Err(msg) = result {
            self.errors.push(PreprocError {
                span: call_site,
                message: msg,
            });
            self.src_cursor += dir_len;
            self.flush_start = self.src_cursor;
            return 1;
        }

        debug_assert!(
            !tmp_tokens.iter().any(|t| t.kind == SyntaxKind::Directive),
            "macro expansion must not produce Directive tokens",
        );
        debug_assert_eq!(
            tmp_tokens.iter().map(|t| usize::from(t.len)).sum::<usize>(),
            tmp_text.len(),
            "macro expansion token lengths must sum to text length",
        );

        if !tmp_text.is_empty() {
            let exp_start = TextSize::new(self.expanded_text.len() as u32);
            for t in tmp_tokens {
                self.out_tokens.push(t);
            }
            self.expanded_text.push_str(&tmp_text);
            let exp_end = TextSize::new(self.expanded_text.len() as u32);

            self.segments.push(Segment {
                expanded_range: TextRange::new(exp_start, exp_end),
                origin: call_site,
                kind: SegmentKind::Macro,
            });
        }

        self.src_cursor += dir_len;
        self.flush_start = self.src_cursor;
        1
    }

    fn strip_macro_token(&mut self, idx: usize) -> usize {
        let dir_len: usize = self.tokens[idx].len.into();
        self.flush_identity();
        self.src_cursor += dir_len;
        self.flush_start = self.src_cursor;
        1
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
    /// current logical directive line (newline-carrying token or EOF).
    /// Returns `(end_idx, end_cursor)` -- pure scan, no mutation.
    fn scan_to_line_end(&self, start_idx: usize, start_cursor: usize) -> (usize, usize) {
        let mut j = start_idx;
        let mut cursor = start_cursor;

        while j < self.tokens.len() {
            let t = self.tokens[j];
            if t.kind == SyntaxKind::Eof {
                break;
            }
            let t_len: usize = t.len.into();
            if token_carries_newline(t.kind, &self.text[cursor..cursor + t_len]) {
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

        while j < self.tokens.len() && self.tokens[j].kind.is_trivia() {
            let t_text = self.tok_text_at(j, cursor);
            if token_carries_newline(self.tokens[j].kind, t_text) {
                return None;
            }
            cursor += t_text.len();
            j += 1;
        }

        if self.has_non_ws_token_on_line(j, cursor) {
            let t_len: usize = self.tokens[j].len.into();
            Some(SmolStr::from(&self.text[cursor..cursor + t_len]))
        } else {
            None
        }
    }

    /// Slice the text for `self.tokens[idx]` at byte position `byte_pos`.
    fn tok_text_at(&self, idx: usize, byte_pos: usize) -> &str {
        let len: usize = self.tokens[idx].len.into();
        let slice = &self.text[byte_pos..byte_pos + len];
        debug_assert_eq!(
            slice.len(),
            len,
            "tok_text_at: slice length mismatch at idx={idx} byte_pos={byte_pos}",
        );
        slice
    }

    /// Check if token at index `j` (at byte position `cursor`) is a
    /// non-trivia, non-EOF token still on the current line.
    fn has_non_ws_token_on_line(&self, j: usize, cursor: usize) -> bool {
        if j >= self.tokens.len() {
            return false;
        }
        let t = self.tokens[j];
        if t.kind == SyntaxKind::Eof {
            return false;
        }
        if t.kind.is_trivia() {
            let t_len: usize = t.len.into();
            if token_carries_newline(t.kind, &self.text[cursor..cursor + t_len]) {
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
            kind: SegmentKind::Identity,
        });

        self.flush_start = self.src_cursor;
    }
}

/// Mutable accumulators for recursive macro expansion, bundled to
/// keep the `expand_seq_into` argument count manageable.
struct ExpansionSink<'a> {
    out_tokens: &'a mut Vec<Token>,
    out_text: &'a mut String,
    events: &'a mut Vec<DirectiveEvent>,
    next_event_seq: &'a mut u32,
}

/// Recursively expand a macro token sequence, splicing non-directive
/// tokens into the sink and stripping or recursing on directive tokens
/// found in the body.
///
/// Precondition: only called when the preprocessor is emitting (i.e.,
/// from `expand_macro_tokens` which is gated on `currently_emitting`).
/// All events produced here are therefore from emitted text.
fn expand_seq_into(
    env: &MacroEnv,
    call_site: Span,
    seq: &MacroTokenSeq,
    depth: usize,
    recursion_limit: usize,
    sink: &mut ExpansionSink<'_>,
    base_offset: TextSize,
) -> Result<(), SmolStr> {
    if depth > recursion_limit {
        return Err(SmolStr::from("macro expansion depth limit exceeded"));
    }

    for tok in seq.tokens() {
        if tok.token.kind == SyntaxKind::Directive {
            let offset = base_offset + TextSize::new(sink.out_text.len() as u32);
            match classify_directive(&tok.text) {
                DirectiveClass::MacroInvoke { name } => {
                    let value = env.get(name).map(|d| d.value.clone());
                    match value {
                        None => {
                            let event_seq = *sink.next_event_seq;
                            *sink.next_event_seq += 1;
                            sink.events.push(DirectiveEvent {
                                span: call_site,
                                kind: DirectiveEventKind::UndefinedMacro(SmolStr::from(name)),
                                origin: DirectiveEventOrigin::MacroExpansion,
                                expanded_offset: offset,
                                event_seq,
                            });
                        }
                        Some(MacroValue::Flag) => {}
                        Some(MacroValue::ObjectLike(inner_seq)) => {
                            expand_seq_into(
                                env,
                                call_site,
                                &inner_seq,
                                depth + 1,
                                recursion_limit,
                                sink,
                                base_offset,
                            )?;
                        }
                    }
                }
                DirectiveClass::Keyword(kw) => {
                    let event_seq = *sink.next_event_seq;
                    *sink.next_event_seq += 1;
                    sink.events.push(DirectiveEvent {
                        span: call_site,
                        kind: DirectiveEventKind::KnownDirective(kw),
                        origin: DirectiveEventOrigin::MacroExpansion,
                        expanded_offset: offset,
                        event_seq,
                    });
                }
                DirectiveClass::Other { raw } => {
                    let event_seq = *sink.next_event_seq;
                    *sink.next_event_seq += 1;
                    sink.events.push(DirectiveEvent {
                        span: call_site,
                        kind: DirectiveEventKind::UnrecognizedDirective(SmolStr::from(raw)),
                        origin: DirectiveEventOrigin::MacroExpansion,
                        expanded_offset: offset,
                        event_seq,
                    });
                }
            }
        } else {
            sink.out_tokens.push(tok.token);
            sink.out_text.push_str(&tok.text);
        }
    }

    Ok(())
}

/// Whether a token with the given kind and text carries a newline.
///
/// Covers all trivia kinds: `Whitespace` (always), `BlockComment`
/// (multi-line), and `LineComment` (currently stops before `\n` in
/// our lexer, but handled defensively). Non-trivia tokens never
/// carry newlines in well-formed source.
fn token_carries_newline(kind: SyntaxKind, text: &str) -> bool {
    kind.is_trivia() && text.contains('\n')
}
