use lyra_lexer::{SyntaxKind, Token};
use lyra_source::{FileId, Span, TextRange, TextSize};
use smol_str::SmolStr;

use crate::directive::{DirectiveClass, DirectiveKeyword, classify_directive};
use crate::engine_cond::{CondState, ElseError, ElsifError};
use crate::env::MacroEnv;
use crate::predefined::{LineMap, classify_predefined};
use crate::source_map::{Segment, SegmentKind, SourceMap};
use crate::{
    DefaultNettypeDirective, DirectiveEvent, DirectiveEventKind, DirectiveEventOrigin,
    IncludeGraph, IncludeProvider, PreprocError, PreprocOutput, PreprocessInputs,
    TimescaleDirective, find_include_path,
};

pub(crate) struct Preprocessor<'a> {
    pub(crate) file: FileId,
    pub(crate) tokens: &'a [Token],
    pub(crate) text: &'a str,
    pub(crate) file_path: &'a str,
    pub(crate) line_map: LineMap,
    pub(crate) provider: &'a dyn IncludeProvider,
    pub(crate) macro_recursion_limit: usize,

    pub(crate) env: MacroEnv,
    pub(crate) cond: CondState,

    pub(crate) out_tokens: Vec<Token>,
    pub(crate) expanded_text: String,
    pub(crate) segments: Vec<Segment>,
    pub(crate) includes: IncludeGraph,
    pub(crate) errors: Vec<PreprocError>,
    pub(crate) directive_events: Vec<DirectiveEvent>,
    pub(crate) event_seq_counter: u32,

    pub(crate) src_cursor: usize,
    pub(crate) flush_start: usize,
}

impl<'a> Preprocessor<'a> {
    pub(crate) fn new(inputs: &PreprocessInputs<'a>) -> Self {
        Self {
            file: inputs.file,
            tokens: inputs.tokens,
            text: inputs.text,
            file_path: inputs.file_path,
            line_map: LineMap::build(inputs.text),
            provider: inputs.provider,
            macro_recursion_limit: inputs.macro_recursion_limit,
            env: inputs.starting_env.clone(),
            cond: CondState::new(),
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

            if self.cond.currently_emitting() {
                self.out_tokens.push(tok);
            } else {
                self.flush_identity();
                self.flush_start = self.src_cursor + tok_len;
            }

            self.src_cursor += tok_len;
            i += 1;
        }

        if !self.cond.is_empty() {
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

    pub(crate) fn next_event_seq(&mut self) -> u32 {
        let seq = self.event_seq_counter;
        self.event_seq_counter += 1;
        seq
    }

    pub(crate) fn push_error(&mut self, range: TextRange, message: SmolStr) {
        self.errors.push(PreprocError {
            span: Span {
                file: self.file,
                range,
            },
            message,
        });
    }

    /// Whether a macro name is defined: built-in predefined macros
    /// are always defined, then user macros from `MacroEnv`.
    pub(crate) fn is_name_defined(&self, name: &str) -> bool {
        classify_predefined(name).is_some() || self.env.is_defined(name)
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
            "`timescale" => self.handle_timescale(idx),
            "`default_nettype" => self.handle_default_nettype(idx),
            _ => match classify_directive(directive_text) {
                DirectiveClass::MacroInvoke { name } => {
                    if self.cond.currently_emitting() {
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

    fn directive_range_and_len(&self, idx: usize) -> (TextRange, usize) {
        let dir_len: usize = self.tokens[idx].len.into();
        let range = TextRange::new(
            TextSize::new(self.src_cursor as u32),
            TextSize::new((self.src_cursor + dir_len) as u32),
        );
        (range, dir_len)
    }

    fn handle_ifdef(&mut self, idx: usize, invert: bool) -> usize {
        let (dir_range, dir_len) = self.directive_range_and_len(idx);
        let name = self.extract_directive_name(idx, self.src_cursor, dir_len);
        let consumed = self.strip_directive_line(idx);
        let predicate = if let Some(n) = &name {
            let d = self.is_name_defined(n);
            if invert { !d } else { d }
        } else {
            let label = if invert { "`ifndef" } else { "`ifdef" };
            self.push_error(
                dir_range,
                SmolStr::from(format!("{label} missing macro name")),
            );
            false
        };
        self.cond.push_ifdef(predicate);
        consumed
    }

    fn handle_elsif(&mut self, idx: usize) -> usize {
        let (dir_range, dir_len) = self.directive_range_and_len(idx);
        let name = self.extract_directive_name(idx, self.src_cursor, dir_len);
        let consumed = self.strip_directive_line(idx);
        let predicate = name.as_deref().map(|n| self.is_name_defined(n));
        match self.cond.apply_elsif(predicate) {
            Ok(()) => {
                if name.is_none() {
                    self.push_error(dir_range, SmolStr::from("`elsif missing macro name"));
                }
            }
            Err(ElsifError::NoMatchingIfdef) => {
                self.push_error(
                    dir_range,
                    SmolStr::from("`elsif without matching `ifdef/`ifndef"),
                );
            }
            Err(ElsifError::ElsifAfterElse) => {
                self.push_error(dir_range, SmolStr::from("`elsif after `else"));
            }
        }
        consumed
    }

    fn handle_else(&mut self, idx: usize) -> usize {
        let (dir_range, _) = self.directive_range_and_len(idx);
        let consumed = self.strip_directive_line(idx);
        match self.cond.apply_else() {
            Ok(()) => {}
            Err(ElseError::NoMatchingIfdef) => {
                self.push_error(
                    dir_range,
                    SmolStr::from("`else without matching `ifdef/`ifndef"),
                );
            }
            Err(ElseError::DuplicateElse) => {
                self.push_error(dir_range, SmolStr::from("duplicate `else"));
            }
        }
        consumed
    }

    fn handle_endif(&mut self, idx: usize) -> usize {
        let (dir_range, _) = self.directive_range_and_len(idx);
        let consumed = self.strip_directive_line(idx);
        if !self.cond.pop_endif() {
            self.push_error(
                dir_range,
                SmolStr::from("`endif without matching `ifdef/`ifndef"),
            );
        }
        consumed
    }

    fn handle_include(&mut self, idx: usize) -> usize {
        if !self.cond.currently_emitting() {
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

    fn handle_timescale(&mut self, idx: usize) -> usize {
        if !self.cond.currently_emitting() {
            return self.strip_directive_line(idx);
        }

        let dir_len: usize = self.tokens[idx].len.into();
        let dir_start = self.src_cursor;
        self.flush_identity();

        let (end_idx, end_cursor) = self.scan_to_line_end(idx, self.src_cursor);

        let expanded_offset = TextSize::new(self.expanded_text.len() as u32);
        let dir_range = TextRange::new(
            TextSize::new(dir_start as u32),
            TextSize::new((dir_start + dir_len) as u32),
        );

        let parsed = self.parse_timescale_tokens(idx, dir_start, end_cursor);

        let event_kind = match parsed {
            Ok(ts) => DirectiveEventKind::Timescale(ts),
            Err(msg) => {
                let full_range = TextRange::new(
                    TextSize::new(dir_start as u32),
                    TextSize::new(end_cursor as u32),
                );
                self.push_error(full_range, SmolStr::from(msg));
                DirectiveEventKind::KnownDirective(DirectiveKeyword::Timescale)
            }
        };

        let event_seq = self.next_event_seq();
        self.directive_events.push(DirectiveEvent {
            span: Span {
                file: self.file,
                range: dir_range,
            },
            kind: event_kind,
            origin: DirectiveEventOrigin::TopLevel,
            expanded_offset,
            event_seq,
        });

        self.src_cursor = end_cursor;
        self.flush_start = end_cursor;
        end_idx - idx
    }

    /// Parse `` `timescale `` payload directly from the token stream.
    ///
    /// Expects: `<time_value> / <time_value>` where each time value is
    /// either a single `TimeLiteral` token (e.g. `1ns`) or an
    /// `IntLiteral` + `Ident` pair (e.g. `1` `ns`). Only validates
    /// structural shape; semantic validation is deferred to
    /// `TimeScaleValue::parse()` in the DB summary query.
    fn parse_timescale_tokens(
        &self,
        dir_idx: usize,
        dir_start: usize,
        line_end_cursor: usize,
    ) -> Result<TimescaleDirective, &'static str> {
        let dir_len: usize = self.tokens[dir_idx].len.into();
        let mut j = dir_idx + 1;
        let mut cursor = self.src_cursor + dir_len;

        let skip_trivia = |j: &mut usize, cursor: &mut usize| {
            while *j < self.tokens.len() {
                let t = self.tokens[*j];
                if t.kind == SyntaxKind::Eof {
                    break;
                }
                let t_len: usize = t.len.into();
                if !t.kind.is_trivia() {
                    break;
                }
                *cursor += t_len;
                *j += 1;
            }
        };

        let at_end = |j: usize, cursor: usize| -> bool {
            j >= self.tokens.len()
                || self.tokens[j].kind == SyntaxKind::Eof
                || cursor >= line_end_cursor
        };

        skip_trivia(&mut j, &mut cursor);
        if at_end(j, cursor) {
            return Err("`timescale directive missing time unit and precision");
        }

        let unit_val =
            self.consume_time_value(&mut j, &mut cursor, &skip_trivia, line_end_cursor)?;

        skip_trivia(&mut j, &mut cursor);
        if at_end(j, cursor) {
            return Err("`timescale directive missing '/' separator");
        }
        if self.tokens[j].kind != SyntaxKind::Slash {
            return Err("`timescale directive missing '/' separator");
        }
        let sep_len: usize = self.tokens[j].len.into();
        cursor += sep_len;
        j += 1;

        skip_trivia(&mut j, &mut cursor);
        if at_end(j, cursor) {
            return Err("`timescale directive missing time precision");
        }

        let prec_val =
            self.consume_time_value(&mut j, &mut cursor, &skip_trivia, line_end_cursor)?;

        let full_span = Span {
            file: self.file,
            range: TextRange::new(
                TextSize::new(dir_start as u32),
                TextSize::new(prec_val.end as u32),
            ),
        };

        Ok(TimescaleDirective {
            unit_text: unit_val.text,
            precision_text: prec_val.text,
            full_span,
        })
    }

    /// Consume a time value from the token stream. Accepts either a
    /// single `TimeLiteral` token or an `IntLiteral` + `Ident` pair
    /// (magnitude + unit suffix). Returns a `ParsedTimeValue` with
    /// canonical token-composed text (no trivia) and the byte offset
    /// after the last consumed token.
    fn consume_time_value(
        &self,
        j: &mut usize,
        cursor: &mut usize,
        skip_trivia: &dyn Fn(&mut usize, &mut usize),
        line_end_cursor: usize,
    ) -> Result<ParsedTimeValue, &'static str> {
        let tok = self.tokens[*j];
        let tok_len: usize = tok.len.into();

        if tok.kind == SyntaxKind::TimeLiteral {
            let text = SmolStr::from(&self.text[*cursor..*cursor + tok_len]);
            let end = *cursor + tok_len;
            *cursor = end;
            *j += 1;
            return Ok(ParsedTimeValue { text, end });
        }

        if tok.kind == SyntaxKind::IntLiteral {
            let mag_text = &self.text[*cursor..*cursor + tok_len];
            *cursor += tok_len;
            *j += 1;

            skip_trivia(j, cursor);
            if *j >= self.tokens.len()
                || self.tokens[*j].kind == SyntaxKind::Eof
                || *cursor >= line_end_cursor
            {
                return Err("`timescale time value missing unit suffix");
            }
            let suffix_tok = self.tokens[*j];
            if suffix_tok.kind == SyntaxKind::Ident {
                let suffix_len: usize = suffix_tok.len.into();
                let suffix_text = &self.text[*cursor..*cursor + suffix_len];
                let end = *cursor + suffix_len;
                let mut buf = String::with_capacity(mag_text.len() + suffix_text.len());
                buf.push_str(mag_text);
                buf.push_str(suffix_text);
                let canonical = SmolStr::from(buf);
                *cursor = end;
                *j += 1;
                return Ok(ParsedTimeValue {
                    text: canonical,
                    end,
                });
            }
            return Err("`timescale time value missing unit suffix");
        }

        Err("`timescale expected time literal (e.g. 1ns)")
    }

    /// Skip trivia tokens on the current line, stopping at newline or EOF.
    /// Returns the updated `(token_index, byte_cursor)`.
    fn skip_line_trivia(&self, mut j: usize, mut cursor: usize) -> (usize, usize) {
        while j < self.tokens.len() {
            let t = self.tokens[j];
            if t.kind == SyntaxKind::Eof {
                break;
            }
            let t_len: usize = t.len.into();
            if !t.kind.is_trivia() {
                break;
            }
            let t_text = &self.text[cursor..cursor + t_len];
            if token_carries_newline(t.kind, t_text) {
                break;
            }
            cursor += t_len;
            j += 1;
        }
        (j, cursor)
    }

    /// Whether token at `(j, cursor)` is a significant (non-trivia) token
    /// before the line end boundary.
    fn is_significant_before(&self, j: usize, cursor: usize, line_end: usize) -> bool {
        j < self.tokens.len()
            && self.tokens[j].kind != SyntaxKind::Eof
            && cursor < line_end
            && !self.tokens[j].kind.is_trivia()
    }

    fn handle_default_nettype(&mut self, idx: usize) -> usize {
        if !self.cond.currently_emitting() {
            return self.strip_directive_line(idx);
        }

        let dir_len: usize = self.tokens[idx].len.into();
        let dir_start = self.src_cursor;
        self.flush_identity();

        let (end_idx, end_cursor) = self.scan_to_line_end(idx, self.src_cursor);
        let expanded_offset = TextSize::new(self.expanded_text.len() as u32);

        match self.parse_default_nettype_line(idx, dir_start, end_cursor) {
            Ok((value, value_span, arg_end)) => {
                let event_seq = self.next_event_seq();
                self.directive_events.push(DirectiveEvent {
                    span: Span {
                        file: self.file,
                        range: TextRange::new(
                            TextSize::new(dir_start as u32),
                            TextSize::new(arg_end as u32),
                        ),
                    },
                    kind: DirectiveEventKind::DefaultNettype(DefaultNettypeDirective {
                        value,
                        value_span,
                    }),
                    origin: DirectiveEventOrigin::TopLevel,
                    expanded_offset,
                    event_seq,
                });
            }
            Err(err) => {
                let (range, msg) = match err {
                    DefaultNettypeParseError::MissingValue => (
                        TextRange::new(
                            TextSize::new(dir_start as u32),
                            TextSize::new((dir_start + dir_len) as u32),
                        ),
                        SmolStr::from("`default_nettype missing net type value"),
                    ),
                    DefaultNettypeParseError::InvalidValue(range) => {
                        let bad = &self.text[usize::from(range.start())..usize::from(range.end())];
                        (
                            range,
                            SmolStr::from(format!("`default_nettype invalid value '{bad}'")),
                        )
                    }
                    DefaultNettypeParseError::TrailingTokens(range) => (
                        range,
                        SmolStr::from("`default_nettype unexpected tokens after value"),
                    ),
                };
                self.push_error(range, msg);
            }
        }

        self.src_cursor = end_cursor;
        self.flush_start = end_cursor;
        end_idx - idx
    }

    /// Parse a complete `` `default_nettype `` directive line.
    ///
    /// Validates: exactly one argument token, must be a recognized net
    /// type value, no trailing non-trivia tokens. Returns
    /// `(value, value_span, byte_offset_after_arg)` on success.
    fn parse_default_nettype_line(
        &self,
        idx: usize,
        dir_start: usize,
        line_end_cursor: usize,
    ) -> Result<(crate::DefaultNettypeValue, Span, usize), DefaultNettypeParseError> {
        let dir_len: usize = self.tokens[idx].len.into();
        let (j, cursor) = self.skip_line_trivia(idx + 1, dir_start + dir_len);

        if !self.is_significant_before(j, cursor, line_end_cursor) {
            return Err(DefaultNettypeParseError::MissingValue);
        }

        let arg_len: usize = self.tokens[j].len.into();
        let arg_text = &self.text[cursor..cursor + arg_len];
        let arg_range = TextRange::new(
            TextSize::new(cursor as u32),
            TextSize::new((cursor + arg_len) as u32),
        );

        let value: crate::DefaultNettypeValue = arg_text
            .parse()
            .map_err(|()| DefaultNettypeParseError::InvalidValue(arg_range))?;

        let value_span = Span {
            file: self.file,
            range: arg_range,
        };
        let arg_end = cursor + arg_len;

        let (trail_j, trail_cursor) = self.skip_line_trivia(j + 1, arg_end);
        if self.is_significant_before(trail_j, trail_cursor, line_end_cursor) {
            let trail_len: usize = self.tokens[trail_j].len.into();
            return Err(DefaultNettypeParseError::TrailingTokens(TextRange::new(
                TextSize::new(trail_cursor as u32),
                TextSize::new((trail_cursor + trail_len) as u32),
            )));
        }

        Ok((value, value_span, arg_end))
    }

    fn handle_known_directive(&mut self, idx: usize, kw: DirectiveKeyword) -> usize {
        let dir_len: usize = self.tokens[idx].len.into();
        let dir_start = self.src_cursor;
        self.flush_identity();
        if self.cond.currently_emitting() {
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
        if self.cond.currently_emitting() {
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

    fn strip_macro_token(&mut self, idx: usize) -> usize {
        let dir_len: usize = self.tokens[idx].len.into();
        self.flush_identity();
        self.src_cursor += dir_len;
        self.flush_start = self.src_cursor;
        1
    }

    /// Strip a directive line from output: flush pending identity,
    /// advance past all tokens on this line. Returns token count consumed.
    pub(crate) fn strip_directive_line(&mut self, start_idx: usize) -> usize {
        self.flush_identity();
        let (end_idx, end_cursor) = self.scan_to_line_end(start_idx, self.src_cursor);
        self.src_cursor = end_cursor;
        self.flush_start = end_cursor;
        end_idx - start_idx
    }

    /// Scan forward from `(start_idx, start_cursor)` to the end of the
    /// current logical directive line (newline-carrying token or EOF).
    /// Skips past backslash-newline line continuations.
    /// Returns `(end_idx, end_cursor)` -- pure scan, no mutation.
    pub(crate) fn scan_to_line_end(&self, start_idx: usize, start_cursor: usize) -> (usize, usize) {
        let mut j = start_idx;
        let mut cursor = start_cursor;

        while j < self.tokens.len() {
            let t = self.tokens[j];
            if t.kind == SyntaxKind::Eof {
                break;
            }
            let t_len: usize = t.len.into();
            let t_text = &self.text[cursor..cursor + t_len];
            if token_carries_newline(t.kind, t_text) {
                if self.is_line_continuation(cursor, t_text) {
                    cursor += t_len;
                    j += 1;
                    continue;
                }
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
    pub(crate) fn tok_text_at(&self, idx: usize, byte_pos: usize) -> &str {
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
    pub(crate) fn has_non_ws_token_on_line(&self, j: usize, cursor: usize) -> bool {
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

    /// Check whether a newline-carrying trivia token represents a line
    /// continuation (`\` immediately before the newline). The backslash
    /// may be inside the trivia text (e.g. `\   \n` -- NOT a continuation
    /// because spaces intervene) or in the preceding token (the common
    /// `EscapedIdent(\)` + `Whitespace(\n)` pattern).
    pub(crate) fn is_line_continuation(&self, trivia_cursor: usize, trivia_text: &str) -> bool {
        let bytes = trivia_text.as_bytes();
        // Find the first \n in the trivia token text.
        let Some(nl_pos) = bytes.iter().position(|&b| b == b'\n') else {
            return false;
        };
        // Determine the start of the newline sequence (\r\n vs \n).
        let nl_start = if nl_pos > 0 && bytes[nl_pos - 1] == b'\r' {
            nl_pos - 1
        } else {
            nl_pos
        };
        if nl_start > 0 {
            // Backslash is inside this token, immediately before the newline.
            bytes[nl_start - 1] == b'\\'
        } else {
            // Newline is at the very start of the token; check the byte in
            // the preceding token (cross-token boundary).
            trivia_cursor > 0 && self.text.as_bytes()[trivia_cursor - 1] == b'\\'
        }
    }

    /// Flush pending identity bytes from the primary file into the
    /// expanded output, recording an identity segment.
    pub(crate) fn flush_identity(&mut self) {
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

/// A canonicalized time value parsed from the token stream.
///
/// `text` holds the token-composed canonical form (e.g. `"1ns"`) with
/// no trivia, regardless of whitespace in source. `end` is the byte
/// offset after the last consumed token.
struct ParsedTimeValue {
    text: SmolStr,
    end: usize,
}

/// Structured parse error for `` `default_nettype `` directives.
enum DefaultNettypeParseError {
    MissingValue,
    InvalidValue(TextRange),
    TrailingTokens(TextRange),
}

/// Whether a token with the given kind and text carries a newline.
///
/// Covers all trivia kinds: `Whitespace` (always), `BlockComment`
/// (multi-line), and `LineComment` (currently stops before `\n` in
/// our lexer, but handled defensively). Non-trivia tokens never
/// carry newlines in well-formed source.
pub(crate) fn token_carries_newline(kind: SyntaxKind, text: &str) -> bool {
    kind.is_trivia() && text.contains('\n')
}
