use lyra_lexer::{SyntaxKind, Token};
use lyra_source::{Span, TextRange, TextSize};
use smol_str::SmolStr;

use crate::args::parse_args_from_tokens;
use crate::engine::Preprocessor;
use crate::env::{MacroTemplate, MacroTok, MacroTokenSeq, MacroValue};
use crate::expand::{ExpansionCtx, ExpansionSink, expand_seq_into};
use crate::operators;
use crate::source_map::{Segment, SegmentKind};
use crate::{DirectiveEvent, DirectiveEventKind, DirectiveEventOrigin, PreprocError};

impl Preprocessor<'_> {
    pub(crate) fn handle_macro_use(&mut self, idx: usize, name: &str) -> usize {
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
            Some(MacroValue::ObjectLike(seq)) => self.expand_macro_tokens(call_site, &seq, 1),
            Some(MacroValue::FunctionLike { params, body }) => {
                self.handle_fn_macro_use(idx, name, &params, &body)
            }
        }
    }

    fn handle_fn_macro_use(
        &mut self,
        idx: usize,
        name: &str,
        params: &[SmolStr],
        body: &MacroTemplate,
    ) -> usize {
        let dir_len: usize = self.tokens[idx].len.into();
        let dir_start = self.src_cursor;

        // Adjacency: immediate next token must be LParen (no trivia skip)
        let next_idx = idx + 1;
        if next_idx >= self.tokens.len() || self.tokens[next_idx].kind != SyntaxKind::LParen {
            self.flush_identity();
            self.push_error(
                TextRange::new(
                    TextSize::new(dir_start as u32),
                    TextSize::new((dir_start + dir_len) as u32),
                ),
                SmolStr::from(format!(
                    "function-like macro '{name}' used without argument list",
                )),
            );
            self.src_cursor += dir_len;
            self.flush_start = self.src_cursor;
            return 1;
        }

        // Parse argument structure from the file token stream
        let lparen_cursor = self.src_cursor + dir_len;
        let parsed = match parse_args_from_tokens(self.tokens, next_idx) {
            Ok(p) => p,
            Err(eof_idx) => {
                self.flush_identity();
                self.push_error(
                    TextRange::new(
                        TextSize::new(lparen_cursor as u32),
                        TextSize::new((lparen_cursor + 1) as u32),
                    ),
                    SmolStr::from(format!("unterminated argument list for macro '{name}'")),
                );
                let end_cursor = lparen_cursor + token_span_bytes(&self.tokens[next_idx..eof_idx]);
                self.src_cursor = end_cursor;
                self.flush_start = self.src_cursor;
                return eof_idx - idx;
            }
        };

        let end_j = parsed.end_idx;
        let end_cursor = lparen_cursor + token_span_bytes(&self.tokens[next_idx..end_j]);

        // Validate arity: FOO() for zero-param macro is valid
        let actual_count = if params.is_empty()
            && parsed.arg_ranges.len() == 1
            && parsed.arg_ranges[0].0 == parsed.arg_ranges[0].1
        {
            0
        } else {
            parsed.arg_ranges.len()
        };

        if actual_count != params.len() {
            self.flush_identity();
            self.push_error(
                TextRange::new(
                    TextSize::new(dir_start as u32),
                    TextSize::new(end_cursor as u32),
                ),
                SmolStr::from(format!(
                    "macro '{name}' expects {} arguments, got {actual_count}",
                    params.len(),
                )),
            );
            self.src_cursor = end_cursor;
            self.flush_start = self.src_cursor;
            return end_j - idx;
        }

        // Materialize argument token sequences from index ranges
        let materialized = self.materialize_arg_ranges(&parsed.arg_ranges, next_idx, lparen_cursor);
        let args = if params.is_empty() {
            &[] as &[MacroTokenSeq]
        } else {
            &materialized
        };
        let instantiated = match body.instantiate(args) {
            Ok(seq) => seq,
            Err(e) => {
                self.flush_identity();
                self.push_error(
                    TextRange::new(
                        TextSize::new(dir_start as u32),
                        TextSize::new(end_cursor as u32),
                    ),
                    SmolStr::from(e.to_string()),
                );
                self.src_cursor = end_cursor;
                self.flush_start = self.src_cursor;
                return end_j - idx;
            }
        };

        // Expand with call_site spanning the full invocation
        let call_site = Span {
            file: self.file,
            range: TextRange::new(
                TextSize::new(dir_start as u32),
                TextSize::new(end_cursor as u32),
            ),
        };
        self.expand_macro_tokens(call_site, &instantiated, end_j - idx)
    }

    fn expand_macro_tokens(
        &mut self,
        call_site: Span,
        seq: &MacroTokenSeq,
        tokens_consumed: usize,
    ) -> usize {
        debug_assert!(
            self.cond.currently_emitting(),
            "expand_macro_tokens called while not emitting",
        );
        self.flush_identity();
        let base_offset = TextSize::new(self.expanded_text.len() as u32);

        let mut tmp_tokens = Vec::new();
        let mut tmp_text = String::new();
        let ctx = ExpansionCtx {
            env: &self.env,
            call_site,
            recursion_limit: self.macro_recursion_limit,
            base_offset,
        };
        let mut sink = ExpansionSink {
            out_tokens: &mut tmp_tokens,
            out_text: &mut tmp_text,
            events: &mut self.directive_events,
            next_event_seq: &mut self.event_seq_counter,
        };
        let result = expand_seq_into(&ctx, seq, 0, &mut sink);

        let site_len = usize::from(call_site.range.len());

        if let Err(msg) = result {
            self.errors.push(PreprocError {
                span: call_site,
                message: msg,
            });
            self.src_cursor += site_len;
            self.flush_start = self.src_cursor;
            return tokens_consumed;
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

        // Resolve macro operators (stringify, concat) if any are present
        if operators::has_macro_operators(&tmp_tokens) {
            let macro_toks = operators::tokens_to_macro_toks(&tmp_tokens, &tmp_text);
            let (resolved, op_errors) = operators::resolve_macro_operators(&macro_toks);
            for e in op_errors {
                self.errors.push(PreprocError {
                    span: operators::expansion_span(call_site, e.exp_range),
                    message: e.message,
                });
            }
            tmp_tokens.clear();
            tmp_text.clear();
            for mt in &resolved {
                tmp_tokens.push(mt.token);
                tmp_text.push_str(&mt.text);
            }
        }

        // Release invariant: output must not contain preprocess-only tokens.
        if operators::has_macro_operators(&tmp_tokens) {
            self.push_error(
                call_site.range,
                SmolStr::from("preprocessor bug: macro operator tokens survived into output"),
            );
            operators::strip_preprocess_only(&mut tmp_tokens, &mut tmp_text);
        }

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

        self.src_cursor += site_len;
        self.flush_start = self.src_cursor;
        tokens_consumed
    }

    /// Convert token-index ranges (from `parse_args_from_tokens`) into
    /// `MacroTokenSeq` argument values. Cursor tracking happens here
    /// so the arg parser stays purely structural.
    fn materialize_arg_ranges(
        &self,
        arg_ranges: &[(usize, usize)],
        lparen_idx: usize,
        lparen_cursor: usize,
    ) -> Vec<MacroTokenSeq> {
        let lparen_len: usize = self.tokens[lparen_idx].len.into();
        let mut cursor = lparen_cursor + lparen_len;
        let mut tok_idx = lparen_idx + 1;
        let mut result = Vec::with_capacity(arg_ranges.len());

        for &(start, end) in arg_ranges {
            // Advance past separator tokens (commas) between args
            while tok_idx < start {
                cursor += usize::from(self.tokens[tok_idx].len);
                tok_idx += 1;
            }
            // Materialize this argument's tokens
            let mut toks = Vec::new();
            for j in start..end {
                let t = self.tokens[j];
                let t_len: usize = t.len.into();
                toks.push(MacroTok {
                    token: t,
                    text: SmolStr::from(&self.text[cursor..cursor + t_len]),
                });
                cursor += t_len;
            }
            tok_idx = end;
            result.push(MacroTokenSeq::from_vec(toks));
        }

        result
    }
}

/// Total byte length of a token slice.
fn token_span_bytes(tokens: &[Token]) -> usize {
    tokens.iter().map(|t| usize::from(t.len)).sum()
}
