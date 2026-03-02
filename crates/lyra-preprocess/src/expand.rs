use lyra_lexer::{SyntaxKind, Token};
use lyra_source::{Span, TextSize};
use smol_str::SmolStr;

use crate::args::{ArgParseError, parse_args_from_seq};
use crate::directive::{DirectiveClass, classify_directive};
use crate::env::{MacroEnv, MacroTok, MacroTokenSeq, MacroValue};
use crate::{DirectiveEvent, DirectiveEventKind, DirectiveEventOrigin};

/// Immutable context shared across recursive macro expansion calls.
pub(crate) struct ExpansionCtx<'a> {
    pub env: &'a MacroEnv,
    pub call_site: Span,
    pub recursion_limit: usize,
    pub base_offset: TextSize,
}

/// Mutable accumulators for recursive macro expansion, bundled to
/// keep the `expand_seq_into` argument count manageable.
pub(crate) struct ExpansionSink<'a> {
    pub out_tokens: &'a mut Vec<Token>,
    pub out_text: &'a mut String,
    pub events: &'a mut Vec<DirectiveEvent>,
    pub next_event_seq: &'a mut u32,
}

/// Recursively expand a macro token sequence, splicing non-directive
/// tokens into the sink and stripping or recursing on directive tokens
/// found in the body. Index-based iteration allows function-like macro
/// invocations inside expansion bodies to consume their argument tokens.
///
/// Precondition: only called when the preprocessor is emitting (i.e.,
/// from `expand_macro_tokens` which is gated on `currently_emitting`).
/// All events produced here are therefore from emitted text.
pub(crate) fn expand_seq_into(
    ctx: &ExpansionCtx<'_>,
    seq: &MacroTokenSeq,
    depth: usize,
    sink: &mut ExpansionSink<'_>,
) -> Result<(), SmolStr> {
    if depth > ctx.recursion_limit {
        return Err(SmolStr::from("macro expansion depth limit exceeded"));
    }

    let toks = seq.tokens();
    let mut i = 0;
    while i < toks.len() {
        let tok = &toks[i];
        if tok.token.kind == SyntaxKind::Directive {
            let offset = ctx.base_offset + TextSize::new(sink.out_text.len() as u32);
            match classify_directive(&tok.text) {
                DirectiveClass::MacroInvoke { name } => {
                    let value = ctx.env.get(name).map(|d| d.value.clone());
                    match value {
                        None => {
                            let event_seq = *sink.next_event_seq;
                            *sink.next_event_seq += 1;
                            sink.events.push(DirectiveEvent {
                                span: ctx.call_site,
                                kind: DirectiveEventKind::UndefinedMacro(SmolStr::from(name)),
                                origin: DirectiveEventOrigin::MacroExpansion,
                                expanded_offset: offset,
                                event_seq,
                            });
                            i += 1;
                        }
                        Some(MacroValue::Flag) => {
                            i += 1;
                        }
                        Some(MacroValue::ObjectLike(inner_seq)) => {
                            expand_seq_into(ctx, &inner_seq, depth + 1, sink)?;
                            i += 1;
                        }
                        Some(MacroValue::FunctionLike { params, body }) => {
                            i += 1;
                            i += expand_fn_in_seq(ctx, toks, i, &params, &body, depth, sink)?;
                        }
                    }
                }
                DirectiveClass::Keyword(kw) => {
                    let event_seq = *sink.next_event_seq;
                    *sink.next_event_seq += 1;
                    sink.events.push(DirectiveEvent {
                        span: ctx.call_site,
                        kind: DirectiveEventKind::KnownDirective(kw),
                        origin: DirectiveEventOrigin::MacroExpansion,
                        expanded_offset: offset,
                        event_seq,
                    });
                    i += 1;
                }
                DirectiveClass::Other { raw } => {
                    let event_seq = *sink.next_event_seq;
                    *sink.next_event_seq += 1;
                    sink.events.push(DirectiveEvent {
                        span: ctx.call_site,
                        kind: DirectiveEventKind::UnrecognizedDirective(SmolStr::from(raw)),
                        origin: DirectiveEventOrigin::MacroExpansion,
                        expanded_offset: offset,
                        event_seq,
                    });
                    i += 1;
                }
            }
        } else {
            sink.out_tokens.push(tok.token);
            sink.out_text.push_str(&tok.text);
            i += 1;
        }
    }

    Ok(())
}

/// Handle a function-like macro invocation inside an expansion body.
/// Looks ahead from `after_directive_idx` in `toks` for an `LParen`
/// (skipping trivia), parses arguments, instantiates, and recursively
/// expands. Returns the number of extra tokens consumed (past the
/// directive, which the caller already advanced past).
fn expand_fn_in_seq(
    ctx: &ExpansionCtx<'_>,
    toks: &[MacroTok],
    after_directive_idx: usize,
    params: &[SmolStr],
    body: &crate::env::MacroTemplate,
    depth: usize,
    sink: &mut ExpansionSink<'_>,
) -> Result<usize, SmolStr> {
    // Look ahead for `LParen`, skipping trivia
    let mut look = after_directive_idx;
    let mut lparen_idx = None;
    while look < toks.len() {
        if toks[look].token.kind.is_trivia() {
            look += 1;
            continue;
        }
        if toks[look].token.kind == SyntaxKind::LParen {
            lparen_idx = Some(look);
        }
        break;
    }

    let Some(lp_idx) = lparen_idx else {
        // No argument list found -- skip the directive
        return Ok(0);
    };

    let parsed = match parse_args_from_seq(toks, lp_idx) {
        Ok(p) => p,
        Err(ArgParseError::Unterminated) => {
            return Ok(toks.len() - after_directive_idx);
        }
    };

    let actual_count = if params.is_empty() && parsed.args.len() == 1 && parsed.args[0].is_empty() {
        0
    } else {
        parsed.args.len()
    };

    if actual_count != params.len() {
        // Arity mismatch -- skip the invocation
        return Ok(parsed.end_idx - after_directive_idx);
    }

    let args = if params.is_empty() {
        &[] as &[MacroTokenSeq]
    } else {
        &parsed.args
    };
    let instantiated = body
        .instantiate(args)
        .map_err(|e| SmolStr::from(e.to_string()))?;
    expand_seq_into(ctx, &instantiated, depth + 1, sink)?;

    Ok(parsed.end_idx - after_directive_idx)
}
