use std::sync::Arc;

use lyra_lexer::SyntaxKind;
use lyra_preprocess::{
    DirectiveEventKind, DirectiveEventOrigin, DirectiveKeyword, MacroEnv, MacroTok, MacroTokenSeq,
    MacroValue, PreprocessInputs, preprocess,
};
use lyra_source::{FileId, TextSize};
use smol_str::SmolStr;

fn tokens_from_text(text: &str) -> MacroTokenSeq {
    let tokens = lyra_lexer::lex(text);
    let mut result = Vec::new();
    let mut cursor = 0usize;
    for t in &tokens {
        if t.kind == SyntaxKind::Eof {
            break;
        }
        let len: usize = t.len.into();
        result.push(MacroTok {
            token: *t,
            text: SmolStr::from(&text[cursor..cursor + len]),
        });
        cursor += len;
    }
    MacroTokenSeq::from_vec(result)
}

struct NoIncludes;

impl lyra_preprocess::IncludeProvider for NoIncludes {
    fn resolve(&self, _: &str) -> Option<lyra_preprocess::ResolvedInclude<'_>> {
        None
    }
}

fn pp(text: &str) -> lyra_preprocess::PreprocOutput {
    pp_with_env(text, &MacroEnv::empty())
}

fn pp_with_env(text: &str, env: &MacroEnv) -> lyra_preprocess::PreprocOutput {
    let tokens = lyra_lexer::lex(text);
    preprocess(&PreprocessInputs {
        file: FileId(0),
        tokens: &tokens,
        text,
        provider: &NoIncludes,
        starting_env: env,
        macro_recursion_limit: PreprocessInputs::DEFAULT_RECURSION_LIMIT,
    })
}

#[test]
fn expand_object_like() {
    let out = pp("`define W 8\n`W\n");
    assert!(
        out.expanded_text.contains('8'),
        "expanded: {:?}",
        out.expanded_text
    );
    assert!(!out.expanded_text.contains("`W"));
    assert!(!out.expanded_text.contains("`define"));
}

#[test]
fn expand_flag_empty() {
    let out = pp("`define G\nx `G y\n");
    assert!(
        out.expanded_text.contains("x  y"),
        "expanded: {:?}",
        out.expanded_text
    );
}

#[test]
fn expand_multi_token() {
    let out = pp("`define DECL wire w;\n`DECL\n");
    assert!(
        out.expanded_text.contains("wire w;"),
        "expanded: {:?}",
        out.expanded_text
    );
}

#[test]
fn expand_preserves_surrounding() {
    let out = pp("`define V 1\na + `V + b\n");
    assert!(
        out.expanded_text.contains("a + 1 + b"),
        "expanded: {:?}",
        out.expanded_text
    );
}

#[test]
fn expand_multiple_on_line() {
    let out = pp("`define A x\n`define B y\n`A`B\n");
    assert!(
        out.expanded_text.contains("xy"),
        "expanded: {:?}",
        out.expanded_text
    );
}

#[test]
fn expand_recursive() {
    let out = pp("`define INNER 42\n`define OUTER `INNER\n`OUTER\n");
    assert!(
        out.expanded_text.contains("42"),
        "expanded: {:?}",
        out.expanded_text
    );
}

#[test]
fn expand_depth_exceeded() {
    let out = pp("`define LOOP `LOOP\n`LOOP\n");
    assert!(
        out.errors.iter().any(|e| e.message.contains("depth limit")),
        "errors: {:?}",
        out.errors
    );
}

#[test]
fn expand_undefined_macro_event() {
    let out = pp("`NOPE\n");
    assert!(
        out.directive_events
            .iter()
            .any(|e| matches!(&e.kind, DirectiveEventKind::UndefinedMacro(n) if n == "NOPE")),
        "events: {:?}",
        out.directive_events
    );
    assert!(!out.expanded_text.contains("`NOPE"));
}

#[test]
fn known_directive_never_macro() {
    let out = pp("`define timescale 100\n`timescale\n");
    assert!(
        out.directive_events.iter().any(|e| matches!(
            &e.kind,
            DirectiveEventKind::KnownDirective(DirectiveKeyword::Timescale)
        )),
        "events: {:?}",
        out.directive_events
    );
    assert!(!out.expanded_text.contains("100"));
}

#[test]
fn expand_in_false_conditional() {
    let mut env = MacroEnv::empty();
    env.define(
        SmolStr::new("X"),
        MacroValue::ObjectLike(Arc::new(tokens_from_text("42"))),
    );
    let out = pp_with_env("`ifdef NOPE\n`X\n`endif\n", &env);
    assert!(!out.expanded_text.contains("42"));
}

#[test]
fn expand_from_starting_env() {
    let mut env = MacroEnv::empty();
    env.define(
        SmolStr::new("V"),
        MacroValue::ObjectLike(Arc::new(tokens_from_text("99"))),
    );
    let out = pp_with_env("`V\n", &env);
    assert!(
        out.expanded_text.contains("99"),
        "expanded: {:?}",
        out.expanded_text
    );
}

#[test]
fn expand_define_after_use() {
    let out = pp("`X\n`define X 1\n");
    assert!(
        out.directive_events
            .iter()
            .any(|e| matches!(&e.kind, DirectiveEventKind::UndefinedMacro(n) if n == "X")),
        "events: {:?}",
        out.directive_events
    );
}

#[test]
fn expand_source_map_monotone() {
    let out = pp("`define W 8\n`W\n");
    let exp_len = out.expanded_text.len();
    let mut prev_offset = TextSize::new(0);
    for i in 0..exp_len {
        if let Some(span) = out.source_map.map_point(TextSize::new(i as u32)) {
            assert!(
                span.range.start() >= prev_offset,
                "non-monotone at expanded offset {i}: prev={prev_offset:?}, cur={:?}",
                span.range.start()
            );
            prev_offset = span.range.start();
        }
    }
}

#[test]
fn expand_body_undefined_macro_origin() {
    let out = pp("`define WRAP `MISSING\n`WRAP\n");
    let event = out
        .directive_events
        .iter()
        .find(|e| matches!(&e.kind, DirectiveEventKind::UndefinedMacro(n) if n == "MISSING"));
    assert!(event.is_some(), "events: {:?}", out.directive_events);
    assert_eq!(event.unwrap().origin, DirectiveEventOrigin::MacroExpansion);
}

#[test]
fn expand_body_known_directive_stripped() {
    let out = pp("`define BODY `ifdef\n`BODY\n");
    assert!(
        out.directive_events.iter().any(|e| matches!(
            (&e.kind, e.origin),
            (
                DirectiveEventKind::KnownDirective(DirectiveKeyword::Ifdef),
                DirectiveEventOrigin::MacroExpansion
            )
        )),
        "events: {:?}",
        out.directive_events
    );
    let has_directive = out.tokens.iter().any(|t| t.kind == SyntaxKind::Directive);
    assert!(!has_directive, "output must be directive-free");
    assert!(
        !out.expanded_text.contains("`ifdef"),
        "expanded: {:?}",
        out.expanded_text
    );
}

#[test]
fn expand_no_directive_tokens_in_output() {
    let out = pp("`define FOO bar\n`FOO\n");
    let has_directive = out.tokens.iter().any(|t| t.kind == SyntaxKind::Directive);
    assert!(!has_directive, "no directive tokens in output");
}

#[test]
fn token_lengths_sum_to_expanded_text() {
    let out = pp("`define W 8\na + `W + b\n");
    let total: u32 = out
        .tokens
        .iter()
        .filter(|t| t.kind != SyntaxKind::Eof)
        .map(|t| u32::from(t.len))
        .sum();
    assert_eq!(
        total as usize,
        out.expanded_text.len(),
        "token lengths should sum to expanded text length"
    );
}

#[test]
fn source_map_covers_every_byte() {
    let out = pp("`define W 8\na + `W + b\n");
    for i in 0..out.expanded_text.len() {
        let span = out.source_map.map_point(TextSize::new(i as u32));
        assert!(span.is_some(), "offset {i} should be mappable");
    }
}
