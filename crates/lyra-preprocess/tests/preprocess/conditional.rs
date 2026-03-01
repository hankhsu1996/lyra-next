use lyra_lexer::SyntaxKind;
use lyra_preprocess::{MacroEnv, MacroValue, PreprocessInputs, preprocess};
use lyra_source::{FileId, TextSize};
use smol_str::SmolStr;

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
    })
}

fn env_with(name: &str) -> MacroEnv {
    let mut env = MacroEnv::empty();
    env.define(SmolStr::new(name), MacroValue::Flag);
    env
}

// ifdef/ifndef basic

#[test]
fn ifdef_true_emits_body() {
    let env = env_with("FOO");
    let out = pp_with_env("`ifdef FOO\nwire w;\n`endif\n", &env);
    assert!(out.expanded_text.contains("wire w;"));
    assert!(!out.expanded_text.contains("`ifdef"));
    assert!(!out.expanded_text.contains("`endif"));
}

#[test]
fn ifdef_false_skips_body() {
    let out = pp("`ifdef FOO\nwire w;\n`endif\n");
    assert!(!out.expanded_text.contains("wire w;"));
}

#[test]
fn ifndef_true_emits_body() {
    let out = pp("`ifndef FOO\nwire w;\n`endif\n");
    assert!(out.expanded_text.contains("wire w;"));
}

#[test]
fn ifndef_false_skips_body() {
    let env = env_with("FOO");
    let out = pp_with_env("`ifndef FOO\nwire w;\n`endif\n", &env);
    assert!(!out.expanded_text.contains("wire w;"));
}

// ifdef/else

#[test]
fn ifdef_else_true_branch() {
    let env = env_with("FOO");
    let out = pp_with_env("`ifdef FOO\nwire a;\n`else\nwire b;\n`endif\n", &env);
    assert!(out.expanded_text.contains("wire a;"));
    assert!(!out.expanded_text.contains("wire b;"));
}

#[test]
fn ifdef_else_false_branch() {
    let out = pp("`ifdef FOO\nwire a;\n`else\nwire b;\n`endif\n");
    assert!(!out.expanded_text.contains("wire a;"));
    assert!(out.expanded_text.contains("wire b;"));
}

// elsif

#[test]
fn elsif_selects_first_true() {
    let env = env_with("BAR");
    let text = "`ifdef FOO\nwire a;\n`elsif BAR\nwire b;\n`elsif BAZ\nwire c;\n`endif\n";
    let out = pp_with_env(text, &env);
    assert!(!out.expanded_text.contains("wire a;"));
    assert!(out.expanded_text.contains("wire b;"));
    assert!(!out.expanded_text.contains("wire c;"));
}

#[test]
fn elsif_all_false_falls_to_else() {
    let text = "`ifdef FOO\nwire a;\n`elsif BAR\nwire b;\n`else\nwire c;\n`endif\n";
    let out = pp(text);
    assert!(!out.expanded_text.contains("wire a;"));
    assert!(!out.expanded_text.contains("wire b;"));
    assert!(out.expanded_text.contains("wire c;"));
}

#[test]
fn elsif_first_true_skips_rest() {
    let mut env = MacroEnv::empty();
    env.define(SmolStr::new("A"), MacroValue::Flag);
    env.define(SmolStr::new("B"), MacroValue::Flag);
    let text = "`ifdef A\nwire a;\n`elsif B\nwire b;\n`else\nwire c;\n`endif\n";
    let out = pp_with_env(text, &env);
    assert!(out.expanded_text.contains("wire a;"));
    assert!(!out.expanded_text.contains("wire b;"));
    assert!(!out.expanded_text.contains("wire c;"));
}

// Nesting

#[test]
fn nested_ifdef() {
    let mut env = MacroEnv::empty();
    env.define(SmolStr::new("A"), MacroValue::Flag);
    env.define(SmolStr::new("B"), MacroValue::Flag);
    let text = "`ifdef A\n`ifdef B\nwire ab;\n`endif\n`endif\n";
    let out = pp_with_env(text, &env);
    assert!(out.expanded_text.contains("wire ab;"));
}

#[test]
fn nested_ifdef_parent_false() {
    let env = env_with("B");
    let text = "`ifdef A\n`ifdef B\nwire ab;\n`endif\n`endif\n";
    let out = pp_with_env(text, &env);
    assert!(!out.expanded_text.contains("wire ab;"));
}

// Error cases

#[test]
fn else_after_else_error() {
    let text = "`ifdef FOO\n`else\n`else\n`endif\n";
    let out = pp(text);
    assert_eq!(out.errors.len(), 1);
    assert!(out.errors[0].message.contains("duplicate `else"));
}

#[test]
fn elsif_after_else_error() {
    let text = "`ifdef FOO\n`else\n`elsif BAR\n`endif\n";
    let out = pp(text);
    assert_eq!(out.errors.len(), 1);
    assert!(out.errors[0].message.contains("`elsif after `else"));
}

#[test]
fn endif_without_ifdef_error() {
    let text = "`endif\nmodule top; endmodule\n";
    let out = pp(text);
    assert_eq!(out.errors.len(), 1);
    assert!(out.errors[0].message.contains("`endif without matching"));
}

#[test]
fn else_without_ifdef_error() {
    let text = "`else\nwire w;\n`endif\n";
    let out = pp(text);
    // Two errors: `else without matching + `endif without matching
    assert!(!out.errors.is_empty());
    assert!(out.errors[0].message.contains("`else without matching"));
}

#[test]
fn elsif_without_ifdef_error() {
    let text = "`elsif FOO\nwire w;\n`endif\n";
    let out = pp(text);
    assert!(!out.errors.is_empty());
    assert!(out.errors[0].message.contains("`elsif without matching"));
}

#[test]
fn unterminated_ifdef_error() {
    let text = "`ifdef FOO\nwire w;\n";
    let out = pp(text);
    assert_eq!(out.errors.len(), 1);
    assert!(out.errors[0].message.contains("unterminated"));
}

#[test]
fn ifdef_missing_name_error() {
    let text = "`ifdef\nwire w;\n`endif\n";
    let out = pp(text);
    assert!(!out.errors.is_empty());
    assert!(out.errors[0].message.contains("missing macro name"));
}

// Directive stripping

#[test]
fn conditional_strips_all_directives() {
    let env = env_with("FOO");
    let text = "`ifdef FOO\nwire w;\n`endif\n";
    let out = pp_with_env(text, &env);
    let has_directive = out.tokens.iter().any(|t| t.kind == SyntaxKind::Directive);
    assert!(!has_directive, "no directive tokens in output");
}

// SourceMap invariants

#[test]
fn source_map_after_skipped_region_is_total() {
    let env = env_with("FOO");
    let text = "wire a;\n`ifdef FOO\nwire b;\n`endif\nwire c;\n";
    let out = pp_with_env(text, &env);

    // Every byte in expanded text should be mappable
    for i in 0..out.expanded_text.len() {
        let span = out.source_map.map_point(TextSize::new(i as u32));
        assert!(span.is_some(), "offset {i} should map");
    }
}

#[test]
fn source_map_maps_every_byte() {
    let text = "`ifdef NONE\nskipped;\n`else\nwire w;\n`endif\n";
    let out = pp(text);

    for i in 0..out.expanded_text.len() {
        let span = out.source_map.map_point(TextSize::new(i as u32));
        assert!(span.is_some(), "offset {i} should map");
    }
}

#[test]
fn token_lengths_sum_to_expanded_text_with_conditionals() {
    let env = env_with("FOO");
    let text = "module top;\n`ifdef FOO\nwire w;\n`endif\nendmodule\n";
    let out = pp_with_env(text, &env);

    let total: u32 = out
        .tokens
        .iter()
        .filter(|t| t.kind != SyntaxKind::Eof)
        .map(|t| u32::from(t.len))
        .sum();
    assert_eq!(total as usize, out.expanded_text.len());
}
