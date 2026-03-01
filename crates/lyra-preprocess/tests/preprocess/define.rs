use lyra_preprocess::{MacroEnv, MacroValue, PreprocessInputs, preprocess};
use lyra_source::FileId;
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

#[test]
fn define_flag() {
    let out = pp("`define FOO\n`ifdef FOO\nwire w;\n`endif\n");
    assert!(out.expanded_text.contains("wire w;"));
}

#[test]
fn define_with_value() {
    let out = pp("`define WIDTH 8\n`ifdef WIDTH\nwire w;\n`endif\n");
    assert!(out.expanded_text.contains("wire w;"));
    let def = out.final_env.get("WIDTH");
    assert!(def.is_some());
    assert_eq!(
        def.map(|d| &d.value),
        Some(&MacroValue::ObjectLike(SmolStr::new("8")))
    );
}

#[test]
fn define_strips_directive_from_output() {
    let out = pp("`define FOO\nwire w;\n");
    assert!(!out.expanded_text.contains("`define"));
    assert!(out.expanded_text.contains("wire w;"));
}

#[test]
fn define_value_stops_at_newline() {
    let out = pp("`define X val1 val2\nwire w;\n");
    let def = out.final_env.get("X");
    assert_eq!(
        def.map(|d| &d.value),
        Some(&MacroValue::ObjectLike(SmolStr::new("val1 val2")))
    );
    assert!(out.expanded_text.contains("wire w;"));
}

#[test]
fn undef_removes_macro() {
    let mut env = MacroEnv::empty();
    env.define(SmolStr::new("FOO"), MacroValue::Flag);
    let out = pp_with_env("`undef FOO\n`ifdef FOO\nwire w;\n`endif\n", &env);
    assert!(!out.expanded_text.contains("wire w;"));
    assert!(!out.final_env.is_defined("FOO"));
}

#[test]
fn undef_nonexistent_is_silent() {
    let out = pp("`undef NOPE\nwire w;\n");
    assert!(out.errors.is_empty());
    assert!(out.expanded_text.contains("wire w;"));
}

#[test]
fn define_redefines() {
    let out = pp("`define X 1\n`define X 2\n");
    assert_eq!(
        out.final_env.get("X").map(|d| &d.value),
        Some(&MacroValue::ObjectLike(SmolStr::new("2")))
    );
}

#[test]
fn define_no_name_error() {
    let out = pp("`define\nwire w;\n");
    assert_eq!(out.errors.len(), 1);
    assert!(out.errors[0].message.contains("missing macro name"));
}

#[test]
fn undef_no_name_error() {
    let out = pp("`undef\nwire w;\n");
    assert_eq!(out.errors.len(), 1);
    assert!(out.errors[0].message.contains("missing macro name"));
}

#[test]
fn define_inside_false_branch_ignored() {
    let out = pp("`ifdef NOPE\n`define FOO\n`endif\n`ifdef FOO\nwire w;\n`endif\n");
    assert!(!out.expanded_text.contains("wire w;"));
    assert!(!out.final_env.is_defined("FOO"));
}

#[test]
fn starting_env_visible() {
    let mut env = MacroEnv::empty();
    env.define(SmolStr::new("PRESET"), MacroValue::Flag);
    let out = pp_with_env("`ifdef PRESET\nwire w;\n`endif\n", &env);
    assert!(out.expanded_text.contains("wire w;"));
}

#[test]
fn final_env_returned() {
    let out = pp("`define A\n`define B 42\n");
    assert!(out.final_env.is_defined("A"));
    assert!(out.final_env.is_defined("B"));
    assert_eq!(out.final_env.len(), 2);
}
