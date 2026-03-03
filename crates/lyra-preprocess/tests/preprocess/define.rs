use lyra_preprocess::{MacroEnv, MacroValue, PreprocessInputs, preprocess};
use lyra_source::FileId;
use smol_str::SmolStr;

fn assert_object_like_text(value: Option<&MacroValue>, expected: &str) {
    match value {
        Some(MacroValue::ObjectLike(seq)) => assert_eq!(seq.text(), expected),
        other => panic!("expected ObjectLike with text \"{expected}\", got {other:?}"),
    }
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
    let tokens = lyra_lexer::lex_with_mode(text, lyra_lexer::LexMode::Preprocess);
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
    assert_object_like_text(def.map(|d| &d.value), "8");
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
    assert_object_like_text(def.map(|d| &d.value), "val1 val2");
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
    assert_object_like_text(out.final_env.get("X").map(|d| &d.value), "2");
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

#[test]
fn continuation_object_like() {
    let out = pp("`define FOO \\\nbar\n`FOO\n");
    assert!(out.errors.is_empty());
    assert_object_like_text(out.final_env.get("FOO").map(|d| &d.value), "bar");
    assert!(out.expanded_text.contains("bar"));
}

#[test]
fn continuation_multiple_lines() {
    let out = pp("`define FOO \\\n  a \\\n  + b\n`FOO\n");
    assert!(out.errors.is_empty());
    assert_object_like_text(out.final_env.get("FOO").map(|d| &d.value), "  a   + b");
}

#[test]
fn continuation_fn_macro() {
    let out = pp("`define ADD(a,b) \\\n((a)+(b))\n`ADD(1,2)\n");
    assert!(out.errors.is_empty());
    assert!(out.expanded_text.contains("((1)+(2))"));
}

#[test]
fn continuation_in_param_list() {
    let out = pp("`define FOO(a, \\\nb) a+b\n`FOO(1,2)\n");
    assert!(out.errors.is_empty());
    assert!(out.expanded_text.contains("1+2"));
}

#[test]
fn continuation_preserves_indentation() {
    let out = pp("`define FOO \\\n    bar\n`FOO\n");
    assert!(out.errors.is_empty());
    assert_object_like_text(out.final_env.get("FOO").map(|d| &d.value), "    bar");
}

#[test]
fn continuation_followed_by_blank_line() {
    let out = pp("`define FOO \\\n\nwire w;\n");
    assert!(out.errors.is_empty());
    let def = out.final_env.get("FOO");
    match def.map(|d| &d.value) {
        Some(MacroValue::Flag) => {}
        other => panic!("expected Flag (empty body), got {other:?}"),
    }
    assert!(out.expanded_text.contains("wire w;"));
}

#[test]
fn continuation_crlf() {
    let out = pp("`define FOO \\\r\nbar\n`FOO\n");
    assert!(out.errors.is_empty());
    assert_object_like_text(out.final_env.get("FOO").map(|d| &d.value), "bar");
}

#[test]
fn continuation_comment_in_body() {
    // Comment with trailing `\` acts as continuation. The comment is initial
    // trivia (skipped before body), so the body is just `bar` from the next line.
    let out = pp("`define FOO // comment \\\nbar\n`FOO\n");
    assert!(out.errors.is_empty());
    assert_object_like_text(out.final_env.get("FOO").map(|d| &d.value), "bar");
}

#[test]
fn no_continuation_when_spaces_after_backslash() {
    // `\` followed by spaces then newline is NOT a continuation per LRM;
    // the backslash must be immediately before the newline.
    let out = pp("`define FOO \\   \nbar\n");
    assert!(out.errors.is_empty());
    // Body is `\` + spaces (stops at the newline without continuing).
    // `bar` is NOT part of the macro body.
    let def = out.final_env.get("FOO");
    match def.map(|d| &d.value) {
        Some(MacroValue::ObjectLike(seq)) => {
            assert!(
                !seq.text().contains("bar"),
                "body should not include 'bar': {:?}",
                seq.text()
            );
        }
        other => panic!("expected ObjectLike, got {other:?}"),
    }
}
