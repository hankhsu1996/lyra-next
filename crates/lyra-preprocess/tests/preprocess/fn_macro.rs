use lyra_lexer::SyntaxKind;
use lyra_preprocess::{MacroEnv, PreprocessInputs, preprocess};
use lyra_source::{FileId, TextSize};

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
        file_path: "",
    })
}

// Define parsing tests

#[test]
fn define_fn_macro_basic() {
    let out = pp("`define ADD(a, b) a+b\n");
    assert!(out.errors.is_empty(), "errors: {:?}", out.errors);
    assert!(out.final_env.is_defined("ADD"));
}

#[test]
fn define_fn_macro_no_params() {
    let out = pp("`define CALL() 1\n");
    assert!(out.errors.is_empty(), "errors: {:?}", out.errors);
    assert!(out.final_env.is_defined("CALL"));
}

#[test]
fn define_fn_macro_whitespace_not_adjacent() {
    // Space between name and `(` makes it object-like with body starting at `(`
    let out = pp("`define FOO (a)\n");
    assert!(out.errors.is_empty(), "errors: {:?}", out.errors);
    match out.final_env.get("FOO").map(|d| &d.value) {
        Some(lyra_preprocess::MacroValue::ObjectLike(seq)) => {
            assert!(seq.text().starts_with("(a)"), "body: {:?}", seq.text());
        }
        other => panic!("expected ObjectLike, got {other:?}"),
    }
}

#[test]
fn define_fn_macro_comment_not_adjacent() {
    // Comment between name and `(` makes it object-like
    let out = pp("`define FOO/**/(a)\n");
    assert!(out.errors.is_empty(), "errors: {:?}", out.errors);
    match out.final_env.get("FOO").map(|d| &d.value) {
        Some(lyra_preprocess::MacroValue::ObjectLike(seq)) => {
            assert!(
                seq.text().contains("(a)"),
                "body should contain (a): {:?}",
                seq.text()
            );
        }
        other => panic!("expected ObjectLike, got {other:?}"),
    }
}

#[test]
fn define_fn_macro_duplicate_param() {
    let out = pp("`define BAD(a, a) body\n");
    assert_eq!(out.errors.len(), 1, "errors: {:?}", out.errors);
    assert!(
        out.errors[0].message.contains("duplicate parameter name"),
        "msg: {}",
        out.errors[0].message
    );
}

#[test]
fn define_fn_macro_unterminated_params() {
    let out = pp("`define BAD(a, b\nwire w;\n");
    assert_eq!(out.errors.len(), 1, "errors: {:?}", out.errors);
    assert!(
        out.errors[0]
            .message
            .contains("unterminated parameter list"),
        "msg: {}",
        out.errors[0].message
    );
}

// Expansion tests

#[test]
fn expand_fn_macro_basic() {
    let out = pp("`define ADD(a, b) a+b\n`ADD(1, 2)\n");
    assert!(out.errors.is_empty(), "errors: {:?}", out.errors);
    // Space before `2` is preserved: whitespace after comma is part of the arg
    assert!(
        out.expanded_text.contains("1+ 2"),
        "expanded: {:?}",
        out.expanded_text
    );
}

#[test]
fn expand_fn_macro_no_args() {
    let out = pp("`define CALL() 1\n`CALL()\n");
    assert!(out.errors.is_empty(), "errors: {:?}", out.errors);
    assert!(
        out.expanded_text.contains('1'),
        "expanded: {:?}",
        out.expanded_text
    );
}

#[test]
fn expand_fn_macro_nested_parens() {
    let out = pp("`define FOO(a, b) a+b\n`FOO((1+2), 3)\n");
    assert!(out.errors.is_empty(), "errors: {:?}", out.errors);
    assert!(
        out.expanded_text.contains("(1+2)+ 3"),
        "expanded: {:?}",
        out.expanded_text
    );
}

#[test]
fn expand_fn_macro_arg_count_mismatch() {
    let out = pp("`define ADD(a, b) a+b\n`ADD(1)\n");
    assert_eq!(out.errors.len(), 1, "errors: {:?}", out.errors);
    assert!(
        out.errors[0].message.contains("expects 2 arguments, got 1"),
        "msg: {}",
        out.errors[0].message
    );
}

#[test]
fn expand_fn_macro_missing_args() {
    let out = pp("`define ADD(a, b) a+b\n`ADD\n");
    assert_eq!(out.errors.len(), 1, "errors: {:?}", out.errors);
    assert!(
        out.errors[0].message.contains("used without argument list"),
        "msg: {}",
        out.errors[0].message
    );
}

#[test]
fn expand_fn_macro_unterminated_args() {
    let out = pp("`define ADD(a, b) a+b\n`ADD(1,\n");
    assert_eq!(out.errors.len(), 1, "errors: {:?}", out.errors);
    assert!(
        out.errors[0].message.contains("unterminated argument list"),
        "msg: {}",
        out.errors[0].message
    );
}

#[test]
fn expand_fn_macro_recursive_body() {
    // Body references an object-like macro
    let out = pp("`define V 42\n`define WRAP(x) x+`V\n`WRAP(1)\n");
    assert!(out.errors.is_empty(), "errors: {:?}", out.errors);
    assert!(
        out.expanded_text.contains("1+42"),
        "expanded: {:?}",
        out.expanded_text
    );
}

#[test]
fn expand_fn_macro_nested_fn_macro() {
    // Function-like macro used inside an instantiated body
    let out = pp("`define ADD(a, b) a+b\n`define WRAP(x) `ADD(x, 0)\n`WRAP(5)\n");
    assert!(out.errors.is_empty(), "errors: {:?}", out.errors);
    assert!(
        out.expanded_text.contains("5+ 0"),
        "expanded: {:?}",
        out.expanded_text
    );
}

#[test]
fn expand_fn_macro_empty_args() {
    let out = pp("`define TWO(a, b) a+b\n`TWO(,)\n");
    assert!(out.errors.is_empty(), "errors: {:?}", out.errors);
    assert!(
        out.expanded_text.contains('+'),
        "expanded: {:?}",
        out.expanded_text
    );
}

#[test]
fn expand_fn_macro_zero_param_invocation() {
    let out = pp("`define NOP() 1\n`NOP()\n");
    assert!(out.errors.is_empty(), "errors: {:?}", out.errors);
    assert!(
        out.expanded_text.contains('1'),
        "expanded: {:?}",
        out.expanded_text
    );
}

#[test]
fn expand_fn_macro_multiline_args() {
    let out = pp("`define ADD(a, b) a+b\n`ADD(1,\n2)\n");
    assert!(out.errors.is_empty(), "errors: {:?}", out.errors);
    assert!(
        out.expanded_text.contains("1+"),
        "expanded: {:?}",
        out.expanded_text
    );
    assert!(
        out.expanded_text.contains("+\n2"),
        "expanded should contain newline in arg: {:?}",
        out.expanded_text
    );
}

#[test]
fn expand_fn_macro_preserves_surrounding() {
    let out = pp("`define ADD(a, b) a+b\nx + `ADD(1, 2) + y\n");
    assert!(out.errors.is_empty(), "errors: {:?}", out.errors);
    assert!(
        out.expanded_text.contains("x + 1+ 2 + y"),
        "expanded: {:?}",
        out.expanded_text
    );
}

#[test]
fn expand_fn_macro_source_map_covers_invocation() {
    let out = pp("`define ADD(a, b) a+b\n`ADD(1, 2)\n");
    assert!(out.errors.is_empty(), "errors: {:?}", out.errors);
    // Source map should cover the expansion
    for i in 0..out.expanded_text.len() {
        let span = out.source_map.map_point(TextSize::new(i as u32));
        assert!(span.is_some(), "offset {i} should be mappable");
    }
}

#[test]
fn expand_fn_macro_call_site_trivia_before_lparen() {
    // Trivia between directive and `(` at the call site means no
    // argument list: adjacency is enforced at use site too.
    let out = pp("`define FOO(a) a\n`FOO (1)\n");
    assert_eq!(out.errors.len(), 1, "errors: {:?}", out.errors);
    assert!(
        out.errors[0].message.contains("used without argument list"),
        "msg: {}",
        out.errors[0].message
    );
}

#[test]
fn expand_fn_macro_call_site_comment_before_lparen() {
    // Block comment between directive and `(` breaks adjacency
    let out = pp("`define FOO(a) a\n`FOO/**/(1)\n");
    assert_eq!(out.errors.len(), 1, "errors: {:?}", out.errors);
    assert!(
        out.errors[0].message.contains("used without argument list"),
        "msg: {}",
        out.errors[0].message
    );
}

#[test]
fn expand_fn_macro_in_false_conditional() {
    let out = pp("`define ADD(a, b) a+b\n`ifdef NOPE\n`ADD(1, 2)\n`endif\n");
    assert!(out.errors.is_empty(), "errors: {:?}", out.errors);
    assert!(
        !out.expanded_text.contains("1+2"),
        "expanded should not contain expansion: {:?}",
        out.expanded_text
    );
}

#[test]
fn expand_fn_macro_no_directive_tokens_in_output() {
    let out = pp("`define ADD(a, b) a+b\n`ADD(1, 2)\n");
    let has_directive = out.tokens.iter().any(|t| t.kind == SyntaxKind::Directive);
    assert!(!has_directive, "no directive tokens in output");
}

#[test]
fn expand_fn_macro_token_lengths_sum() {
    let out = pp("`define ADD(a, b) a+b\nx + `ADD(1, 2) + y\n");
    assert!(out.errors.is_empty(), "errors: {:?}", out.errors);
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
