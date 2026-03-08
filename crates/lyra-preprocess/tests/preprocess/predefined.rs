use lyra_lexer::SyntaxKind;
use lyra_preprocess::{MacroEnv, PreprocessInputs, preprocess};
use lyra_source::FileId;

struct NoIncludes;

impl lyra_preprocess::IncludeProvider for NoIncludes {
    fn resolve(&self, _: &str) -> Option<lyra_preprocess::ResolvedInclude<'_>> {
        None
    }
}

fn pp(text: &str) -> lyra_preprocess::PreprocOutput {
    pp_with_path(text, "test.sv")
}

fn pp_with_path(text: &str, path: &str) -> lyra_preprocess::PreprocOutput {
    let tokens = lyra_lexer::lex_with_mode(text, lyra_lexer::LexMode::Preprocess);
    preprocess(&PreprocessInputs {
        file: FileId(0),
        tokens: &tokens,
        text,
        provider: &NoIncludes,
        starting_env: &MacroEnv::empty(),
        macro_recursion_limit: PreprocessInputs::DEFAULT_RECURSION_LIMIT,
        file_path: path,
    })
}

#[test]
fn file_expands_to_string_literal() {
    let out = pp("`__FILE__\n");
    assert!(out.errors.is_empty(), "errors: {:?}", out.errors);
    assert_eq!(out.expanded_text.trim(), "\"test.sv\"");
    let tok = out.tokens.iter().find(|t| t.kind != SyntaxKind::Eof);
    assert_eq!(tok.map(|t| t.kind), Some(SyntaxKind::StringLiteral));
}

#[test]
fn file_uses_provided_path() {
    let out = pp_with_path("`__FILE__\n", "src/foo.sv");
    assert!(out.errors.is_empty(), "errors: {:?}", out.errors);
    assert_eq!(out.expanded_text.trim(), "\"src/foo.sv\"");
}

#[test]
fn line_expands_to_int_literal() {
    let out = pp("`__LINE__\n");
    assert!(out.errors.is_empty(), "errors: {:?}", out.errors);
    assert_eq!(out.expanded_text.trim(), "1");
    let tok = out.tokens.iter().find(|t| t.kind != SyntaxKind::Eof);
    assert_eq!(tok.map(|t| t.kind), Some(SyntaxKind::IntLiteral));
}

#[test]
fn line_tracks_position() {
    let out = pp("\n\n`__LINE__\n");
    assert!(out.errors.is_empty(), "errors: {:?}", out.errors);
    assert!(
        out.expanded_text.contains('3'),
        "expected line 3, got: {:?}",
        out.expanded_text,
    );
}

#[test]
fn line_different_lines() {
    let out = pp("`__LINE__\n\n`__LINE__\n");
    assert!(out.errors.is_empty(), "errors: {:?}", out.errors);
    let text = out.expanded_text.trim();
    assert!(text.contains('1'), "expected line 1 in: {text:?}");
    assert!(text.contains('3'), "expected line 3 in: {text:?}");
}

#[test]
fn skipped_inside_false_ifdef() {
    let out = pp("`ifdef UNDEF\n`__FILE__\n`endif\n");
    assert!(out.errors.is_empty(), "errors: {:?}", out.errors);
    assert!(!out.expanded_text.contains("test.sv"));
}

#[test]
fn define_predefined_produces_error() {
    let out = pp("`define __FILE__ foo\n");
    assert_eq!(out.errors.len(), 1);
    assert!(
        out.errors[0].message.contains("cannot redefine"),
        "msg: {}",
        out.errors[0].message,
    );
}

#[test]
fn undef_predefined_produces_error() {
    let out = pp("`undef __LINE__\n");
    assert_eq!(out.errors.len(), 1);
    assert!(
        out.errors[0].message.contains("cannot undefine"),
        "msg: {}",
        out.errors[0].message,
    );
}

#[test]
fn ifdef_predefined_is_true() {
    let out = pp("`ifdef __FILE__\nyes\n`endif\n");
    assert!(out.errors.is_empty(), "errors: {:?}", out.errors);
    assert!(out.expanded_text.contains("yes"));
}

#[test]
fn ifndef_predefined_is_false() {
    let out = pp("`ifndef __LINE__\nyes\n`endif\n");
    assert!(out.errors.is_empty(), "errors: {:?}", out.errors);
    assert!(!out.expanded_text.contains("yes"));
}

#[test]
fn nested_in_macro_body_uses_call_site() {
    let out = pp("`define M `__LINE__\n\n\n`M\n");
    assert!(out.errors.is_empty(), "errors: {:?}", out.errors);
    assert!(
        out.expanded_text.contains('4'),
        "expected call-site line 4, got: {:?}",
        out.expanded_text,
    );
}

#[test]
fn nested_file_in_macro_body() {
    let out = pp("`define F `__FILE__\n`F\n");
    assert!(out.errors.is_empty(), "errors: {:?}", out.errors);
    assert!(
        out.expanded_text.contains("\"test.sv\""),
        "got: {:?}",
        out.expanded_text,
    );
}
