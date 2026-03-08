use lyra_preprocess::{MacroEnv, PreprocessInputs, preprocess};
use lyra_source::FileId;

struct NoIncludes;

impl lyra_preprocess::IncludeProvider for NoIncludes {
    fn resolve(&self, _: &str) -> Option<lyra_preprocess::ResolvedInclude<'_>> {
        None
    }
}

fn pp(text: &str) -> lyra_preprocess::PreprocOutput {
    let tokens = lyra_lexer::lex_with_mode(text, lyra_lexer::LexMode::Preprocess);
    preprocess(&PreprocessInputs {
        file: FileId(0),
        tokens: &tokens,
        text,
        provider: &NoIncludes,
        starting_env: &MacroEnv::empty(),
        macro_recursion_limit: PreprocessInputs::DEFAULT_RECURSION_LIMIT,
        file_path: "",
    })
}

// Stringify: basic parameter substitution
#[test]
fn stringify_basic() {
    let out = pp("`define MSG(x) `\"Hello x`\"\n`MSG(world)\n");
    assert!(out.errors.is_empty(), "errors: {:?}", out.errors);
    assert!(
        out.expanded_text.contains("\"Hello world\""),
        "expanded: {:?}",
        out.expanded_text
    );
}

// Stringify: escaped quote
#[test]
fn stringify_escaped_quote() {
    let out = pp("`define MSG(x,y) `\"x: `\\`\"y`\\`\"`\"\n`MSG(left,right)\n");
    assert!(out.errors.is_empty(), "errors: {:?}", out.errors);
    assert!(
        out.expanded_text.contains("\"left: \\\"right\\\"\""),
        "expanded: {:?}",
        out.expanded_text
    );
}

// Stringify: empty
#[test]
fn stringify_empty() {
    let out = pp("`define Q `\"`\"\n`Q\n");
    assert!(out.errors.is_empty(), "errors: {:?}", out.errors);
    assert!(
        out.expanded_text.contains("\"\""),
        "expanded: {:?}",
        out.expanded_text
    );
}

// Concat: basic identifier construction
#[test]
fn concat_basic() {
    let out = pp("`define APPEND(f) f``_primary\n`APPEND(clock)\n");
    assert!(out.errors.is_empty(), "errors: {:?}", out.errors);
    assert!(
        out.expanded_text.contains("clock_primary"),
        "expanded: {:?}",
        out.expanded_text
    );
}

// Concat: whitespace around operator is absorbed
#[test]
fn concat_strips_whitespace() {
    let out = pp("`define JOIN(a,b) a `` b\n`JOIN(foo,bar)\n");
    assert!(out.errors.is_empty(), "errors: {:?}", out.errors);
    assert!(
        out.expanded_text.contains("foobar"),
        "expanded: {:?}",
        out.expanded_text
    );
}

// Concat: inside stringify (both operators compose)
#[test]
fn concat_in_stringify() {
    let out = pp("`define FIELD(p,n) `\"p``n`\"\n`FIELD(my_,field)\n");
    assert!(out.errors.is_empty(), "errors: {:?}", out.errors);
    assert!(
        out.expanded_text.contains("\"my_field\""),
        "expanded: {:?}",
        out.expanded_text
    );
}

// Concat: object-like macro
#[test]
fn concat_object_like() {
    let out = pp("`define PREFIX clock``_primary\n`PREFIX\n");
    assert!(out.errors.is_empty(), "errors: {:?}", out.errors);
    assert!(
        out.expanded_text.contains("clock_primary"),
        "expanded: {:?}",
        out.expanded_text
    );
}

// Error: escaped quote outside stringify diagnosed
#[test]
fn escaped_quote_outside_stringify_diagnosed() {
    let out = pp("`define BAD x`\\`\"y\n`BAD\n");
    assert!(
        out.errors
            .iter()
            .any(|e| e.message.contains("outside stringify")),
        "errors: {:?}",
        out.errors
    );
}

// Error: concat at end of body diagnosed
#[test]
fn concat_missing_rhs_diagnosed() {
    let out = pp("`define BAD foo``\n`BAD\n");
    assert!(
        out.errors
            .iter()
            .any(|e| e.message.contains("end of macro body")),
        "errors: {:?}",
        out.errors
    );
}

// No macro operator tokens should appear in output
#[test]
fn no_operator_tokens_in_output() {
    let out = pp("`define MSG(x) `\"Hello x`\"\n`MSG(world)\n");
    for t in &out.tokens {
        assert!(
            !matches!(
                t.kind,
                lyra_lexer::SyntaxKind::MacroStringify
                    | lyra_lexer::SyntaxKind::MacroConcat
                    | lyra_lexer::SyntaxKind::MacroEscapedQuote
            ),
            "macro operator token in output: {t:?}",
        );
    }
}

// Token lengths must sum to expanded text length
#[test]
fn token_lengths_sum_to_expanded_text() {
    let out = pp("`define MSG(x) `\"Hello x`\"\n`MSG(world)\n");
    let total: u32 = out
        .tokens
        .iter()
        .filter(|t| t.kind != lyra_lexer::SyntaxKind::Eof)
        .map(|t| u32::from(t.len))
        .sum();
    assert_eq!(
        total as usize,
        out.expanded_text.len(),
        "token lengths should sum to expanded text length"
    );
}

// Concat: numeric suffix construction
#[test]
fn concat_numeric_suffix() {
    let out = pp("`define WIRE(n) wire``n\n`WIRE(8)\n");
    assert!(out.errors.is_empty(), "errors: {:?}", out.errors);
    assert!(
        out.expanded_text.contains("wire8"),
        "expanded: {:?}",
        out.expanded_text
    );
}

// Stringify: preserves whitespace inside
#[test]
fn stringify_preserves_internal_whitespace() {
    let out = pp("`define S(a,b) `\"a  b`\"\n`S(x,y)\n");
    assert!(out.errors.is_empty(), "errors: {:?}", out.errors);
    assert!(
        out.expanded_text.contains("\"x  y\""),
        "expanded: {:?}",
        out.expanded_text
    );
}

// Nested macro expansion inside stringify
#[test]
fn stringify_with_nested_macro() {
    let out = pp("`define INNER world\n`define MSG(x) `\"Hello x`\"\n`MSG(`INNER)\n");
    assert!(out.errors.is_empty(), "errors: {:?}", out.errors);
    // Nested `INNER is expanded to "world" before stringify
    assert!(
        out.expanded_text.contains("\"Hello world\""),
        "expanded: {:?}",
        out.expanded_text
    );
}
