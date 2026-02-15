use lyra_preprocess::preprocess_identity;
use lyra_source::{FileId, TextRange, TextSize};

fn sample_tokens() -> Vec<lyra_lexer::Token> {
    lyra_lexer::lex("module top; endmodule")
}

#[test]
fn passthrough_tokens() {
    let tokens = sample_tokens();
    let output = preprocess_identity(FileId(0), &tokens, "module top; endmodule");
    assert_eq!(output.tokens, tokens);
}

#[test]
fn expanded_text_equals_source() {
    let text = "module top; endmodule";
    let tokens = lyra_lexer::lex(text);
    let output = preprocess_identity(FileId(0), &tokens, text);
    assert_eq!(output.expanded_text, text);
}

#[test]
fn identity_source_map() {
    let text = "module top; endmodule";
    let tokens = lyra_lexer::lex(text);
    let output = preprocess_identity(FileId(7), &tokens, text);
    let range = TextRange::new(TextSize::new(0), TextSize::new(6));
    let span = output.source_map.map_span(range);
    assert_eq!(span.file, FileId(7));
}

#[test]
fn identity_map_point() {
    let text = "module top; endmodule";
    let tokens = lyra_lexer::lex(text);
    let output = preprocess_identity(FileId(3), &tokens, text);
    let span = output
        .source_map
        .map_point(TextSize::new(7))
        .expect("in-bounds offset should map");
    assert_eq!(span.file, FileId(3));
    assert_eq!(span.range.start(), TextSize::new(7));
}

#[test]
fn identity_map_range() {
    let text = "module top; endmodule";
    let tokens = lyra_lexer::lex(text);
    let output = preprocess_identity(FileId(3), &tokens, text);
    let range = TextRange::new(TextSize::new(0), TextSize::new(6));
    let span = output.source_map.map_range(range);
    assert!(span.is_some());
    let span = span.expect("identity map_range should succeed");
    assert_eq!(span.file, FileId(3));
    assert_eq!(span.range, range);
}

#[test]
fn empty_include_graph() {
    let text = "module top; endmodule";
    let tokens = lyra_lexer::lex(text);
    let output = preprocess_identity(FileId(0), &tokens, text);
    assert!(output.includes.is_empty());
    assert!(output.includes.dependencies().is_empty());
}

#[test]
fn no_errors() {
    let text = "module top; endmodule";
    let tokens = lyra_lexer::lex(text);
    let output = preprocess_identity(FileId(0), &tokens, text);
    assert!(output.errors.is_empty());
}

#[test]
fn map_point_out_of_bounds_returns_none() {
    let text = "module top; endmodule";
    let tokens = lyra_lexer::lex(text);
    let output = preprocess_identity(FileId(0), &tokens, text);
    let past_end = TextSize::new(text.len() as u32 + 1);
    assert!(output.source_map.map_point(past_end).is_none());
}
