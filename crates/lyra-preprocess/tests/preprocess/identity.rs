use lyra_lexer::Token;
use lyra_preprocess::preprocess;
use lyra_source::{FileId, TextRange, TextSize};

fn sample_tokens() -> Vec<Token> {
    lyra_lexer::lex("module top; endmodule")
}

#[test]
fn passthrough_tokens() {
    let tokens = sample_tokens();
    let output = preprocess(FileId(0), &tokens);
    assert_eq!(output.tokens, tokens);
}

#[test]
fn identity_source_map() {
    let output = preprocess(FileId(7), &sample_tokens());
    let range = TextRange::new(TextSize::new(0), TextSize::new(6));
    let span = output.source_map.map_span(range);
    assert_eq!(span.file, FileId(7));
    assert_eq!(span.range, range);
}

#[test]
fn empty_include_graph() {
    let output = preprocess(FileId(0), &sample_tokens());
    assert!(output.includes.is_empty());
    assert!(output.includes.dependencies().is_empty());
}
