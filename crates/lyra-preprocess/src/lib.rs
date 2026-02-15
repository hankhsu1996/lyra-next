/// Preprocessor placeholder.
///
/// Will handle `` `define ``, `` `ifdef ``, `` `include `` etc.
/// For now, passes tokens through unchanged (clones the slice).
/// The real implementation will produce a new token stream in place.
pub fn preprocess(tokens: &[lyra_lexer::Token]) -> Vec<lyra_lexer::Token> {
    tokens.to_vec()
}
