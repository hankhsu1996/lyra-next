use lyra_lexer::SyntaxKind;

// Events emitted by the parser, converted to a green tree during replay.
#[derive(Debug)]
pub(crate) enum Event {
    Start {
        kind: SyntaxKind,
        forward_parent: Option<u32>,
    },
    Finish,
    // Consume `n_raw_tokens` from the token stream (trivia + one significant).
    Token {
        n_raw_tokens: u16,
    },
}
