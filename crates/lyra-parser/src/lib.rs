mod event;
mod grammar;
pub(crate) mod parser;

use lyra_lexer::Token;
use rowan::Language;

pub use lyra_lexer::SyntaxKind;
use lyra_source::TextRange;

use crate::event::Event;

/// The `SystemVerilog` language tag for rowan.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SvLanguage {}

impl Language for SvLanguage {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> SyntaxKind {
        assert!(
            raw.0 <= SyntaxKind::ErrorNode as u16,
            "invalid SyntaxKind value: {}",
            raw.0
        );
        // SAFETY: SyntaxKind is repr(u16) with contiguous variants.
        unsafe { std::mem::transmute(raw.0) }
    }

    fn kind_to_raw(kind: SyntaxKind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind as u16)
    }
}

pub type SyntaxNode = rowan::SyntaxNode<SvLanguage>;
pub type SyntaxToken = rowan::SyntaxToken<SvLanguage>;
pub type SyntaxElement = rowan::SyntaxElement<SvLanguage>;

/// A parse error with source range and message.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    pub range: TextRange,
    pub message: String,
}

/// Result of parsing a source file.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parse {
    pub green: rowan::GreenNode,
    pub errors: Vec<ParseError>,
}

impl Parse {
    pub fn syntax(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green.clone())
    }
}

/// Parse a list of tokens into a lossless green tree rooted at `SourceFile`.
pub fn parse(tokens: &[Token], src: &str) -> Parse {
    let mut p = parser::Parser::new(tokens);
    grammar::source_file(&mut p);
    let (events, errors) = p.finish();
    let green = replay(&events, tokens, src);
    Parse { green, errors }
}

// Convert parser events into a rowan green tree.
fn replay(events: &[Event], tokens: &[Token], src: &str) -> rowan::GreenNode {
    let mut builder = rowan::GreenNodeBuilder::new();
    let mut token_pos = 0usize;
    let mut cursor = 0usize;
    let mut processed = vec![false; events.len()];
    let mut forward_chain = Vec::new();

    for i in 0..events.len() {
        if processed[i] {
            continue;
        }

        match &events[i] {
            Event::Start {
                kind,
                forward_parent,
            } => {
                forward_chain.clear();
                forward_chain.push(*kind);
                let mut idx = i;
                let mut fp = *forward_parent;
                while let Some(delta) = fp {
                    idx += delta as usize;
                    processed[idx] = true;
                    if let Event::Start {
                        kind,
                        forward_parent,
                    } = &events[idx]
                    {
                        forward_chain.push(*kind);
                        fp = *forward_parent;
                    } else {
                        break;
                    }
                }
                // Start nodes from outermost to innermost
                for kind in forward_chain.iter().rev() {
                    builder.start_node(SvLanguage::kind_to_raw(*kind));
                }
            }
            Event::Finish => {
                builder.finish_node();
            }
            Event::Token { n_raw_tokens } => {
                for _ in 0..*n_raw_tokens {
                    if token_pos >= tokens.len() {
                        break;
                    }
                    let tok = tokens[token_pos];
                    if tok.kind == SyntaxKind::Eof {
                        break;
                    }
                    let tok_len: usize = tok.len.into();
                    let text = &src[cursor..cursor + tok_len];
                    builder.token(SvLanguage::kind_to_raw(tok.kind), text);
                    cursor += tok_len;
                    token_pos += 1;
                }
            }
        }
    }

    builder.finish()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn roundtrip_trivia() {
        let src = "// header\nmodule foo ; endmodule\n";
        let tokens = lyra_lexer::lex(src);
        let parse = parse(&tokens, src);
        let root = parse.syntax();
        assert_eq!(root.text().to_string(), src);
    }

    #[test]
    fn roundtrip_two_modules() {
        let src = "module a; endmodule\nmodule b; endmodule\n";
        let tokens = lyra_lexer::lex(src);
        let parse = parse(&tokens, src);
        let root = parse.syntax();
        assert_eq!(root.text().to_string(), src);
    }

    #[test]
    fn roundtrip_ports() {
        let src =
            "module top #(parameter WIDTH = 8) (input logic [7:0] a, output logic b); endmodule\n";
        let tokens = lyra_lexer::lex(src);
        let parse = parse(&tokens, src);
        let root = parse.syntax();
        assert_eq!(root.text().to_string(), src);
    }

    #[test]
    fn roundtrip_always() {
        let src = "module top; always_comb begin a = b; end endmodule\n";
        let tokens = lyra_lexer::lex(src);
        let parse = parse(&tokens, src);
        let root = parse.syntax();
        assert_eq!(root.text().to_string(), src);
    }

    #[test]
    fn error_recovery_missing_endmodule() {
        let src = "module top;";
        let tokens = lyra_lexer::lex(src);
        let parse = parse(&tokens, src);
        assert!(!parse.errors.is_empty());
        // Still roundtrips
        assert_eq!(parse.syntax().text().to_string(), src);
    }
}
