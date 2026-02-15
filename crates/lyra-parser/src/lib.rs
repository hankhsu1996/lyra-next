use lyra_lexer::Token;
use rowan::Language;

pub use lyra_lexer::SyntaxKind;

/// The `SystemVerilog` language tag for rowan.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SvLanguage {}

impl Language for SvLanguage {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> SyntaxKind {
        assert!(
            raw.0 <= SyntaxKind::ParamDecl as u16,
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

/// Result of parsing a source file.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parse {
    pub green: rowan::GreenNode,
    pub errors: Vec<lyra_diag::Diagnostic>,
}

impl Parse {
    pub fn syntax(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green.clone())
    }
}

/// Parse a list of tokens into a green tree rooted at `SourceFile`.
pub fn parse(_file_id: lyra_source::FileId, tokens: &[Token], src: &str) -> Parse {
    let mut builder = rowan::GreenNodeBuilder::new();
    let errors = Vec::new();
    let mut pos = 0usize;
    let mut cursor = 0usize;

    builder.start_node(SvLanguage::kind_to_raw(SyntaxKind::SourceFile));

    while pos < tokens.len() {
        let tok = tokens[pos];
        if tok.kind == SyntaxKind::Eof {
            break;
        }

        let tok_len: usize = tok.len.into();
        let tok_text = &src[cursor..cursor + tok_len];

        builder.token(SvLanguage::kind_to_raw(tok.kind), tok_text);
        cursor += tok_len;
        pos += 1;
    }

    builder.finish_node();
    Parse {
        green: builder.finish(),
        errors,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn roundtrip_trivia() {
        let src = "// header\nmodule foo ; endmodule\n";
        let tokens = lyra_lexer::lex(src);
        let parse = parse(lyra_source::FileId(0), &tokens, src);
        let root = parse.syntax();
        // Green tree text must exactly equal original source (trivia preserved)
        assert_eq!(root.text().to_string(), src);
    }
}
