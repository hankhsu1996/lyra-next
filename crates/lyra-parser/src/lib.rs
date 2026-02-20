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
            raw.0 < SyntaxKind::__Last as u16,
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
                    idx += delta;
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

    #[test]
    fn escaped_ident_normalized_in_lookahead() {
        let src = r"module m; int \foo ; endmodule";
        let tokens = lyra_lexer::lex(src);
        let p = parser::Parser::new(&tokens);
        // Skip `module` and `m` and `;` (positions 0, 1, 2 after trivia skip)
        // Find the escaped ident in the token stream
        let mut found = false;
        for n in 0..20 {
            if p.nth(n) == SyntaxKind::Eof {
                break;
            }
            // nth() should never return EscapedIdent
            assert_ne!(
                p.nth(n),
                SyntaxKind::EscapedIdent,
                "nth({n}) returned EscapedIdent; normalization failed"
            );
            found = true;
        }
        assert!(found);
    }

    #[test]
    fn escaped_ident_parses_as_declaration() {
        // Plain expect(Ident) must accept escaped identifiers via normalization
        let src = r"module m; int \busa+index ; endmodule";
        let tokens = lyra_lexer::lex(src);
        let parse = parse(&tokens, src);
        assert!(
            parse.errors.is_empty(),
            "escaped ident should parse without errors: {:?}",
            parse.errors,
        );
        assert_eq!(parse.syntax().text().to_string(), src);
    }

    #[test]
    fn escaped_ident_preserved_in_cst() {
        let src = r"module m; int \foo ; endmodule";
        let tokens = lyra_lexer::lex(src);
        let parse = parse(&tokens, src);
        assert!(parse.errors.is_empty());
        let root = parse.syntax();
        // Walk all tokens and find the EscapedIdent
        let has_escaped = root
            .descendants_with_tokens()
            .filter_map(|el| el.into_token())
            .any(|tok| tok.kind() == SyntaxKind::EscapedIdent && tok.text() == r"\foo");
        assert!(
            has_escaped,
            "CST must preserve EscapedIdent token kind for roundtrip fidelity"
        );
    }

    // Helper: find the first node of a given kind in the tree
    fn find_node(root: &SyntaxNode, kind: SyntaxKind) -> Option<SyntaxNode> {
        if root.kind() == kind {
            return Some(root.clone());
        }
        for child in root.children() {
            if let Some(found) = find_node(&child, kind) {
                return Some(found);
            }
        }
        None
    }

    fn parse_module(src: &str) -> (Parse, SyntaxNode) {
        let full = format!("module m; {src} endmodule");
        let tokens = lyra_lexer::lex(&full);
        let p = parse(&tokens, &full);
        let root = p.syntax();
        (
            Parse {
                green: p.green.clone(),
                errors: p.errors.clone(),
            },
            root,
        )
    }

    #[test]
    fn system_tf_call_with_type_arg() {
        let (p, root) = parse_module("parameter P = $bits(logic [7:0]);");
        assert!(p.errors.is_empty(), "errors: {:?}", p.errors);
        assert_eq!(
            root.text().to_string().as_str(),
            "module m; parameter P = $bits(logic [7:0]); endmodule"
        );
        let call = find_node(&root, SyntaxKind::SystemTfCall);
        assert!(call.is_some(), "should have SystemTfCall node");
        let call = call.unwrap();
        // SystemIdent is a direct child token
        let has_sys = call
            .children_with_tokens()
            .filter_map(|el| el.into_token())
            .any(|tok| tok.kind() == SyntaxKind::SystemIdent && tok.text() == "$bits");
        assert!(
            has_sys,
            "SystemTfCall should contain $bits SystemIdent token"
        );
        // Should have SystemTfArgList child
        let arg_list = call
            .children()
            .find(|c| c.kind() == SyntaxKind::SystemTfArgList);
        assert!(arg_list.is_some(), "should have SystemTfArgList");
        // TypeSpec inside arg list
        let ts = find_node(&arg_list.unwrap(), SyntaxKind::TypeSpec);
        assert!(
            ts.is_some(),
            "arg list should contain TypeSpec for `logic [7:0]`"
        );
    }

    #[test]
    fn system_tf_call_simple_type() {
        let (p, root) = parse_module("parameter P = $bits(int);");
        assert!(p.errors.is_empty(), "errors: {:?}", p.errors);
        let call = find_node(&root, SyntaxKind::SystemTfCall);
        assert!(call.is_some());
    }

    #[test]
    fn system_tf_call_user_type_as_expr() {
        // User-defined type name parses as expression (NameRef), not TypeSpec
        let (p, root) = parse_module("parameter P = $bits(foo_t);");
        assert!(p.errors.is_empty(), "errors: {:?}", p.errors);
        let call = find_node(&root, SyntaxKind::SystemTfCall).unwrap();
        let arg_list = call
            .children()
            .find(|c| c.kind() == SyntaxKind::SystemTfArgList)
            .unwrap();
        let name_ref = find_node(&arg_list, SyntaxKind::NameRef);
        assert!(
            name_ref.is_some(),
            "foo_t should parse as NameRef expression"
        );
        let ts = find_node(&arg_list, SyntaxKind::TypeSpec);
        assert!(ts.is_none(), "foo_t should NOT parse as TypeSpec");
    }

    #[test]
    fn system_tf_call_display() {
        let (p, root) = parse_module("initial begin $display(\"hello\"); end");
        assert!(p.errors.is_empty(), "errors: {:?}", p.errors);
        let call = find_node(&root, SyntaxKind::SystemTfCall);
        assert!(call.is_some(), "$display(...) should be SystemTfCall");
    }

    #[test]
    fn system_ident_without_parens_is_name_ref() {
        let (p, root) = parse_module("initial begin $finish; end");
        assert!(p.errors.is_empty(), "errors: {:?}", p.errors);
        let call = find_node(&root, SyntaxKind::SystemTfCall);
        assert!(
            call.is_none(),
            "$finish without parens should NOT be SystemTfCall"
        );
        let nr = find_node(&root, SyntaxKind::NameRef);
        assert!(nr.is_some(), "$finish should be NameRef");
    }

    #[test]
    fn system_tf_call_clog2() {
        let (p, root) = parse_module("parameter P = $clog2(4);");
        assert!(p.errors.is_empty(), "errors: {:?}", p.errors);
        let call = find_node(&root, SyntaxKind::SystemTfCall);
        assert!(call.is_some());
    }

    #[test]
    fn normal_call_still_produces_call_expr() {
        let (p, root) = parse_module("initial begin f(1); end");
        assert!(p.errors.is_empty(), "errors: {:?}", p.errors);
        let call = find_node(&root, SyntaxKind::CallExpr);
        assert!(call.is_some(), "f(...) should produce CallExpr");
        let sys_call = find_node(&root, SyntaxKind::SystemTfCall);
        assert!(sys_call.is_none(), "f(...) should NOT be SystemTfCall");
    }

    #[test]
    fn system_tf_call_roundtrip() {
        let src = "module m; parameter P = $bits(logic [7:0]); endmodule";
        let tokens = lyra_lexer::lex(src);
        let p = parse(&tokens, src);
        assert_eq!(p.syntax().text().to_string(), src);
    }
}
