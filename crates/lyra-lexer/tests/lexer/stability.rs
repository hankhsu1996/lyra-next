use super::common::{kinds, lex_kinds};
use lyra_lexer::{NODE_START, SyntaxKind, lex};

#[test]
fn token_ordinal_stability() {
    // Token ordinals must never change across releases.
    // If this test fails, a variant was inserted or reordered.
    assert_eq!(SyntaxKind::Whitespace as u16, 0);
    assert_eq!(SyntaxKind::Semicolon as u16, 3);
    assert_eq!(SyntaxKind::Ident as u16, 22);
    assert_eq!(SyntaxKind::ModuleKw as u16, 23);
    assert_eq!(SyntaxKind::Error as u16, 39);
    assert_eq!(SyntaxKind::Eof as u16, 40);
    // First new token appended after Eof
    assert_eq!(SyntaxKind::Bang as u16, 41);
    // NODE_START must be after all token variants
    assert!(NODE_START > SyntaxKind::XorKw as u16);
    assert!(NODE_START == SyntaxKind::__NodeStart as u16);
    assert_eq!(SyntaxKind::SourceFile as u16, NODE_START + 1);
}

#[test]
fn roundtrip_lex_all_text() {
    let inputs = [
        "module foo; endmodule",
        "assign a = b + c;",
        "4'b1001 8'hFF 'x 'z",
        "1.2 3.14e-2 23E10",
        "\\bus+index $display `define",
        "== != === !== ==? !=? << >> <<< >>> <-> ->>",
        "\"hello\" \"\"\"triple\"\"\"",
        "// comment\n/* block */",
    ];
    for input in inputs {
        let tokens = lex(input);
        let mut reconstructed = String::new();
        let mut pos = 0usize;
        for tok in &tokens {
            if tok.kind == SyntaxKind::Eof {
                break;
            }
            let len: usize = tok.len.into();
            reconstructed.push_str(&input[pos..pos + len]);
            pos += len;
        }
        assert_eq!(reconstructed, input, "roundtrip failed for: {input:?}");
    }
}

#[test]
fn lex_module_header() {
    assert_eq!(
        kinds("module foo;"),
        vec![
            SyntaxKind::ModuleKw,
            SyntaxKind::Whitespace,
            SyntaxKind::Ident,
            SyntaxKind::Semicolon,
        ]
    );
}

#[test]
fn trivia_preserved() {
    let tokens = lex("// comment\nmodule");
    assert_eq!(tokens[0].kind, SyntaxKind::LineComment);
    assert_eq!(tokens[1].kind, SyntaxKind::Whitespace);
    assert_eq!(tokens[2].kind, SyntaxKind::ModuleKw);
}

#[test]
fn block_comment_unterminated() {
    let k = lex_kinds("/* unterminated");
    assert_eq!(k[0], (SyntaxKind::BlockComment, "/* unterminated"));
}
