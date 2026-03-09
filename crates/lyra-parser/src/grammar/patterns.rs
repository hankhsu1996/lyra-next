// Pattern parsing (LRM 12.6).
//
// Patterns are a first-class syntax family. Each pattern form gets its own
// SyntaxKind node, and the grammar does not route patterns through expression
// machinery.

use lyra_lexer::SyntaxKind;

use crate::parser::Parser;

use super::expressions;

// Parse a pattern.
//
// pattern ::=
//     '.' '*'                                     -- WildcardPattern
//   | '.' variable_identifier                     -- BindPattern
//   | tagged member_identifier [ pattern ]        -- TaggedPattern
//   | '\'' '{' pattern_fields '}'                 -- StructPattern
//   | '(' pattern ')'                             -- ParenPattern
//   | constant_expression                         -- ConstantPattern
pub(crate) fn pattern(p: &mut Parser) {
    match p.current() {
        // `.*` is lexed as a single DotStar token
        SyntaxKind::DotStar => {
            let m = p.start();
            p.bump(); // .*
            m.complete(p, SyntaxKind::WildcardPattern);
        }
        SyntaxKind::Dot => {
            if p.nth(1) == SyntaxKind::Star {
                wildcard_pattern(p);
            } else if matches!(p.nth(1), SyntaxKind::Ident | SyntaxKind::EscapedIdent) {
                bind_pattern(p);
            } else {
                // Error recovery: consume dot and emit error
                let m = p.start();
                p.bump(); // .
                p.error("expected `*` or identifier after `.` in pattern");
                m.complete(p, SyntaxKind::ConstantPattern);
            }
        }
        SyntaxKind::TaggedKw => tagged_pattern(p),
        SyntaxKind::TickBrace => struct_pattern(p),
        SyntaxKind::LParen => paren_pattern(p),
        _ => constant_pattern(p),
    }
}

// `.*`
fn wildcard_pattern(p: &mut Parser) {
    let m = p.start();
    p.bump(); // .
    p.bump(); // *
    m.complete(p, SyntaxKind::WildcardPattern);
}

// `.variable_identifier`
fn bind_pattern(p: &mut Parser) {
    let m = p.start();
    p.bump(); // .
    p.bump(); // identifier
    m.complete(p, SyntaxKind::BindPattern);
}

// `tagged member_identifier [pattern]`
fn tagged_pattern(p: &mut Parser) {
    let m = p.start();
    p.bump(); // tagged
    p.expect(SyntaxKind::Ident); // member_identifier
    // Optional inner pattern (if next token can start a pattern)
    if can_start_pattern(p) {
        pattern(p);
    }
    m.complete(p, SyntaxKind::TaggedPattern);
}

// `'{` pattern_field { , pattern_field } `}`
fn struct_pattern(p: &mut Parser) {
    let m = p.start();
    p.bump(); // '{
    if !p.at(SyntaxKind::RBrace) {
        pattern_field(p);
        while p.eat(SyntaxKind::Comma) {
            pattern_field(p);
        }
    }
    p.expect(SyntaxKind::RBrace);
    m.complete(p, SyntaxKind::StructPattern);
}

fn pattern_field(p: &mut Parser) {
    let m = p.start();
    // member_name : pattern | default : pattern | positional pattern
    if p.at(SyntaxKind::DefaultKw) && p.nth(1) == SyntaxKind::Colon {
        p.bump(); // default
        p.bump(); // :
        pattern(p);
    } else if (p.at(SyntaxKind::Ident) || p.at(SyntaxKind::EscapedIdent))
        && p.nth(1) == SyntaxKind::Colon
    {
        p.bump(); // member_name
        p.bump(); // :
        pattern(p);
    } else {
        pattern(p);
    }
    m.complete(p, SyntaxKind::PatternField);
}

// `( pattern )`
fn paren_pattern(p: &mut Parser) {
    let m = p.start();
    p.bump(); // (
    pattern(p);
    p.expect(SyntaxKind::RParen);
    m.complete(p, SyntaxKind::ParenPattern);
}

// Constant expression pattern: wraps a restricted expression form.
fn constant_pattern(p: &mut Parser) {
    let m = p.start();
    expressions::expr(p);
    m.complete(p, SyntaxKind::ConstantPattern);
}

// Whether the current token can start a pattern.
fn can_start_pattern(p: &Parser) -> bool {
    match p.current() {
        SyntaxKind::Dot
        | SyntaxKind::DotStar
        | SyntaxKind::TaggedKw
        | SyntaxKind::TickBrace
        | SyntaxKind::LParen
        // Tokens that can start a constant expression
        | SyntaxKind::Ident
        | SyntaxKind::SystemIdent
        | SyntaxKind::IntLiteral
        | SyntaxKind::BasedLiteralPrefix
        | SyntaxKind::RealLiteral
        | SyntaxKind::UnbasedUnsizedLiteral
        | SyntaxKind::StringLiteral
        | SyntaxKind::TimeLiteral
        | SyntaxKind::LBrace
        | SyntaxKind::Plus
        | SyntaxKind::Minus
        | SyntaxKind::Bang
        | SyntaxKind::Tilde => true,
        _ => false,
    }
}
