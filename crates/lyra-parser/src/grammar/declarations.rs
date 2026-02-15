use lyra_lexer::SyntaxKind;

use crate::parser::Parser;

use super::expressions;

// `parameter` or `localparam` declaration as a module item (with semicolon).
pub(crate) fn param_decl(p: &mut Parser) {
    let m = p.start();
    p.bump(); // parameter | localparam
    if is_data_type_keyword(p.current())
        || p.at(SyntaxKind::SignedKw)
        || p.at(SyntaxKind::UnsignedKw)
    {
        type_spec(p);
    }
    p.expect(SyntaxKind::Ident);
    if p.eat(SyntaxKind::Assign) {
        expressions::expr(p);
    }
    // Additional declarators: `, name = expr`
    while p.eat(SyntaxKind::Comma) {
        p.expect(SyntaxKind::Ident);
        if p.eat(SyntaxKind::Assign) {
            expressions::expr(p);
        }
    }
    p.expect(SyntaxKind::Semicolon);
    m.complete(p, SyntaxKind::ParamDecl);
}

// Net declaration: `wire [7:0] a, b ;`
pub(crate) fn net_decl(p: &mut Parser) {
    let m = p.start();
    p.bump(); // net type keyword (wire, tri, etc.)
    // Optional signing
    if p.at(SyntaxKind::SignedKw) || p.at(SyntaxKind::UnsignedKw) {
        p.bump();
    }
    // Optional packed dimensions
    while p.at(SyntaxKind::LBracket) {
        packed_dimension(p);
    }
    // Declarators
    net_declarator(p);
    while p.eat(SyntaxKind::Comma) {
        net_declarator(p);
    }
    p.expect(SyntaxKind::Semicolon);
    m.complete(p, SyntaxKind::NetDecl);
}

fn net_declarator(p: &mut Parser) {
    p.expect(SyntaxKind::Ident);
    while p.at(SyntaxKind::LBracket) {
        unpacked_dimension(p);
    }
    if p.eat(SyntaxKind::Assign) {
        expressions::expr(p);
    }
}

// Variable declaration: `logic [7:0] a = 0, b ;`
// Also handles declarations starting with direction keywords or user-defined types.
pub(crate) fn var_decl(p: &mut Parser) {
    let m = p.start();
    // Optional direction for port-like declarations in module body
    if matches!(
        p.current(),
        SyntaxKind::InputKw | SyntaxKind::OutputKw | SyntaxKind::InoutKw
    ) {
        p.bump();
    }
    // Optional var keyword
    if p.at(SyntaxKind::VarKw) {
        p.bump();
    }
    // Type
    type_spec(p);
    // Declarators
    var_declarator(p);
    while p.eat(SyntaxKind::Comma) {
        var_declarator(p);
    }
    p.expect(SyntaxKind::Semicolon);
    m.complete(p, SyntaxKind::VarDecl);
}

fn var_declarator(p: &mut Parser) {
    p.expect(SyntaxKind::Ident);
    while p.at(SyntaxKind::LBracket) {
        unpacked_dimension(p);
    }
    if p.eat(SyntaxKind::Assign) {
        expressions::expr(p);
    }
}

// Parse a type specifier: `logic`, `reg`, `bit`, `logic [7:0]`, `Ident`, etc.
pub(crate) fn type_spec(p: &mut Parser) {
    let m = p.start();
    if is_data_type_keyword(p.current()) {
        p.bump();
    } else if p.at(SyntaxKind::Ident) {
        p.bump(); // user-defined type
    } else {
        p.error("expected type");
        m.abandon(p);
        return;
    }
    // Optional signing
    if p.at(SyntaxKind::SignedKw) || p.at(SyntaxKind::UnsignedKw) {
        p.bump();
    }
    // Packed dimensions
    while p.at(SyntaxKind::LBracket) {
        packed_dimension(p);
    }
    m.complete(p, SyntaxKind::TypeSpec);
}

pub(crate) fn packed_dimension(p: &mut Parser) {
    let m = p.start();
    p.bump(); // [
    if !p.at(SyntaxKind::RBracket) {
        expressions::expr(p);
        if p.eat(SyntaxKind::Colon) {
            expressions::expr(p);
        }
    }
    p.expect(SyntaxKind::RBracket);
    m.complete(p, SyntaxKind::PackedDimension);
}

fn unpacked_dimension(p: &mut Parser) {
    let m = p.start();
    p.bump(); // [
    if !p.at(SyntaxKind::RBracket) {
        expressions::expr(p);
        if p.eat(SyntaxKind::Colon) {
            expressions::expr(p);
        }
    }
    p.expect(SyntaxKind::RBracket);
    m.complete(p, SyntaxKind::UnpackedDimension);
}

pub(crate) fn is_data_type_keyword(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::LogicKw
            | SyntaxKind::RegKw
            | SyntaxKind::BitKw
            | SyntaxKind::IntegerKw
            | SyntaxKind::IntKw
            | SyntaxKind::ShortintKw
            | SyntaxKind::LongintKw
            | SyntaxKind::ByteKw
            | SyntaxKind::TimeKw
            | SyntaxKind::RealtimeKw
            | SyntaxKind::RealKw
            | SyntaxKind::ShortRealKw
            | SyntaxKind::StringKw
            | SyntaxKind::ChandleKw
            | SyntaxKind::EventKw
            | SyntaxKind::VoidKw
    )
}
