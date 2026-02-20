use lyra_lexer::SyntaxKind;

use crate::parser::Parser;

use super::declarations::type_spec;
use super::expressions;

// Parse parameter port list: `#( param_decl { , param_decl } )`
pub(crate) fn param_port_list(p: &mut Parser) {
    let m = p.start();
    p.bump(); // #
    p.expect(SyntaxKind::LParen);
    if !p.at(SyntaxKind::RParen) {
        param_port_decl(p);
        while p.eat(SyntaxKind::Comma) {
            param_port_decl(p);
        }
    }
    p.expect(SyntaxKind::RParen);
    m.complete(p, SyntaxKind::ParamPortList);
}

// Single parameter declaration inside `#(...)`.
fn param_port_decl(p: &mut Parser) {
    let m = p.start();
    // Optional `parameter` or `localparam` keyword
    if p.at(SyntaxKind::ParameterKw) || p.at(SyntaxKind::LocalparamKw) {
        p.bump();
    }
    // Optional type
    if is_type_start(p.current()) && p.current() != SyntaxKind::Ident {
        type_spec(p);
    }
    // Declarator: name [= expr]
    let d = p.start();
    p.expect(SyntaxKind::Ident);
    if p.eat(SyntaxKind::Assign) {
        expressions::expr(p);
    }
    d.complete(p, SyntaxKind::Declarator);
    m.complete(p, SyntaxKind::ParamDecl);
}

// Parse ANSI port declaration list: `( port_decl { , port_decl } )`
pub(crate) fn port_decl_list(p: &mut Parser) {
    let m = p.start();
    p.bump(); // (
    if !p.at(SyntaxKind::RParen) {
        port_decl(p);
        while p.eat(SyntaxKind::Comma) {
            port_decl(p);
        }
    }
    p.expect(SyntaxKind::RParen);
    m.complete(p, SyntaxKind::PortList);
}

// Single ANSI port declaration.
fn port_decl(p: &mut Parser) {
    let m = p.start();
    super::eat_attr_instances(p);
    // Direction: input / output / inout / ref
    if is_direction(p.current()) {
        p.bump();
    }
    // Optional net type or var keyword
    if p.at(SyntaxKind::WireKw) || p.at(SyntaxKind::VarKw) {
        p.bump();
    }
    // Optional type
    if is_type_start(p.current()) && p.nth(1) != SyntaxKind::RParen && p.nth(1) != SyntaxKind::Comma
    {
        // Only parse type if something follows (the port name)
        if p.current() != SyntaxKind::Ident
            || p.nth(1) == SyntaxKind::Ident
            || p.nth(1) == SyntaxKind::LBracket
        {
            type_spec(p);
        }
    }
    // Port name
    if p.at(SyntaxKind::Ident) {
        p.bump();
    }
    // Unpacked dimensions
    while p.at(SyntaxKind::LBracket) {
        unpacked_dimension(p);
    }
    // Default value
    if p.eat(SyntaxKind::Assign) {
        expressions::expr(p);
    }
    m.complete(p, SyntaxKind::Port);
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

pub(crate) fn is_direction(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::InputKw | SyntaxKind::OutputKw | SyntaxKind::InoutKw | SyntaxKind::RefKw
    )
}

pub(crate) fn is_type_start(kind: SyntaxKind) -> bool {
    super::declarations::is_scalar_type_keyword(kind)
        || matches!(kind, SyntaxKind::WireKw | SyntaxKind::Ident)
}
