use lyra_lexer::SyntaxKind;

use crate::parser::Parser;

use super::declarations;
use super::expressions;
use super::ports;
use super::statements;

// `function [lifetime] return_type name ( tf_port_list ) ; { stmt/decl } endfunction [: name]`
pub(super) fn function_decl(p: &mut Parser) {
    let m = p.start();
    p.bump(); // function

    // Optional lifetime
    if p.at(SyntaxKind::AutomaticKw) || p.at(SyntaxKind::StaticKw) {
        p.bump();
    }

    // Return type (optional -- implicit `logic` if omitted, LRM 13.4.1).
    // Lookahead: if current is Ident followed by `(` or `;`, the Ident is
    // the function name, not a type. Otherwise consume a type_spec first.
    if p.at(SyntaxKind::Ident)
        && (p.nth(1) == SyntaxKind::LParen || p.nth(1) == SyntaxKind::Semicolon)
    {
        // No explicit return type -- name follows directly
        p.bump(); // function name
    } else {
        declarations::type_spec(p);
        p.expect(SyntaxKind::Ident); // function name
    }

    // Optional port list
    if p.at(SyntaxKind::LParen) {
        tf_port_list(p);
    }

    p.expect(SyntaxKind::Semicolon);

    // Body: statements and declarations until endfunction
    while !p.at(SyntaxKind::EndfunctionKw) && !p.at_end() && !at_func_task_end(p) {
        let cp = p.checkpoint();
        statements::stmt(p);
        if !p.has_progressed(cp) {
            p.error_bump("expected statement");
        }
    }

    if !p.eat(SyntaxKind::EndfunctionKw) {
        p.error("expected `endfunction`");
    }

    // Optional `: name`
    if p.eat(SyntaxKind::Colon) && p.at(SyntaxKind::Ident) {
        p.bump();
    }

    m.complete(p, SyntaxKind::FunctionDecl);
}

// `task [lifetime] name ( tf_port_list ) ; { stmt/decl } endtask [: name]`
pub(super) fn task_decl(p: &mut Parser) {
    let m = p.start();
    p.bump(); // task

    // Optional lifetime
    if p.at(SyntaxKind::AutomaticKw) || p.at(SyntaxKind::StaticKw) {
        p.bump();
    }

    // Task name
    p.expect(SyntaxKind::Ident);

    // Optional port list
    if p.at(SyntaxKind::LParen) {
        tf_port_list(p);
    }

    p.expect(SyntaxKind::Semicolon);

    // Body: statements and declarations until endtask
    while !p.at(SyntaxKind::EndtaskKw) && !p.at_end() && !at_func_task_end(p) {
        let cp = p.checkpoint();
        statements::stmt(p);
        if !p.has_progressed(cp) {
            p.error_bump("expected statement");
        }
    }

    if !p.eat(SyntaxKind::EndtaskKw) {
        p.error("expected `endtask`");
    }

    // Optional `: name`
    if p.eat(SyntaxKind::Colon) && p.at(SyntaxKind::Ident) {
        p.bump();
    }

    m.complete(p, SyntaxKind::TaskDecl);
}

// Parse TF port list: `( [dir] type name {, name} [= default] {; ...} )`
fn tf_port_list(p: &mut Parser) {
    p.bump(); // (

    if !p.at(SyntaxKind::RParen) {
        tf_port_decl(p);
        while p.eat(SyntaxKind::Comma) {
            if p.at(SyntaxKind::RParen) {
                break;
            }
            tf_port_decl(p);
        }
    }

    p.expect(SyntaxKind::RParen);
}

// Parse a single TF port declaration: `[direction] type name {, name} [= default]`
fn tf_port_decl(p: &mut Parser) {
    let m = p.start();

    // Optional direction
    if ports::is_direction(p.current()) {
        p.bump();
    }

    // Type (may be omitted if direction-only with implicit type)
    if declarations::is_data_type_keyword(p.current())
        || (p.at(SyntaxKind::Ident) && p.nth(1) == SyntaxKind::Ident)
        || (p.at(SyntaxKind::Ident) && p.nth(1) == SyntaxKind::ColonColon)
        || (p.at(SyntaxKind::Ident) && p.nth(1) == SyntaxKind::Dot)
    {
        declarations::type_spec(p);
    }

    // First declarator (name + optional default)
    tf_declarator(p);

    // Additional names sharing the same type: `, name [= default]`
    // Only if next comma is followed by a plain Ident (not a direction or type keyword)
    while p.at(SyntaxKind::Comma)
        && !ports::is_direction(p.nth(1))
        && !declarations::is_data_type_keyword(p.nth(1))
        && p.nth(1) == SyntaxKind::Ident
        && p.nth(2) != SyntaxKind::Ident
        && p.nth(2) != SyntaxKind::ColonColon
    {
        p.bump(); // ,
        tf_declarator(p);
    }

    m.complete(p, SyntaxKind::TfPortDecl);
}

fn tf_declarator(p: &mut Parser) {
    let d = p.start();
    p.expect(SyntaxKind::Ident);
    // Optional unpacked dimensions
    while p.at(SyntaxKind::LBracket) {
        declarations::unpacked_dimension(p);
    }
    // Optional default
    if p.eat(SyntaxKind::Assign) {
        expressions::expr(p);
    }
    d.complete(p, SyntaxKind::Declarator);
}

// Outer-construct boundary check for function/task body recovery
fn at_func_task_end(p: &Parser) -> bool {
    p.at(SyntaxKind::EndmoduleKw)
        || p.at(SyntaxKind::EndpackageKw)
        || p.at(SyntaxKind::EndinterfaceKw)
        || p.at(SyntaxKind::EndprogramKw)
        || p.at(SyntaxKind::EndgenerateKw)
}

// `modport` modport_item { `,` modport_item } `;`
pub(super) fn modport_decl(p: &mut Parser) {
    let m = p.start();
    p.bump(); // modport

    modport_item(p);
    while p.eat(SyntaxKind::Comma) {
        modport_item(p);
    }

    p.expect(SyntaxKind::Semicolon);
    m.complete(p, SyntaxKind::ModportDecl);
}

// IDENT `(` modport_port { `,` modport_port } `)`
fn modport_item(p: &mut Parser) {
    let m = p.start();
    p.expect(SyntaxKind::Ident);
    p.expect(SyntaxKind::LParen);

    if !p.at(SyntaxKind::RParen) {
        modport_ports(p);
    }

    p.expect(SyntaxKind::RParen);
    m.complete(p, SyntaxKind::ModportItem);
}

// Parse modport port entries. Each direction keyword applies to subsequent
// names until the next direction keyword.
// Example: `input a, b, output c` -> 3 ModportPort nodes
fn modport_ports(p: &mut Parser) {
    // Current sticky direction
    let mut has_dir = false;

    loop {
        // Accept a new direction keyword (updates the sticky direction)
        if ports::is_direction(p.current()) {
            has_dir = true;
        } else if !has_dir {
            p.error("expected direction keyword");
            break;
        }

        // Parse one port: direction name
        let port = p.start();
        if ports::is_direction(p.current()) {
            p.bump(); // direction
        }
        p.expect(SyntaxKind::Ident);
        port.complete(p, SyntaxKind::ModportPort);

        if !p.eat(SyntaxKind::Comma) {
            break;
        }
        if p.at(SyntaxKind::RParen) {
            break;
        }
    }
}

pub(super) fn is_net_type(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::WireKw
            | SyntaxKind::TriKw
            | SyntaxKind::WandKw
            | SyntaxKind::WorKw
            | SyntaxKind::Tri0Kw
            | SyntaxKind::Tri1Kw
            | SyntaxKind::TriregKw
            | SyntaxKind::Supply0Kw
            | SyntaxKind::Supply1Kw
            | SyntaxKind::UwireKw
    )
}
