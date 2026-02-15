use lyra_lexer::SyntaxKind;

use crate::parser::Parser;

use super::declarations;
use super::expressions;
use super::ports;
use super::statements;

// Parse a module declaration: `module name [#(params)] [(ports)] ; { item } endmodule`
pub(crate) fn module_decl(p: &mut Parser) {
    let m = p.start();
    p.bump(); // module

    // Optional lifetime: automatic | static
    if p.at(SyntaxKind::AutomaticKw) || p.at(SyntaxKind::StaticKw) {
        p.bump();
    }

    // Module name
    p.expect(SyntaxKind::Ident);

    // Optional parameter port list #(...)
    if p.at(SyntaxKind::Hash) && p.nth(1) == SyntaxKind::LParen {
        ports::param_port_list(p);
    }

    // Optional port list (...)
    if p.at(SyntaxKind::LParen) {
        ports::port_decl_list(p);
    }

    p.expect(SyntaxKind::Semicolon);

    // Module body: items until endmodule
    let body = p.start();
    while !p.at(SyntaxKind::EndmoduleKw) && !p.at_end() {
        if !module_item(p) {
            break;
        }
    }
    body.complete(p, SyntaxKind::ModuleBody);

    if !p.eat(SyntaxKind::EndmoduleKw) {
        p.error("expected `endmodule`");
    }

    // Optional `: name`
    if p.eat(SyntaxKind::Colon) && p.at(SyntaxKind::Ident) {
        p.bump();
    }

    m.complete(p, SyntaxKind::ModuleDecl);
}

// Parse one module item. Returns false if no progress was made.
fn module_item(p: &mut Parser) -> bool {
    match p.current() {
        SyntaxKind::AssignKw => {
            continuous_assign(p);
            true
        }
        SyntaxKind::AlwaysKw
        | SyntaxKind::AlwaysCombKw
        | SyntaxKind::AlwaysFfKw
        | SyntaxKind::AlwaysLatchKw => {
            always_block(p);
            true
        }
        SyntaxKind::InitialKw => {
            initial_block(p);
            true
        }
        SyntaxKind::ParameterKw | SyntaxKind::LocalparamKw => {
            declarations::param_decl(p);
            true
        }
        k if is_net_type(k) => {
            declarations::net_decl(p);
            true
        }
        k if declarations::is_data_type_keyword(k) => {
            declarations::var_decl(p);
            true
        }
        SyntaxKind::InputKw | SyntaxKind::OutputKw | SyntaxKind::InoutKw => {
            declarations::var_decl(p);
            true
        }
        SyntaxKind::Ident => {
            // Could be module instantiation or declaration with user-defined type.
            // Heuristic: Ident followed by Ident is module_inst or typedef-based decl.
            // Ident followed by #( is module instantiation with parameters.
            if p.nth(1) == SyntaxKind::Hash {
                module_instantiation(p);
            } else if p.nth(1) == SyntaxKind::Ident {
                // Ident Ident: could be decl or instantiation.
                // If third significant token is `(`, likely instantiation.
                if p.nth(2) == SyntaxKind::LParen {
                    module_instantiation(p);
                } else {
                    declarations::var_decl(p);
                }
            } else {
                // Bare identifier -- skip as error
                p.error_bump("unexpected token in module body");
            }
            true
        }
        _ => {
            p.error_bump("unexpected token in module body");
            // Return true because we consumed a token (error_bump does that).
            !p.at_end()
        }
    }
}

// `assign lhs = rhs ;`
fn continuous_assign(p: &mut Parser) {
    let m = p.start();
    p.bump(); // assign
    expressions::expr(p);
    p.expect(SyntaxKind::Assign);
    expressions::expr(p);
    // Handle additional assignments: `, lhs = rhs`
    while p.eat(SyntaxKind::Comma) {
        expressions::expr(p);
        p.expect(SyntaxKind::Assign);
        expressions::expr(p);
    }
    p.expect(SyntaxKind::Semicolon);
    m.complete(p, SyntaxKind::ContinuousAssign);
}

// `always_keyword statement`
fn always_block(p: &mut Parser) {
    let m = p.start();
    p.bump(); // always / always_comb / always_ff / always_latch
    statements::stmt(p);
    m.complete(p, SyntaxKind::AlwaysBlock);
}

// `initial statement`
fn initial_block(p: &mut Parser) {
    let m = p.start();
    p.bump(); // initial
    statements::stmt(p);
    m.complete(p, SyntaxKind::InitialBlock);
}

// Module instantiation: `mod_name [#(params)] inst_name (ports) ;`
fn module_instantiation(p: &mut Parser) {
    let m = p.start();
    p.bump(); // module name (Ident)

    // Optional parameter overrides #(...)
    if p.at(SyntaxKind::Hash) && p.nth(1) == SyntaxKind::LParen {
        param_override_list(p);
    }

    // Instance name
    p.expect(SyntaxKind::Ident);

    // Port connections (...)
    if p.at(SyntaxKind::LParen) {
        instance_port_list(p);
    }

    // Additional instances: `, name (...)`
    while p.eat(SyntaxKind::Comma) {
        p.expect(SyntaxKind::Ident);
        if p.at(SyntaxKind::LParen) {
            instance_port_list(p);
        }
    }

    p.expect(SyntaxKind::Semicolon);
    m.complete(p, SyntaxKind::ModuleInstantiation);
}

fn instance_port_list(p: &mut Parser) {
    let m = p.start();
    p.bump(); // (
    if !p.at(SyntaxKind::RParen) {
        instance_port(p);
        while p.eat(SyntaxKind::Comma) {
            instance_port(p);
        }
    }
    p.expect(SyntaxKind::RParen);
    m.complete(p, SyntaxKind::InstancePortList);
}

fn instance_port(p: &mut Parser) {
    let m = p.start();
    if p.at(SyntaxKind::Dot) {
        // Named: .port_name(expr)
        p.bump(); // .
        p.expect(SyntaxKind::Ident);
        if p.at(SyntaxKind::LParen) {
            p.bump(); // (
            if !p.at(SyntaxKind::RParen) {
                expressions::expr(p);
            }
            p.expect(SyntaxKind::RParen);
        }
    } else if p.at(SyntaxKind::DotStar) {
        // Wildcard: .*
        p.bump();
    } else {
        // Positional
        expressions::expr(p);
    }
    m.complete(p, SyntaxKind::InstancePort);
}

// Parameter value override list for instantiation: `#( .name(expr), ... )` or `#( expr, ... )`
fn param_override_list(p: &mut Parser) {
    let m = p.start();
    p.bump(); // #
    p.expect(SyntaxKind::LParen);
    if !p.at(SyntaxKind::RParen) {
        param_override(p);
        while p.eat(SyntaxKind::Comma) {
            param_override(p);
        }
    }
    p.expect(SyntaxKind::RParen);
    m.complete(p, SyntaxKind::ParamPortList);
}

fn param_override(p: &mut Parser) {
    let m = p.start();
    if p.at(SyntaxKind::Dot) {
        // Named: .NAME(expr)
        p.bump(); // .
        p.expect(SyntaxKind::Ident);
        if p.at(SyntaxKind::LParen) {
            p.bump(); // (
            if !p.at(SyntaxKind::RParen) {
                expressions::expr(p);
            }
            p.expect(SyntaxKind::RParen);
        }
    } else {
        // Positional
        expressions::expr(p);
    }
    m.complete(p, SyntaxKind::InstancePort);
}

fn is_net_type(kind: SyntaxKind) -> bool {
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
