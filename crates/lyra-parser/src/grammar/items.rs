use lyra_lexer::SyntaxKind;

use crate::parser::Parser;

use super::declarations;
use super::expressions;
use super::ports;
use super::statements;

// Parse a package declaration: `package [lifetime] name ; { item } endpackage [: name]`
pub(crate) fn package_decl(p: &mut Parser) {
    let m = p.start();
    p.bump(); // package

    // Optional lifetime: automatic | static
    if p.at(SyntaxKind::AutomaticKw) || p.at(SyntaxKind::StaticKw) {
        p.bump();
    }

    // Package name
    p.expect(SyntaxKind::Ident);
    p.expect(SyntaxKind::Semicolon);

    // Package body
    let body = p.start();
    while !p.at(SyntaxKind::EndpackageKw) && !p.at_end() {
        if !package_item(p) {
            break;
        }
    }
    body.complete(p, SyntaxKind::PackageBody);

    if !p.eat(SyntaxKind::EndpackageKw) {
        p.error("expected `endpackage`");
    }

    // Optional `: name`
    if p.eat(SyntaxKind::Colon) && p.at(SyntaxKind::Ident) {
        p.bump();
    }

    m.complete(p, SyntaxKind::PackageDecl);
}

// Parse one package item. Returns false if no progress was made.
fn package_item(p: &mut Parser) -> bool {
    match p.current() {
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
        SyntaxKind::ImportKw => {
            import_decl(p);
            true
        }
        SyntaxKind::TypedefKw => {
            declarations::typedef_decl(p);
            true
        }
        _ => {
            p.error_bump("unexpected token in package body");
            !p.at_end()
        }
    }
}

// Parse an import declaration: `import pkg::sym, pkg::* ;`
pub(crate) fn import_decl(p: &mut Parser) {
    let m = p.start();
    p.bump(); // import

    import_item(p);
    while p.eat(SyntaxKind::Comma) {
        import_item(p);
    }

    p.expect(SyntaxKind::Semicolon);
    m.complete(p, SyntaxKind::ImportDecl);
}

fn import_item(p: &mut Parser) {
    let m = p.start();
    if !p.at(SyntaxKind::Ident) {
        // Malformed: sync to semicolon
        while !p.at(SyntaxKind::Semicolon) && !p.at_end() {
            p.error_bump("expected package name");
        }
        m.complete(p, SyntaxKind::ImportItem);
        return;
    }

    // Check if this is `pkg :: sym` (non-wildcard) or `pkg :: *` (wildcard)
    if p.nth(1) == SyntaxKind::ColonColon && p.nth(2) == SyntaxKind::Star {
        // Wildcard: pkg :: *
        p.bump(); // pkg
        p.bump(); // ::
        p.bump(); // *
    } else if p.nth(1) == SyntaxKind::ColonColon && p.nth(2) == SyntaxKind::Ident {
        // Non-wildcard: wrap in QualifiedName
        let qn = p.start();
        p.bump(); // pkg
        p.bump(); // ::
        p.bump(); // sym
        qn.complete(p, SyntaxKind::QualifiedName);
    } else {
        // Malformed: just consume the ident and sync
        p.error_bump("expected `::` after package name");
    }

    m.complete(p, SyntaxKind::ImportItem);
}

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
        SyntaxKind::ImportKw => {
            import_decl(p);
            true
        }
        SyntaxKind::TypedefKw => {
            declarations::typedef_decl(p);
            true
        }
        SyntaxKind::GenerateKw => {
            generate_region(p);
            true
        }
        SyntaxKind::IfKw => {
            generate_if(p);
            true
        }
        SyntaxKind::ForKw => {
            generate_for(p);
            true
        }
        SyntaxKind::CaseKw | SyntaxKind::CasexKw | SyntaxKind::CasezKw => {
            generate_case(p);
            true
        }
        SyntaxKind::BeginKw => {
            generate_block(p);
            true
        }
        SyntaxKind::GenvarKw => {
            genvar_decl(p);
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

// Parse an interface declaration: `interface [lifetime] name [#(params)] [(ports)] ; { item } endinterface [: name]`
pub(crate) fn interface_decl(p: &mut Parser) {
    let m = p.start();
    p.bump(); // interface

    // Optional lifetime: automatic | static
    if p.at(SyntaxKind::AutomaticKw) || p.at(SyntaxKind::StaticKw) {
        p.bump();
    }

    // Interface name
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

    // Interface body: items until endinterface
    let body = p.start();
    while !p.at(SyntaxKind::EndinterfaceKw) && !p.at_end() {
        if !module_item(p) {
            break;
        }
    }
    body.complete(p, SyntaxKind::InterfaceBody);

    if !p.eat(SyntaxKind::EndinterfaceKw) {
        p.error("expected `endinterface`");
    }

    // Optional `: name`
    if p.eat(SyntaxKind::Colon) && p.at(SyntaxKind::Ident) {
        p.bump();
    }

    m.complete(p, SyntaxKind::InterfaceDecl);
}

// Parse a program declaration: `program [lifetime] name [#(params)] [(ports)] ; { item } endprogram [: name]`
pub(crate) fn program_decl(p: &mut Parser) {
    let m = p.start();
    p.bump(); // program

    // Optional lifetime: automatic | static
    if p.at(SyntaxKind::AutomaticKw) || p.at(SyntaxKind::StaticKw) {
        p.bump();
    }

    // Program name
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

    // Program body: items until endprogram
    let body = p.start();
    while !p.at(SyntaxKind::EndprogramKw) && !p.at_end() {
        if !module_item(p) {
            break;
        }
    }
    body.complete(p, SyntaxKind::ProgramBody);

    if !p.eat(SyntaxKind::EndprogramKw) {
        p.error("expected `endprogram`");
    }

    // Optional `: name`
    if p.eat(SyntaxKind::Colon) && p.at(SyntaxKind::Ident) {
        p.bump();
    }

    m.complete(p, SyntaxKind::ProgramDecl);
}

// Parse a primitive (UDP) declaration: `primitive name (ports) ; ... endprimitive [: name]`
pub(crate) fn primitive_decl(p: &mut Parser) {
    let m = p.start();
    p.bump(); // primitive

    // Primitive name
    p.expect(SyntaxKind::Ident);

    // Port list (...)
    if p.at(SyntaxKind::LParen) {
        ports::port_decl_list(p);
    }

    p.expect(SyntaxKind::Semicolon);

    // Opaque body: skip tokens until endprimitive
    while !p.at(SyntaxKind::EndprimitiveKw) && !p.at_end() {
        p.bump();
    }

    if !p.eat(SyntaxKind::EndprimitiveKw) {
        p.error("expected `endprimitive`");
    }

    // Optional `: name`
    if p.eat(SyntaxKind::Colon) && p.at(SyntaxKind::Ident) {
        p.bump();
    }

    m.complete(p, SyntaxKind::PrimitiveDecl);
}

// Parse a config declaration: `config name ; ... endconfig [: name]`
pub(crate) fn config_decl(p: &mut Parser) {
    let m = p.start();
    p.bump(); // config

    // Config name
    p.expect(SyntaxKind::Ident);
    p.expect(SyntaxKind::Semicolon);

    // Opaque body: skip tokens until endconfig
    while !p.at(SyntaxKind::EndconfigKw) && !p.at_end() {
        p.bump();
    }

    if !p.eat(SyntaxKind::EndconfigKw) {
        p.error("expected `endconfig`");
    }

    // Optional `: name`
    if p.eat(SyntaxKind::Colon) && p.at(SyntaxKind::Ident) {
        p.bump();
    }

    m.complete(p, SyntaxKind::ConfigDecl);
}

// `generate { module_item* } endgenerate`
fn generate_region(p: &mut Parser) {
    let m = p.start();
    p.bump(); // generate
    while !p.at(SyntaxKind::EndgenerateKw) && !p.at_end() {
        if !module_item(p) {
            break;
        }
    }
    if !p.eat(SyntaxKind::EndgenerateKw) {
        p.error("expected `endgenerate`");
    }
    m.complete(p, SyntaxKind::GenerateRegion);
}

// `if (expr) generate_body [else generate_body]`
fn generate_if(p: &mut Parser) {
    let m = p.start();
    p.bump(); // if
    p.expect(SyntaxKind::LParen);
    expressions::expr(p);
    p.expect(SyntaxKind::RParen);
    generate_body(p);
    if p.eat(SyntaxKind::ElseKw) {
        generate_body(p);
    }
    m.complete(p, SyntaxKind::IfStmt);
}

// `for (genvar i = 0; i < N; i = i + 1) generate_body`
fn generate_for(p: &mut Parser) {
    let m = p.start();
    p.bump(); // for
    p.expect(SyntaxKind::LParen);
    // Init: `genvar i = expr` or `i = expr`
    if !p.at(SyntaxKind::Semicolon) {
        if p.at(SyntaxKind::GenvarKw) {
            p.bump(); // genvar
        }
        expressions::expr_for_stmt(p);
        if p.at(SyntaxKind::Assign) {
            p.bump();
            expressions::expr(p);
        }
    }
    p.expect(SyntaxKind::Semicolon);
    // Condition
    if !p.at(SyntaxKind::Semicolon) {
        expressions::expr(p);
    }
    p.expect(SyntaxKind::Semicolon);
    // Step
    if !p.at(SyntaxKind::RParen) {
        expressions::expr_for_stmt(p);
        if p.at(SyntaxKind::Assign) {
            p.bump();
            expressions::expr(p);
        }
    }
    p.expect(SyntaxKind::RParen);
    generate_body(p);
    m.complete(p, SyntaxKind::ForStmt);
}

// `case (expr) { case_item } endcase`
fn generate_case(p: &mut Parser) {
    let m = p.start();
    p.bump(); // case / casex / casez
    p.expect(SyntaxKind::LParen);
    expressions::expr(p);
    p.expect(SyntaxKind::RParen);
    while !p.at(SyntaxKind::EndcaseKw) && !p.at_end() && !at_gen_end(p) {
        generate_case_item(p);
    }
    if !p.eat(SyntaxKind::EndcaseKw) {
        p.error("expected `endcase`");
    }
    m.complete(p, SyntaxKind::CaseStmt);
}

fn generate_case_item(p: &mut Parser) {
    let m = p.start();
    if p.eat(SyntaxKind::DefaultKw) {
        p.eat(SyntaxKind::Colon);
        generate_body(p);
    } else {
        expressions::expr(p);
        while p.eat(SyntaxKind::Comma) {
            expressions::expr(p);
        }
        p.expect(SyntaxKind::Colon);
        generate_body(p);
    }
    m.complete(p, SyntaxKind::CaseItem);
}

// `begin [:label] { module_item* } end [:label]`
fn generate_block(p: &mut Parser) {
    let m = p.start();
    p.bump(); // begin
    if p.eat(SyntaxKind::Colon) && p.at(SyntaxKind::Ident) {
        p.bump();
    }
    while !p.at(SyntaxKind::EndKw) && !p.at_end() && !at_gen_end(p) {
        if !module_item(p) {
            break;
        }
    }
    if !p.eat(SyntaxKind::EndKw) {
        p.error("expected `end`");
    }
    if p.eat(SyntaxKind::Colon) && p.at(SyntaxKind::Ident) {
        p.bump();
    }
    m.complete(p, SyntaxKind::BlockStmt);
}

// Generate body: either a begin..end block or a single module_item
fn generate_body(p: &mut Parser) {
    if p.at(SyntaxKind::BeginKw) {
        generate_block(p);
    } else {
        module_item(p);
    }
}

// `genvar ident [, ident]* ;`
fn genvar_decl(p: &mut Parser) {
    let m = p.start();
    p.bump(); // genvar
    p.expect(SyntaxKind::Ident);
    while p.eat(SyntaxKind::Comma) {
        p.expect(SyntaxKind::Ident);
    }
    p.expect(SyntaxKind::Semicolon);
    m.complete(p, SyntaxKind::GenvarDecl);
}

fn at_gen_end(p: &Parser) -> bool {
    p.at(SyntaxKind::EndmoduleKw)
        || p.at(SyntaxKind::EndgenerateKw)
        || p.at(SyntaxKind::EndinterfaceKw)
        || p.at(SyntaxKind::EndprogramKw)
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
