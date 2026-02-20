use lyra_lexer::SyntaxKind;

use crate::parser::Parser;

use super::declarations;
use super::expressions;
use super::generate;
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
    super::eat_attr_instances(p);
    match p.current() {
        SyntaxKind::ParameterKw | SyntaxKind::LocalparamKw => {
            declarations::param_decl(p);
            true
        }
        SyntaxKind::ImportKw => {
            import_decl(p);
            true
        }
        SyntaxKind::ExportKw => {
            export_decl(p);
            true
        }
        SyntaxKind::TypedefKw => {
            declarations::typedef_decl(p);
            true
        }
        SyntaxKind::FunctionKw => {
            function_decl(p);
            true
        }
        SyntaxKind::TaskKw => {
            task_decl(p);
            true
        }
        _ if declarations::at_unambiguous_data_decl_start(p) => {
            if is_net_type(p.current()) {
                declarations::net_decl(p);
            } else {
                declarations::var_decl(p);
            }
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

// Parse an export declaration: `export pkg::sym, pkg::*, *::* ;`
pub(crate) fn export_decl(p: &mut Parser) {
    let m = p.start();
    p.bump(); // export
    export_item(p);
    while p.eat(SyntaxKind::Comma) {
        export_item(p);
    }
    p.expect(SyntaxKind::Semicolon);
    m.complete(p, SyntaxKind::ExportDecl);
}

fn export_item(p: &mut Parser) {
    let m = p.start();
    if p.at(SyntaxKind::Star) && p.nth(1) == SyntaxKind::ColonColon && p.nth(2) == SyntaxKind::Star
    {
        // *::*
        p.bump(); // *
        p.bump(); // ::
        p.bump(); // *
    } else if p.at(SyntaxKind::Ident) && p.nth(1) == SyntaxKind::ColonColon {
        p.bump(); // pkg
        p.bump(); // ::
        if p.at(SyntaxKind::Star) {
            p.bump(); // *
        } else if p.at(SyntaxKind::Ident) {
            p.bump(); // name
        } else {
            p.error("expected identifier or `*` after `::`");
        }
    } else {
        p.error_bump("expected export item");
    }
    m.complete(p, SyntaxKind::ExportItem);
}

// Parse a module declaration: `module name [import ...;]* [#(params)] [(ports)] ; { item } endmodule`
pub(crate) fn module_decl(p: &mut Parser) {
    let m = p.start();
    p.bump(); // module

    // Optional lifetime: automatic | static
    if p.at(SyntaxKind::AutomaticKw) || p.at(SyntaxKind::StaticKw) {
        p.bump();
    }

    // Module name
    p.expect(SyntaxKind::Ident);

    // Header-level package imports (LRM 26.4)
    while p.at(SyntaxKind::ImportKw) {
        import_decl(p);
    }

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
pub(super) fn module_item(p: &mut Parser) -> bool {
    super::eat_attr_instances(p);
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
        SyntaxKind::ImportKw => {
            import_decl(p);
            true
        }
        SyntaxKind::TypedefKw => {
            declarations::typedef_decl(p);
            true
        }
        SyntaxKind::GenerateKw => {
            generate::generate_region(p);
            true
        }
        SyntaxKind::IfKw => {
            generate::generate_if(p);
            true
        }
        SyntaxKind::ForKw => {
            generate::generate_for(p);
            true
        }
        SyntaxKind::CaseKw | SyntaxKind::CasexKw | SyntaxKind::CasezKw => {
            generate::generate_case(p);
            true
        }
        SyntaxKind::BeginKw => {
            generate::generate_block(p);
            true
        }
        SyntaxKind::GenvarKw => {
            generate::genvar_decl(p);
            true
        }
        SyntaxKind::FunctionKw => {
            function_decl(p);
            true
        }
        SyntaxKind::TaskKw => {
            task_decl(p);
            true
        }
        SyntaxKind::ModportKw => {
            modport_decl(p);
            true
        }
        _ if declarations::at_unambiguous_data_decl_start(p) => {
            if is_net_type(p.current()) {
                declarations::net_decl(p);
            } else {
                declarations::var_decl(p);
            }
            true
        }
        SyntaxKind::Ident => {
            // Ambiguous: bare Ident not caught by classifier (no ColonColon).
            // Heuristic: Ident # -> instantiation, Ident Ident ( -> instantiation.
            if p.nth(1) == SyntaxKind::Hash {
                module_instantiation(p);
            } else if p.nth(1) == SyntaxKind::Ident {
                if p.nth(2) == SyntaxKind::LParen {
                    module_instantiation(p);
                } else {
                    declarations::var_decl(p);
                }
            } else {
                p.error_bump("unexpected token in module body");
            }
            true
        }
        _ => {
            p.error_bump("unexpected token in module body");
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
    super::eat_attr_instances(p);
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

    // Header-level package imports (LRM 26.4)
    while p.at(SyntaxKind::ImportKw) {
        import_decl(p);
    }

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

    // Header-level package imports (LRM 26.4)
    while p.at(SyntaxKind::ImportKw) {
        import_decl(p);
    }

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

// `function [lifetime] return_type name ( tf_port_list ) ; { stmt/decl } endfunction [: name]`
pub(crate) fn function_decl(p: &mut Parser) {
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
pub(crate) fn task_decl(p: &mut Parser) {
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
fn modport_decl(p: &mut Parser) {
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
