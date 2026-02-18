mod declarations;
mod expressions;
mod items;
mod ports;
mod statements;

use lyra_lexer::SyntaxKind;

use crate::parser::Parser;

pub(crate) fn eat_attr_instances(p: &mut Parser) {
    while p.at(SyntaxKind::AttrOpen) {
        attr_instance(p);
    }
}

fn attr_instance(p: &mut Parser) {
    let m = p.start();
    p.bump(); // AttrOpen
    if p.at(SyntaxKind::AttrClose) {
        p.error("expected attribute name");
    } else {
        attr_spec(p);
        while p.eat(SyntaxKind::Comma) {
            attr_spec(p);
        }
    }
    if !p.eat(SyntaxKind::AttrClose) {
        p.error("expected `*)`");
        while !p.at(SyntaxKind::AttrClose) && !at_attr_recovery(p) && !p.at_end() {
            p.bump();
        }
        eat_attr_close(p);
    }
    m.complete(p, SyntaxKind::AttrInst);
}

fn eat_attr_close(p: &mut Parser) -> bool {
    if p.eat(SyntaxKind::AttrClose) {
        return true;
    }
    if p.at(SyntaxKind::Star) && p.nth(1) == SyntaxKind::RParen {
        p.bump(); // *
        p.bump(); // )
        return true;
    }
    false
}

fn at_attr_recovery(p: &Parser) -> bool {
    p.at(SyntaxKind::Semicolon)
        || p.at(SyntaxKind::EndKw)
        || p.at(SyntaxKind::EndmoduleKw)
        || p.at(SyntaxKind::EndpackageKw)
        || p.at(SyntaxKind::EndinterfaceKw)
        || p.at(SyntaxKind::EndprogramKw)
}

fn attr_spec(p: &mut Parser) {
    let m = p.start();
    if p.at(SyntaxKind::Ident) {
        p.bump();
    } else {
        p.error("expected identifier");
    }
    if p.eat(SyntaxKind::Assign) {
        expressions::expr(p);
    }
    m.complete(p, SyntaxKind::AttrSpec);
}

pub(crate) fn source_file(p: &mut Parser) {
    let m = p.start();
    while !p.at_end() {
        eat_attr_instances(p);
        if p.at(SyntaxKind::ModuleKw) {
            items::module_decl(p);
        } else if p.at(SyntaxKind::PackageKw) {
            items::package_decl(p);
        } else if p.at(SyntaxKind::InterfaceKw) {
            items::interface_decl(p);
        } else if p.at(SyntaxKind::ProgramKw) {
            items::program_decl(p);
        } else if p.at(SyntaxKind::PrimitiveKw) {
            items::primitive_decl(p);
        } else if p.at(SyntaxKind::ConfigKw) {
            items::config_decl(p);
        } else {
            p.error_bump("expected top-level declaration");
        }
    }
    // Trailing trivia must be inside the root node for a valid rowan tree.
    p.eat_remaining_trivia();
    m.complete(p, SyntaxKind::SourceFile);
}
