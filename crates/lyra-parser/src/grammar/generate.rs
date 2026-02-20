use lyra_lexer::SyntaxKind;

use crate::parser::Parser;

use super::expressions;
use super::items;

// `generate { module_item* } endgenerate`
pub(super) fn generate_region(p: &mut Parser) {
    let m = p.start();
    p.bump(); // generate
    while !p.at(SyntaxKind::EndgenerateKw) && !p.at_end() {
        if !items::module_item(p) {
            break;
        }
    }
    if !p.eat(SyntaxKind::EndgenerateKw) {
        p.error("expected `endgenerate`");
    }
    m.complete(p, SyntaxKind::GenerateRegion);
}

// `if (expr) generate_body [else generate_body]`
pub(super) fn generate_if(p: &mut Parser) {
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
pub(super) fn generate_for(p: &mut Parser) {
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
pub(super) fn generate_case(p: &mut Parser) {
    let m = p.start();
    p.bump(); // case / casex / casez
    p.expect(SyntaxKind::LParen);
    expressions::expr(p);
    p.expect(SyntaxKind::RParen);
    while !p.at(SyntaxKind::EndcaseKw) && !p.at_end() && !at_gen_end(p) {
        let cp = p.checkpoint();
        generate_case_item(p);
        if !p.has_progressed(cp) {
            p.error_bump("expected case item");
        }
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
pub(super) fn generate_block(p: &mut Parser) {
    let m = p.start();
    p.bump(); // begin
    if p.eat(SyntaxKind::Colon) && p.at(SyntaxKind::Ident) {
        p.bump();
    }
    while !p.at(SyntaxKind::EndKw) && !p.at_end() && !at_gen_end(p) {
        if !items::module_item(p) {
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
        items::module_item(p);
    }
}

// `genvar ident [, ident]* ;`
pub(super) fn genvar_decl(p: &mut Parser) {
    let m = p.start();
    p.bump(); // genvar
    p.expect(SyntaxKind::Ident);
    while p.eat(SyntaxKind::Comma) {
        p.expect(SyntaxKind::Ident);
    }
    p.expect(SyntaxKind::Semicolon);
    m.complete(p, SyntaxKind::GenvarDecl);
}

pub(super) fn at_gen_end(p: &Parser) -> bool {
    p.at(SyntaxKind::EndmoduleKw)
        || p.at(SyntaxKind::EndgenerateKw)
        || p.at(SyntaxKind::EndinterfaceKw)
        || p.at(SyntaxKind::EndprogramKw)
        || p.at(SyntaxKind::EndfunctionKw)
        || p.at(SyntaxKind::EndtaskKw)
}
