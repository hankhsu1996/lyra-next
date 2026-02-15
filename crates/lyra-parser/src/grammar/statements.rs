use lyra_lexer::SyntaxKind;

use crate::parser::Parser;

use super::declarations;
use super::expressions;

// Parse a single statement.
pub(crate) fn stmt(p: &mut Parser) {
    // Timing control prefix: `@(...)` or `#delay`
    if p.at(SyntaxKind::At) || p.at(SyntaxKind::Hash) {
        timing_control_stmt(p);
        return;
    }

    match p.current() {
        SyntaxKind::BeginKw => block_stmt(p),
        SyntaxKind::IfKw => if_stmt(p),
        SyntaxKind::UniqueKw | SyntaxKind::Unique0Kw | SyntaxKind::PriorityKw => {
            // Look ahead: modifier can precede `if` or `case`/`casex`/`casez`
            if is_case_keyword(p.nth(1)) {
                case_stmt(p);
            } else {
                if_stmt(p);
            }
        }
        SyntaxKind::CaseKw | SyntaxKind::CasexKw | SyntaxKind::CasezKw => case_stmt(p),
        SyntaxKind::ForKw => for_stmt(p),
        SyntaxKind::WhileKw => while_stmt(p),
        SyntaxKind::RepeatKw => repeat_stmt(p),
        SyntaxKind::ForeverKw => forever_stmt(p),
        // Local declaration in procedural context
        k if declarations::is_data_type_keyword(k) => declarations::var_decl(p),
        SyntaxKind::Semicolon => {
            // Null statement
            p.bump();
        }
        _ => {
            // Expression statement (assignments, calls)
            expr_stmt(p);
        }
    }
}

// `begin [:label] { stmt } end [:label]`
fn block_stmt(p: &mut Parser) {
    let m = p.start();
    p.bump(); // begin
    // Optional label `: name`
    if p.eat(SyntaxKind::Colon) && p.at(SyntaxKind::Ident) {
        p.bump();
    }
    while !p.at(SyntaxKind::EndKw) && !p.at_end() && !at_block_end(p) {
        stmt(p);
    }
    if !p.eat(SyntaxKind::EndKw) {
        p.error("expected `end`");
    }
    // Optional end label
    if p.eat(SyntaxKind::Colon) && p.at(SyntaxKind::Ident) {
        p.bump();
    }
    m.complete(p, SyntaxKind::BlockStmt);
}

// `[unique|priority] if (expr) stmt [else stmt]`
fn if_stmt(p: &mut Parser) {
    let m = p.start();
    // Optional unique/priority modifier
    if p.at(SyntaxKind::UniqueKw) || p.at(SyntaxKind::Unique0Kw) || p.at(SyntaxKind::PriorityKw) {
        p.bump();
    }
    p.expect(SyntaxKind::IfKw);
    p.expect(SyntaxKind::LParen);
    expressions::expr(p);
    p.expect(SyntaxKind::RParen);
    stmt(p);
    if p.eat(SyntaxKind::ElseKw) {
        stmt(p);
    }
    m.complete(p, SyntaxKind::IfStmt);
}

// `[unique|priority] case[xz] (expr) { case_item } endcase`
fn case_stmt(p: &mut Parser) {
    let m = p.start();
    // Optional unique/unique0/priority modifier
    if matches!(
        p.current(),
        SyntaxKind::UniqueKw | SyntaxKind::Unique0Kw | SyntaxKind::PriorityKw
    ) {
        p.bump();
    }
    p.bump(); // case / casex / casez
    p.expect(SyntaxKind::LParen);
    expressions::expr(p);
    p.expect(SyntaxKind::RParen);
    while !p.at(SyntaxKind::EndcaseKw) && !p.at_end() && !at_block_end(p) {
        case_item(p);
    }
    if !p.eat(SyntaxKind::EndcaseKw) {
        p.error("expected `endcase`");
    }
    m.complete(p, SyntaxKind::CaseStmt);
}

fn case_item(p: &mut Parser) {
    let m = p.start();
    if p.eat(SyntaxKind::DefaultKw) {
        // default: stmt
        p.eat(SyntaxKind::Colon);
        stmt(p);
    } else {
        // expr {, expr} : stmt
        expressions::expr(p);
        while p.eat(SyntaxKind::Comma) {
            expressions::expr(p);
        }
        p.expect(SyntaxKind::Colon);
        stmt(p);
    }
    m.complete(p, SyntaxKind::CaseItem);
}

// `for (init; cond; step) stmt`
fn for_stmt(p: &mut Parser) {
    let m = p.start();
    p.bump(); // for
    p.expect(SyntaxKind::LParen);
    // Init: could be a declaration or assignment
    if !p.at(SyntaxKind::Semicolon) {
        if declarations::is_data_type_keyword(p.current()) {
            // `int i = 0` style
            for_var_decl(p);
        } else {
            expressions::expr_for_stmt(p);
            if p.at(SyntaxKind::Assign) || is_assign_op(p.current()) || p.at(SyntaxKind::LtEq) {
                p.bump();
                expressions::expr(p);
            }
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
        if p.at(SyntaxKind::Assign) || is_assign_op(p.current()) || p.at(SyntaxKind::LtEq) {
            p.bump();
            expressions::expr(p);
        }
    }
    p.expect(SyntaxKind::RParen);
    stmt(p);
    m.complete(p, SyntaxKind::ForStmt);
}

// Variable declaration inside for-init: `int i = 0`
fn for_var_decl(p: &mut Parser) {
    let m = p.start();
    declarations::type_spec(p);
    p.expect(SyntaxKind::Ident);
    if p.eat(SyntaxKind::Assign) {
        expressions::expr(p);
    }
    m.complete(p, SyntaxKind::VarDecl);
}

// `while (expr) stmt`
fn while_stmt(p: &mut Parser) {
    let m = p.start();
    p.bump(); // while
    p.expect(SyntaxKind::LParen);
    expressions::expr(p);
    p.expect(SyntaxKind::RParen);
    stmt(p);
    m.complete(p, SyntaxKind::WhileStmt);
}

// `repeat (expr) stmt`
fn repeat_stmt(p: &mut Parser) {
    let m = p.start();
    p.bump(); // repeat
    p.expect(SyntaxKind::LParen);
    expressions::expr(p);
    p.expect(SyntaxKind::RParen);
    stmt(p);
    m.complete(p, SyntaxKind::RepeatStmt);
}

// `forever stmt`
fn forever_stmt(p: &mut Parser) {
    let m = p.start();
    p.bump(); // forever
    stmt(p);
    m.complete(p, SyntaxKind::ForeverStmt);
}

// Timing control: `@(event_expr) stmt` or `#delay stmt`
fn timing_control_stmt(p: &mut Parser) {
    let m = p.start();
    if p.at(SyntaxKind::At) {
        event_control(p);
    } else {
        // # delay
        p.bump(); // #
        if p.at(SyntaxKind::LParen) {
            p.bump();
            expressions::expr(p);
            p.expect(SyntaxKind::RParen);
        } else if p.at(SyntaxKind::IntLiteral) || p.at(SyntaxKind::RealLiteral) {
            p.bump();
        }
    }
    // The controlled statement
    stmt(p);
    m.complete(p, SyntaxKind::TimingControl);
}

fn event_control(p: &mut Parser) {
    let m = p.start();
    p.bump(); // @
    if p.at(SyntaxKind::LParen) {
        p.bump(); // (
        if p.at(SyntaxKind::Star) {
            p.bump(); // *
        } else {
            event_expr(p);
        }
        p.expect(SyntaxKind::RParen);
    } else if p.at(SyntaxKind::Star) {
        p.bump(); // @*
    }
    m.complete(p, SyntaxKind::EventExpr);
}

fn event_expr(p: &mut Parser) {
    event_item(p);
    while p.at(SyntaxKind::Comma) || p.at(SyntaxKind::OrKw) {
        p.bump(); // , or `or`
        event_item(p);
    }
}

fn event_item(p: &mut Parser) {
    let m = p.start();
    // Optional edge: posedge / negedge / edge
    if p.at(SyntaxKind::PosedgeKw) || p.at(SyntaxKind::NegedgeKw) || p.at(SyntaxKind::EdgeKw) {
        p.bump();
    }
    expressions::expr(p);
    m.complete(p, SyntaxKind::EventItem);
}

// Expression statement: `expr ;` or `lhs = rhs ;` or `lhs <= rhs ;`
fn expr_stmt(p: &mut Parser) {
    let m = p.start();
    // Use StmtLhs mode so `<=` stops the expression for NBA handling
    if expressions::expr_for_stmt(p).is_none() {
        p.error_bump("expected statement");
        m.abandon(p);
        return;
    }
    // Check for assignment operators (including NBA `<=`)
    if p.at(SyntaxKind::Assign) || is_assign_op(p.current()) || p.at(SyntaxKind::LtEq) {
        p.bump(); // assignment operator
        expressions::expr(p);
    }
    p.expect(SyntaxKind::Semicolon);
    m.complete(p, SyntaxKind::AssignStmt);
}

fn is_assign_op(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::PlusEq
            | SyntaxKind::MinusEq
            | SyntaxKind::StarEq
            | SyntaxKind::SlashEq
            | SyntaxKind::PercentEq
            | SyntaxKind::AmpEq
            | SyntaxKind::PipeEq
            | SyntaxKind::CaretEq
            | SyntaxKind::LtLtEq
            | SyntaxKind::GtGtEq
            | SyntaxKind::LtLtLtEq
            | SyntaxKind::GtGtGtEq
    )
}

fn is_case_keyword(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::CaseKw | SyntaxKind::CasexKw | SyntaxKind::CasezKw
    )
}

// Check if we're at a token that closes an outer construct, to prevent runaway.
fn at_block_end(p: &Parser) -> bool {
    p.at(SyntaxKind::EndmoduleKw)
}
