use lyra_lexer::SyntaxKind;

use crate::parser::Parser;

use super::declarations;
use super::expressions;

// Parse a single statement.
pub(crate) fn stmt(p: &mut Parser) {
    super::eat_attr_instances(p);
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
        SyntaxKind::ForeachKw => foreach_stmt(p),
        SyntaxKind::WhileKw => while_stmt(p),
        SyntaxKind::RepeatKw => repeat_stmt(p),
        SyntaxKind::ForeverKw => forever_stmt(p),
        SyntaxKind::DoKw => do_while_stmt(p),
        SyntaxKind::BreakKw => break_stmt(p),
        SyntaxKind::ContinueKw => continue_stmt(p),
        SyntaxKind::ReturnKw => return_stmt(p),
        // Local declaration in procedural context
        _ if declarations::at_unambiguous_data_decl_start(p) => declarations::var_decl(p),
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
        let cp = p.checkpoint();
        stmt(p);
        if !p.has_progressed(cp) {
            p.error_bump("expected statement");
        }
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

// `[unique|priority] if (cond_predicate) stmt [else stmt]`
fn if_stmt(p: &mut Parser) {
    let m = p.start();
    // Optional unique/priority modifier
    if p.at(SyntaxKind::UniqueKw) || p.at(SyntaxKind::Unique0Kw) || p.at(SyntaxKind::PriorityKw) {
        p.bump();
    }
    p.expect(SyntaxKind::IfKw);
    p.expect(SyntaxKind::LParen);
    expressions::cond_predicate(p);
    p.expect(SyntaxKind::RParen);
    stmt(p);
    if p.eat(SyntaxKind::ElseKw) {
        stmt(p);
    }
    m.complete(p, SyntaxKind::IfStmt);
}

// `[unique|priority] case[xz] (expr) { case_item } endcase`
// `[unique|priority] case (expr) inside { case_inside_item } endcase` (LRM 12.5.4)
// `[unique|priority] case (expr) matches { case_pattern_item } endcase` (LRM 12.6.1)
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
    let is_inside = p.eat(SyntaxKind::InsideKw);
    let is_matches = !is_inside && p.eat(SyntaxKind::MatchesKw);
    while !p.at(SyntaxKind::EndcaseKw) && !p.at_end() && !at_block_end(p) {
        let cp = p.checkpoint();
        if is_inside {
            case_inside_item(p);
        } else if is_matches {
            case_pattern_item(p);
        } else {
            case_item(p);
        }
        if !p.has_progressed(cp) {
            p.error_bump("expected case item");
        }
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

// `case_inside_item ::= range_list : stmt | default [:] stmt` (LRM 12.5.4)
fn case_inside_item(p: &mut Parser) {
    let m = p.start();
    if p.eat(SyntaxKind::DefaultKw) {
        p.eat(SyntaxKind::Colon);
        stmt(p);
    } else {
        range_list(p);
        p.expect(SyntaxKind::Colon);
        stmt(p);
    }
    m.complete(p, SyntaxKind::CaseInsideItem);
}

// `case_pattern_item ::= pattern [&&& expr] : stmt | default [:] stmt` (LRM 12.6.1)
fn case_pattern_item(p: &mut Parser) {
    let m = p.start();
    if p.eat(SyntaxKind::DefaultKw) {
        p.eat(SyntaxKind::Colon);
        stmt(p);
    } else {
        super::patterns::pattern(p);
        if p.at(SyntaxKind::AmpAmpAmp) {
            expressions::cond_guard(p);
        }
        p.expect(SyntaxKind::Colon);
        stmt(p);
    }
    m.complete(p, SyntaxKind::CasePatternItem);
}

// `range_list ::= value_range { , value_range }`
fn range_list(p: &mut Parser) {
    let m = p.start();
    value_range(p);
    while p.eat(SyntaxKind::Comma) {
        value_range(p);
    }
    m.complete(p, SyntaxKind::RangeList);
}

// `value_range ::= expression | [ expression : expression ]`
fn value_range(p: &mut Parser) {
    let m = p.start();
    if p.at(SyntaxKind::LBracket) {
        p.bump(); // [
        expressions::expr(p);
        p.expect(SyntaxKind::Colon);
        expressions::expr(p);
        p.expect(SyntaxKind::RBracket);
    } else {
        expressions::expr(p);
    }
    m.complete(p, SyntaxKind::ValueRange);
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
    let d = p.start();
    p.expect(SyntaxKind::Ident);
    if p.eat(SyntaxKind::Assign) {
        expressions::expr(p);
    }
    d.complete(p, SyntaxKind::Declarator);
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

// `do stmt while (expr) ;`
fn do_while_stmt(p: &mut Parser) {
    let m = p.start();
    p.bump(); // do
    stmt(p);
    p.expect(SyntaxKind::WhileKw);
    p.expect(SyntaxKind::LParen);
    expressions::expr(p);
    p.expect(SyntaxKind::RParen);
    p.expect(SyntaxKind::Semicolon);
    m.complete(p, SyntaxKind::DoWhileStmt);
}

// `break ;` (LRM 12.8)
fn break_stmt(p: &mut Parser) {
    let m = p.start();
    p.bump(); // break
    p.expect(SyntaxKind::Semicolon);
    m.complete(p, SyntaxKind::BreakStmt);
}

// `continue ;` (LRM 12.8)
fn continue_stmt(p: &mut Parser) {
    let m = p.start();
    p.bump(); // continue
    p.expect(SyntaxKind::Semicolon);
    m.complete(p, SyntaxKind::ContinueStmt);
}

// `return [expr] ;` -- expr omitted if next token is `;` (LRM 12.8)
fn return_stmt(p: &mut Parser) {
    let m = p.start();
    p.bump(); // return
    if !p.at(SyntaxKind::Semicolon) {
        expressions::expr(p);
    }
    p.expect(SyntaxKind::Semicolon);
    m.complete(p, SyntaxKind::ReturnStmt);
}

// `foreach (array_ref[vars]) stmt` (LRM 12.7.3)
fn foreach_stmt(p: &mut Parser) {
    let m = p.start();
    p.bump(); // foreach
    p.expect(SyntaxKind::LParen);
    foreach_array_ref(p);
    foreach_var_list(p);
    p.expect(SyntaxKind::RParen);
    stmt(p);
    m.complete(p, SyntaxKind::ForeachStmt);
}

// Parse the array reference: a reference-like expression consisting of a
// base name (NameRef or QualifiedName) followed by optional postfix selects
// (field access `.ident` and index/part-select `[...]`). The last `[...]`
// before `)` is the foreach var list, not an index select.
fn foreach_array_ref(p: &mut Parser) {
    if !p.at(SyntaxKind::Ident) && !p.at(SyntaxKind::EscapedIdent) {
        p.error("expected array name");
        return;
    }
    let mut lhs = if p.current() == SyntaxKind::Ident
        && p.nth(1) == SyntaxKind::ColonColon
        && p.nth(2) == SyntaxKind::Ident
    {
        let m = p.start();
        p.bump(); // first segment
        while p.at(SyntaxKind::ColonColon) && p.nth(1) == SyntaxKind::Ident {
            p.bump(); // ::
            p.bump(); // segment
        }
        m.complete(p, SyntaxKind::QualifiedName)
    } else {
        let m = p.start();
        p.bump(); // ident
        m.complete(p, SyntaxKind::NameRef)
    };
    // Postfix: field access and index selects (not the var-list bracket)
    loop {
        if p.at(SyntaxKind::Dot)
            && (p.nth(1) == SyntaxKind::Ident || p.nth(1) == SyntaxKind::EscapedIdent)
        {
            let m = lhs.precede(p);
            p.bump(); // .
            p.bump(); // ident
            lhs = m.complete(p, SyntaxKind::FieldExpr);
        } else if p.at(SyntaxKind::LBracket) && !is_foreach_var_bracket(p) {
            lhs = expressions::parse_index_or_range(p, lhs);
        } else {
            break;
        }
    }
}

// Check if the current `[` starts the foreach loop-variable list.
// The var list bracket is the last `[...]` before `)`.
fn is_foreach_var_bracket(p: &Parser) -> bool {
    if p.current() != SyntaxKind::LBracket {
        return false;
    }
    let mut n = 1;
    let mut depth: u32 = 1;
    loop {
        let kind = p.nth(n);
        if kind == SyntaxKind::LBracket {
            depth += 1;
        } else if kind == SyntaxKind::RBracket {
            depth -= 1;
            if depth == 0 {
                return p.nth(n + 1) == SyntaxKind::RParen;
            }
        } else if kind == SyntaxKind::Eof {
            return false;
        }
        n += 1;
    }
}

// Parse `[var1, , var3]` as ForeachVarList with ForeachVarSlot children.
fn foreach_var_list(p: &mut Parser) {
    let m = p.start();
    p.expect(SyntaxKind::LBracket);
    // First slot
    foreach_var_slot(p);
    while p.eat(SyntaxKind::Comma) {
        foreach_var_slot(p);
    }
    p.expect(SyntaxKind::RBracket);
    m.complete(p, SyntaxKind::ForeachVarList);
}

fn foreach_var_slot(p: &mut Parser) {
    let m = p.start();
    if p.at(SyntaxKind::Ident) || p.at(SyntaxKind::EscapedIdent) {
        let d = p.start();
        p.bump(); // variable name
        d.complete(p, SyntaxKind::Declarator);
    }
    // Skipped slot: no Declarator child
    m.complete(p, SyntaxKind::ForeachVarSlot);
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
    p.at(SyntaxKind::EndmoduleKw) || p.at(SyntaxKind::EndfunctionKw) || p.at(SyntaxKind::EndtaskKw)
}
