use lyra_lexer::SyntaxKind;

use crate::parser::{CompletedMarker, Parser};

/// Controls whether `<=` is treated as relational (in pure expressions)
/// or stops the expression so the caller can handle it as assignment.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum ExprMode {
    /// Normal expression: `<=` is relational operator.
    Normal,
    /// Statement context: `<=` stops expression (caller handles as NBA).
    StmtLhs,
    /// Inside `[...]`: `+:` and `-:` stop the expression for indexed
    /// part-select handling (LRM 11.5.1).
    Bracket,
}

// Parse an expression (normal mode). Returns None if no expression could be parsed.
pub(crate) fn expr(p: &mut Parser) -> Option<CompletedMarker> {
    expr_bp(p, 0, ExprMode::Normal)
}

// Parse an expression that may be the LHS of an assignment statement.
// `<=` will NOT be consumed as an operator.
pub(crate) fn expr_for_stmt(p: &mut Parser) -> Option<CompletedMarker> {
    expr_bp(p, 0, ExprMode::StmtLhs)
}

// Pratt parser with minimum binding power.
fn expr_bp(p: &mut Parser, min_bp: u8, mode: ExprMode) -> Option<CompletedMarker> {
    let mut lhs = lhs(p, mode)?;

    loop {
        // Conditional ternary: `? expr : expr`
        if p.at(SyntaxKind::Question) {
            let (l_bp, _) = (2, 1);
            if l_bp < min_bp {
                break;
            }
            let m = lhs.precede(p);
            p.bump(); // ?
            super::eat_attr_instances(p);
            expr_bp(p, 0, mode); // middle expression, no min_bp restriction
            p.expect(SyntaxKind::Colon);
            expr_bp(p, 1, mode); // right-associative
            lhs = m.complete(p, SyntaxKind::CondExpr);
            continue;
        }

        // In StmtLhs mode, `<=` stops the expression for NBA handling
        if mode == ExprMode::StmtLhs && p.at(SyntaxKind::LtEq) {
            break;
        }

        // In Bracket mode, `+:` and `-:` stop for indexed part-select
        if mode == ExprMode::Bracket
            && matches!(p.current(), SyntaxKind::Plus | SyntaxKind::Minus)
            && p.nth(1) == SyntaxKind::Colon
        {
            break;
        }

        let Some((op_bp, kind)) = infix_bp(p.current()) else {
            break;
        };
        if op_bp.0 < min_bp {
            break;
        }
        let m = lhs.precede(p);
        p.bump(); // operator
        super::eat_attr_instances(p);
        expr_bp(p, op_bp.1, mode);
        lhs = m.complete(p, kind);
    }

    Some(lhs)
}

fn lhs(p: &mut Parser, mode: ExprMode) -> Option<CompletedMarker> {
    if let Some(bp) = prefix_bp(p.current()) {
        let m = p.start();
        p.bump(); // prefix operator
        super::eat_attr_instances(p);
        expr_bp(p, bp, mode);
        return Some(m.complete(p, SyntaxKind::PrefixExpr));
    }
    let cm = atom(p)?;
    Some(postfix(p, cm))
}

fn atom(p: &mut Parser) -> Option<CompletedMarker> {
    // Cast expression: data_type ' ( expr )
    // Speculatively parse a type; commit only if followed by Tick + LParen.
    if super::declarations::at_cast_type(p.current()) {
        let state = p.save_state();
        let m = p.start();
        super::declarations::type_spec(p);
        if p.at(SyntaxKind::Tick) && p.nth(1) == SyntaxKind::LParen {
            p.bump(); // '
            p.bump(); // (
            expr(p);
            p.expect(SyntaxKind::RParen);
            return Some(m.complete(p, SyntaxKind::CastExpr));
        }
        m.abandon(p);
        p.restore_state(state);
    }

    match p.current() {
        SyntaxKind::IntLiteral => {
            let m = p.start();
            p.bump();
            // Sized based literal: IntLiteral followed by BasedLiteral.
            // Trivia (whitespace, comments) between them is allowed -- the LRM
            // grammar operates on post-preprocessor token streams where
            // macro/include expansion can insert trivia between tokens.
            if p.current() == SyntaxKind::BasedLiteral {
                p.bump();
            }
            Some(m.complete(p, SyntaxKind::Literal))
        }
        SyntaxKind::RealLiteral
        | SyntaxKind::BasedLiteral
        | SyntaxKind::UnbasedUnsizedLiteral
        | SyntaxKind::StringLiteral
        | SyntaxKind::TimeLiteral => {
            let m = p.start();
            p.bump();
            Some(m.complete(p, SyntaxKind::Literal))
        }
        SyntaxKind::Ident | SyntaxKind::SystemIdent => {
            // System function/task call: $foo(...)
            if p.current() == SyntaxKind::SystemIdent && p.nth(1) == SyntaxKind::LParen {
                let m = p.start();
                p.bump(); // SystemIdent token (direct child, no NameRef wrapper)
                system_tf_arg_list(p);
                return Some(m.complete(p, SyntaxKind::SystemTfCall));
            }
            // Check for qualified name: Ident :: Ident [:: Ident]*
            if p.current() == SyntaxKind::Ident
                && p.nth(1) == SyntaxKind::ColonColon
                && p.nth(2) == SyntaxKind::Ident
            {
                let m = p.start();
                p.bump(); // first segment
                while p.at(SyntaxKind::ColonColon) && p.nth(1) == SyntaxKind::Ident {
                    p.bump(); // ::
                    p.bump(); // segment
                }
                return Some(m.complete(p, SyntaxKind::QualifiedName));
            }
            let m = p.start();
            p.bump();
            Some(m.complete(p, SyntaxKind::NameRef))
        }
        SyntaxKind::LParen => {
            let m = p.start();
            p.bump(); // (
            expr_bp(p, 0, ExprMode::Normal);
            p.expect(SyntaxKind::RParen);
            Some(m.complete(p, SyntaxKind::ParenExpr))
        }
        SyntaxKind::LBrace => Some(concat_or_replic(p)),
        SyntaxKind::TickBrace => {
            // Assignment pattern `'{expr, ...}`, `'{key:val, ...}`, `'{default:val}`
            let m = p.start();
            p.bump(); // '{
            if !p.at(SyntaxKind::RBrace) {
                assignment_pattern_item(p);
                while p.eat(SyntaxKind::Comma) {
                    assignment_pattern_item(p);
                }
            }
            p.expect(SyntaxKind::RBrace);
            Some(m.complete(p, SyntaxKind::ConcatExpr))
        }
        SyntaxKind::NewKw => {
            let m = p.start();
            p.bump(); // new
            if p.at(SyntaxKind::LBracket) {
                p.bump(); // [
                expr_bp(p, 0, ExprMode::Normal);
                p.expect(SyntaxKind::RBracket);
                if p.at(SyntaxKind::LParen) {
                    arg_list(p);
                }
            } else if p.at(SyntaxKind::LParen) {
                arg_list(p);
            }
            Some(m.complete(p, SyntaxKind::NewExpr))
        }
        _ => None,
    }
}

// Postfix: call `(...)`, index `[...]`, field `.name`
fn postfix(p: &mut Parser, mut lhs: CompletedMarker) -> CompletedMarker {
    loop {
        match p.current() {
            SyntaxKind::LParen => {
                let m = lhs.precede(p);
                arg_list(p);
                lhs = m.complete(p, SyntaxKind::CallExpr);
            }
            SyntaxKind::LBracket => {
                lhs = parse_index_or_range(p, lhs);
            }
            SyntaxKind::Dot => {
                let m = lhs.precede(p);
                p.bump(); // .
                p.expect(SyntaxKind::Ident);
                lhs = m.complete(p, SyntaxKind::FieldExpr);
            }
            _ => break,
        }
    }
    lhs
}

fn parse_index_or_range(p: &mut Parser, lhs: CompletedMarker) -> CompletedMarker {
    let m = lhs.precede(p);
    p.bump(); // [
    // Use Bracket mode so `+:` and `-:` are not consumed as binary ops
    expr_bp(p, 0, ExprMode::Bracket);
    if p.eat(SyntaxKind::Colon) {
        // Part select: [hi:lo]
        expr_bp(p, 0, ExprMode::Normal);
        p.expect(SyntaxKind::RBracket);
        m.complete(p, SyntaxKind::RangeExpr)
    } else if (p.at(SyntaxKind::Plus) || p.at(SyntaxKind::Minus)) && p.nth(1) == SyntaxKind::Colon {
        // Indexed part select: [base+:width] or [base-:width]
        p.bump(); // + or -
        p.bump(); // :
        expr_bp(p, 0, ExprMode::Normal);
        p.expect(SyntaxKind::RBracket);
        m.complete(p, SyntaxKind::RangeExpr)
    } else {
        p.expect(SyntaxKind::RBracket);
        m.complete(p, SyntaxKind::IndexExpr)
    }
}

fn arg_list(p: &mut Parser) {
    let m = p.start();
    p.bump(); // (
    if !p.at(SyntaxKind::RParen) {
        expr_bp(p, 0, ExprMode::Normal);
        while p.eat(SyntaxKind::Comma) {
            expr_bp(p, 0, ExprMode::Normal);
        }
    }
    p.expect(SyntaxKind::RParen);
    m.complete(p, SyntaxKind::ArgList);
}

fn system_tf_arg_list(p: &mut Parser) {
    let m = p.start();
    p.bump(); // (
    if !p.at(SyntaxKind::RParen) {
        if super::declarations::is_scalar_type_keyword(p.current()) {
            super::declarations::type_spec(p);
        } else {
            expr_bp(p, 0, ExprMode::Normal);
        }
        while p.eat(SyntaxKind::Comma) {
            expr_bp(p, 0, ExprMode::Normal);
        }
    }
    p.expect(SyntaxKind::RParen);
    m.complete(p, SyntaxKind::SystemTfArgList);
}

// `{ expr, expr }`, `{ count { expr } }` (replication), or `{>> ...}` / `{<< ...}` (streaming)
fn concat_or_replic(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    p.bump(); // {

    // Streaming: {>> ...} or {<< ...}
    if p.at(SyntaxKind::GtGt) || p.at(SyntaxKind::LtLt) {
        return stream_expr(p, m);
    }

    if p.at(SyntaxKind::RBrace) {
        p.bump();
        return m.complete(p, SyntaxKind::ConcatExpr);
    }

    // Parse first expression
    expr_bp(p, 0, ExprMode::Normal);

    if p.at(SyntaxKind::LBrace) {
        // Replication: `{ count { items } }`
        p.bump(); // inner {
        if !p.at(SyntaxKind::RBrace) {
            expr_bp(p, 0, ExprMode::Normal);
            while p.eat(SyntaxKind::Comma) {
                expr_bp(p, 0, ExprMode::Normal);
            }
        }
        p.expect(SyntaxKind::RBrace); // inner }
        p.expect(SyntaxKind::RBrace); // outer }
        m.complete(p, SyntaxKind::ReplicExpr)
    } else {
        // Concatenation: `{ a, b, c }`
        while p.eat(SyntaxKind::Comma) {
            expr_bp(p, 0, ExprMode::Normal);
        }
        p.expect(SyntaxKind::RBrace);
        m.complete(p, SyntaxKind::ConcatExpr)
    }
}

// Streaming operator: outer { already consumed by caller.
//
// Disambiguation: `{` immediately after `<<`/`>>` always starts the operands
// list (no slice_size). A `{`-starting constant expression as slice_size is
// syntactically ambiguous and not supported; use a parameter name instead.
fn stream_expr(p: &mut Parser, m: crate::parser::Marker) -> CompletedMarker {
    p.bump(); // >> or <<

    // Optional slice_size: wrapped in StreamSliceSize node.
    // Data-type keywords and identifiers parse as a full TypeSpec (handles
    // packed dims, signing, qualified names). Everything else parses as an
    // expression.
    if !p.at(SyntaxKind::LBrace) {
        let ss = p.start();
        if super::declarations::is_data_type_keyword(p.current()) || p.at(SyntaxKind::Ident) {
            super::declarations::type_spec(p);
        } else {
            expr_bp(p, 0, ExprMode::Normal);
        }
        ss.complete(p, SyntaxKind::StreamSliceSize);
    }

    // Inner { expr_list } wrapped in StreamOperands node
    let ops = p.start();
    p.expect(SyntaxKind::LBrace);
    if !p.at(SyntaxKind::RBrace) {
        expr_bp(p, 0, ExprMode::Normal);
        while p.eat(SyntaxKind::Comma) {
            expr_bp(p, 0, ExprMode::Normal);
        }
    }
    p.expect(SyntaxKind::RBrace); // inner }
    ops.complete(p, SyntaxKind::StreamOperands);

    p.expect(SyntaxKind::RBrace); // outer }
    m.complete(p, SyntaxKind::StreamExpr)
}

// Parse a single item inside an assignment pattern `'{...}`.
// Handles positional (`expr`), keyed (`expr : expr`), and default (`default : expr`).
// All non-replication items produce an `AssignmentPatternItem` wrapper for uniform
// downstream consumption.
fn assignment_pattern_item(p: &mut Parser) {
    // default : expr
    if p.at(SyntaxKind::DefaultKw) && p.nth(1) == SyntaxKind::Colon {
        let m = p.start();
        p.bump(); // default
        p.bump(); // :
        expr_bp(p, 0, ExprMode::Normal);
        m.complete(p, SyntaxKind::AssignmentPatternItem);
        return;
    }
    // Try to parse an expression
    let Some(cm) = expr_bp(p, 0, ExprMode::Normal) else {
        p.error("expected expression");
        return;
    };
    // Replication: count { items }
    if p.at(SyntaxKind::LBrace) {
        let m = cm.precede(p);
        p.bump(); // inner {
        if !p.at(SyntaxKind::RBrace) {
            expr_bp(p, 0, ExprMode::Normal);
            while p.eat(SyntaxKind::Comma) {
                expr_bp(p, 0, ExprMode::Normal);
            }
        }
        p.expect(SyntaxKind::RBrace);
        m.complete(p, SyntaxKind::ReplicExpr);
        return;
    }
    // Keyed or positional -- wrap uniformly in AssignmentPatternItem
    let m = cm.precede(p);
    if p.at(SyntaxKind::Colon) {
        p.bump(); // :
        expr_bp(p, 0, ExprMode::Normal);
    }
    m.complete(p, SyntaxKind::AssignmentPatternItem);
}

// Binding power for binary/infix operators.
// Returns ((left_bp, right_bp), node_kind).
fn infix_bp(kind: SyntaxKind) -> Option<((u8, u8), SyntaxKind)> {
    let bp = match kind {
        SyntaxKind::PipePipe => ((3, 4), SyntaxKind::BinExpr),
        SyntaxKind::AmpAmp => ((5, 6), SyntaxKind::BinExpr),
        SyntaxKind::Pipe => ((7, 8), SyntaxKind::BinExpr),
        SyntaxKind::Caret | SyntaxKind::TildeCaret | SyntaxKind::CaretTilde => {
            ((9, 10), SyntaxKind::BinExpr)
        }
        SyntaxKind::Amp => ((11, 12), SyntaxKind::BinExpr),
        SyntaxKind::EqEq
        | SyntaxKind::BangEq
        | SyntaxKind::EqEqEq
        | SyntaxKind::BangEqEq
        | SyntaxKind::EqEqQuestion
        | SyntaxKind::BangEqQuestion => ((13, 14), SyntaxKind::BinExpr),
        SyntaxKind::Lt | SyntaxKind::LtEq | SyntaxKind::Gt | SyntaxKind::GtEq => {
            ((15, 16), SyntaxKind::BinExpr)
        }
        SyntaxKind::LtLt | SyntaxKind::GtGt | SyntaxKind::LtLtLt | SyntaxKind::GtGtGt => {
            ((17, 18), SyntaxKind::BinExpr)
        }
        SyntaxKind::Plus | SyntaxKind::Minus => ((19, 20), SyntaxKind::BinExpr),
        SyntaxKind::Star | SyntaxKind::Slash | SyntaxKind::Percent => {
            ((21, 22), SyntaxKind::BinExpr)
        }
        SyntaxKind::StarStar => ((24, 23), SyntaxKind::BinExpr), // right-assoc
        _ => return None,
    };
    Some(bp)
}

// Prefix binding power (right side).
fn prefix_bp(kind: SyntaxKind) -> Option<u8> {
    match kind {
        SyntaxKind::Plus
        | SyntaxKind::Minus
        | SyntaxKind::Bang
        | SyntaxKind::Tilde
        | SyntaxKind::Amp
        | SyntaxKind::TildeAmp
        | SyntaxKind::Pipe
        | SyntaxKind::TildePipe
        | SyntaxKind::Caret
        | SyntaxKind::TildeCaret
        | SyntaxKind::CaretTilde => Some(25),
        _ => None,
    }
}
