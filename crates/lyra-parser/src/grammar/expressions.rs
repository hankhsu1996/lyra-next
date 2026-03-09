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

// Plain expression (normal mode). Returns None if no expression could
// be parsed.
//
// Handles ternary (`?:`) but does NOT accept `matches`/`&&&` predicate
// syntax. Use `cond_predicate()` for grammar positions that allow
// conditional predicates (e.g. `if()` conditions, `case matches`).
pub(crate) fn expr(p: &mut Parser) -> Option<CompletedMarker> {
    let lhs = expr_bp(p, 0, ExprMode::Normal)?;
    Some(maybe_ternary(p, lhs))
}

// Parse an expression that may be the LHS of an assignment statement.
// `<=` will NOT be consumed as an operator. No ternary (LHS only).
pub(crate) fn expr_for_stmt(p: &mut Parser) -> Option<CompletedMarker> {
    expr_bp(p, 0, ExprMode::StmtLhs)
}

// Parse an expression in bracket mode (indexed part-select context).
fn expr_in_bracket(p: &mut Parser) -> Option<CompletedMarker> {
    let lhs = expr_bp(p, 0, ExprMode::Bracket)?;
    Some(maybe_ternary(p, lhs))
}

// Conditional predicate (LRM 12.6): `expression_or_cond_pattern
// { &&& expression_or_cond_pattern }`.
//
// Used in `if()` conditions and `case matches` items. Also handles a
// trailing ternary `?:` so that `if (a > b ? 1 : 0)` and
// `if (a matches p ? b : c)` both work. NOT reachable from plain
// `expr()` -- `expr()` never consumes `matches` or `&&&`, and
// parenthesized expressions call `expr()`, not `cond_predicate()`.
pub(crate) fn cond_predicate(p: &mut Parser) -> Option<CompletedMarker> {
    let mut lhs = expression_or_cond_pattern(p)?;
    if p.at(SyntaxKind::AmpAmpAmp) {
        let m = lhs.precede(p);
        while p.at(SyntaxKind::AmpAmpAmp) {
            cond_guard(p);
        }
        lhs = m.complete(p, SyntaxKind::CondPredicate);
    }
    // A ternary whose test is a predicate (e.g. `x matches p ? a : b`)
    // is a `conditional_expression` per LRM. The ternary `?` can follow
    // a cond_predicate when it appears inside an expression context
    // (e.g. parenthesized expression, if-condition).
    if p.at(SyntaxKind::Question) {
        Some(finish_ternary(p, lhs))
    } else {
        Some(lhs)
    }
}

// LRM `expression_or_cond_pattern`: either a plain expression or
// `expression matches pattern`. Used as each element of a
// `cond_predicate` and inside `&&&` guards.
fn expression_or_cond_pattern(p: &mut Parser) -> Option<CompletedMarker> {
    let mut lhs = expr(p)?;
    if p.at(SyntaxKind::MatchesKw) {
        let m = lhs.precede(p);
        p.bump(); // matches
        super::patterns::pattern(p);
        lhs = m.complete(p, SyntaxKind::MatchesExpr);
    }
    Some(lhs)
}

// Parse a single `&&& guard_expr` clause (LRM 12.6).
//
// Each guard element is an `expression_or_cond_pattern`, so the guard
// can itself contain `matches`.
pub(crate) fn cond_guard(p: &mut Parser) {
    let g = p.start();
    p.bump(); // &&&
    super::eat_attr_instances(p);
    expression_or_cond_pattern(p);
    g.complete(p, SyntaxKind::CondPredicateGuard);
}

// Ternary `? expr : expr`. Only handles `?` -- does NOT consume
// `matches` or `&&&`.
fn maybe_ternary(p: &mut Parser, lhs: CompletedMarker) -> CompletedMarker {
    if p.at(SyntaxKind::Question) {
        finish_ternary(p, lhs)
    } else {
        lhs
    }
}

fn finish_ternary(p: &mut Parser, test: CompletedMarker) -> CompletedMarker {
    let m = test.precede(p);
    p.bump(); // ?
    super::eat_attr_instances(p);
    expr(p); // middle: full expression (right-assoc via recursion)
    p.expect(SyntaxKind::Colon);
    expr(p); // right: full expression
    m.complete(p, SyntaxKind::CondExpr)
}

// Pratt parser with minimum binding power.
//
// Handles all binary/prefix operators. Ternary (`?`), `matches`, and
// `&&&` are handled at a higher layer.
fn expr_bp(p: &mut Parser, min_bp: u8, mode: ExprMode) -> Option<CompletedMarker> {
    let mut lhs = lhs(p, mode)?;

    loop {
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

/// Parse a primary expression: an atom followed by postfix continuations
/// (field access, indexing, calls).
fn primary(p: &mut Parser) -> Option<CompletedMarker> {
    let cm = atom(p)?;
    Some(postfix(p, cm))
}

/// Parse a primary that excludes tagged expressions from the atom layer.
///
/// Used as the operand parser for tagged expression constructors (LRM 11.9)
/// to avoid upward recursion: `atom()` contains `TaggedKw` which would
/// otherwise call back through `primary()` -> `atom()`.
fn tagged_operand_primary(p: &mut Parser) -> Option<CompletedMarker> {
    let cm = atom_non_tagged(p)?;
    Some(postfix(p, cm))
}

fn lhs(p: &mut Parser, mode: ExprMode) -> Option<CompletedMarker> {
    if let Some(bp) = prefix_bp(p.current()) {
        let m = p.start();
        p.bump(); // prefix operator
        super::eat_attr_instances(p);
        expr_bp(p, bp, mode);
        return Some(m.complete(p, SyntaxKind::PrefixExpr));
    }
    primary(p)
}

// type(...) in expression context: standalone TypeExpr or type(...)'(expr) cast.
fn type_expr_or_cast(p: &mut Parser) -> CompletedMarker {
    let state = p.save_state();
    let m = p.start();
    super::declarations::type_expr(p);
    if p.at(SyntaxKind::Tick) && p.nth(1) == SyntaxKind::LParen {
        // Cast: wrap the TypeExpr in a TypeSpec, then CastExpr.
        let ts = m.complete(p, SyntaxKind::TypeSpec);
        let cm = ts.precede(p);
        p.bump(); // '
        p.bump(); // (
        expr(p);
        p.expect(SyntaxKind::RParen);
        return cm.complete(p, SyntaxKind::CastExpr);
    }
    // Standalone type expression
    m.abandon(p);
    p.restore_state(state);
    super::declarations::type_expr(p)
}

// Parse a literal: IntLiteral (optionally followed by based parts),
// BasedLiteralPrefix (+ optional digits), or simple literal tokens.
fn literal(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    match p.current() {
        SyntaxKind::IntLiteral => {
            p.bump();
            // Sized based literal: IntLiteral followed by BasedLiteralPrefix
            // (+ optional BasedLiteralDigits). Trivia between parts is allowed
            // per LRM 5.7.1.
            if p.at(SyntaxKind::BasedLiteralPrefix) {
                p.bump();
                if p.at(SyntaxKind::BasedLiteralDigits) {
                    p.bump();
                }
            }
        }
        SyntaxKind::BasedLiteralPrefix => {
            p.bump();
            if p.at(SyntaxKind::BasedLiteralDigits) {
                p.bump();
            }
        }
        _ => {
            p.bump();
        }
    }
    m.complete(p, SyntaxKind::Literal)
}

/// Parse an atomic expression excluding `tagged` (used by tagged operand parsing
/// to avoid upward recursion).
fn atom_non_tagged(p: &mut Parser) -> Option<CompletedMarker> {
    if p.at(SyntaxKind::TypeKw) && p.nth(1) == SyntaxKind::LParen {
        return Some(type_expr_or_cast(p));
    }

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
        SyntaxKind::IntLiteral
        | SyntaxKind::BasedLiteralPrefix
        | SyntaxKind::RealLiteral
        | SyntaxKind::UnbasedUnsizedLiteral
        | SyntaxKind::StringLiteral
        | SyntaxKind::TimeLiteral => Some(literal(p)),
        SyntaxKind::Ident | SyntaxKind::SystemIdent => {
            // System function/task call: $foo(...)
            if p.current() == SyntaxKind::SystemIdent && p.nth(1) == SyntaxKind::LParen {
                let m = p.start();
                p.bump(); // SystemIdent token (direct child, no NameRef wrapper)
                system_tf_arg_list(p);
                return Some(m.complete(p, SyntaxKind::SystemTfCall));
            }
            // $unit :: Ident qualified name
            if super::is_unit_scope_prefix(p)
                && p.nth(1) == SyntaxKind::ColonColon
                && p.nth(2) == SyntaxKind::Ident
            {
                return Some(super::parse_qualified_name(p));
            }
            // Check for qualified name: Ident :: Ident [:: Ident]*
            if p.current() == SyntaxKind::Ident
                && p.nth(1) == SyntaxKind::ColonColon
                && p.nth(2) == SyntaxKind::Ident
            {
                return Some(super::parse_qualified_name(p));
            }
            let m = p.start();
            p.bump();
            Some(m.complete(p, SyntaxKind::NameRef))
        }
        SyntaxKind::LParen => {
            let m = p.start();
            p.bump(); // (
            expr(p);
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
            Some(m.complete(p, SyntaxKind::AssignmentPatternExpr))
        }
        SyntaxKind::Dollar => {
            let m = p.start();
            p.bump(); // $
            Some(m.complete(p, SyntaxKind::DollarExpr))
        }
        SyntaxKind::NewKw => {
            let m = p.start();
            p.bump(); // new
            if p.at(SyntaxKind::LBracket) {
                p.bump(); // [
                expr(p);
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

fn atom(p: &mut Parser) -> Option<CompletedMarker> {
    if p.at(SyntaxKind::TaggedKw) {
        let m = p.start();
        p.bump(); // tagged
        p.expect(SyntaxKind::Ident); // member_identifier
        tagged_operand_primary(p); // operand via non-tagged atom + postfix
        return Some(m.complete(p, SyntaxKind::TaggedExpr));
    }
    atom_non_tagged(p)
}

/// Whether a token can appear as a method/member name after `.`.
///
/// LRM 7.12 array manipulation methods use keyword tokens (`and`, `or`,
/// `xor`, `unique`) as method names.
fn is_member_name_token(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::Ident
            | SyntaxKind::EscapedIdent
            | SyntaxKind::AndKw
            | SyntaxKind::OrKw
            | SyntaxKind::XorKw
            | SyntaxKind::UniqueKw
    )
}

// Postfix: call `(...)`, index `[...]`, field `.name`
fn postfix(p: &mut Parser, mut lhs: CompletedMarker) -> CompletedMarker {
    loop {
        match p.current() {
            SyntaxKind::LParen => {
                let m = lhs.precede(p);
                arg_list(p);
                if p.at(SyntaxKind::WithKw) {
                    array_manip_with_clause(p);
                }
                lhs = m.complete(p, SyntaxKind::CallExpr);
            }
            SyntaxKind::LBracket => {
                lhs = parse_index_or_range(p, lhs);
            }
            SyntaxKind::Dot => {
                let m = lhs.precede(p);
                p.bump(); // .
                if is_member_name_token(p.current()) {
                    p.bump();
                } else {
                    p.error("expected member name");
                }
                lhs = m.complete(p, SyntaxKind::FieldExpr);
            }
            _ => break,
        }
    }
    lhs
}

/// Parse `with ( expr )` clause for array manipulation methods (LRM 7.12).
fn array_manip_with_clause(p: &mut Parser) {
    let m = p.start();
    p.bump(); // with
    p.expect(SyntaxKind::LParen);
    expr(p);
    p.expect(SyntaxKind::RParen);
    m.complete(p, SyntaxKind::ArrayManipWithClause);
}

/// Consume `+:` or `-:` as indexed part-select operator tokens.
fn eat_indexed_part_select_op(p: &mut Parser) -> bool {
    if p.at(SyntaxKind::Plus) && p.nth(1) == SyntaxKind::Colon {
        p.bump(); // +
        p.bump(); // :
        true
    } else if p.at(SyntaxKind::Minus) && p.nth(1) == SyntaxKind::Colon {
        p.bump(); // -
        p.bump(); // :
        true
    } else {
        false
    }
}

pub(crate) fn parse_index_or_range(p: &mut Parser, lhs: CompletedMarker) -> CompletedMarker {
    let m = lhs.precede(p);
    p.bump(); // [
    // Use Bracket mode so `+:` and `-:` are not consumed as binary ops
    expr_in_bracket(p);
    if p.eat(SyntaxKind::Colon) {
        // Part select: [hi:lo]
        expr(p);
        p.expect(SyntaxKind::RBracket);
        m.complete(p, SyntaxKind::RangeExpr)
    } else if eat_indexed_part_select_op(p) {
        // Indexed part select: [base+:width] or [base-:width]
        expr(p);
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
        expr(p);
        while p.eat(SyntaxKind::Comma) {
            expr(p);
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
            expr(p);
        }
        while p.eat(SyntaxKind::Comma) {
            expr(p);
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
    expr(p);

    if p.at(SyntaxKind::LBrace) {
        // Replication: `{ count { items } }`
        p.bump(); // inner {
        if !p.at(SyntaxKind::RBrace) {
            expr(p);
            while p.eat(SyntaxKind::Comma) {
                expr(p);
            }
        }
        p.expect(SyntaxKind::RBrace); // inner }
        p.expect(SyntaxKind::RBrace); // outer }
        m.complete(p, SyntaxKind::ReplicExpr)
    } else {
        // Concatenation: `{ a, b, c }`
        while p.eat(SyntaxKind::Comma) {
            expr(p);
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
            expr(p);
        }
        ss.complete(p, SyntaxKind::StreamSliceSize);
    }

    // Inner { operand_list } wrapped in StreamOperands node
    let ops = p.start();
    p.expect(SyntaxKind::LBrace);
    if !p.at(SyntaxKind::RBrace) {
        stream_operand_item(p);
        while p.eat(SyntaxKind::Comma) {
            stream_operand_item(p);
        }
    }
    p.expect(SyntaxKind::RBrace); // inner }
    ops.complete(p, SyntaxKind::StreamOperands);

    p.expect(SyntaxKind::RBrace); // outer }
    m.complete(p, SyntaxKind::StreamExpr)
}

fn stream_operand_item(p: &mut Parser) {
    let m = p.start();
    expr(p);
    if p.at(SyntaxKind::WithKw) {
        stream_with_clause(p);
    }
    m.complete(p, SyntaxKind::StreamOperandItem);
}

fn stream_with_clause(p: &mut Parser) {
    let m = p.start();
    p.bump(); // with
    if !p.at(SyntaxKind::LBracket) {
        p.error("expected `[` after `with`");
        m.complete(p, SyntaxKind::StreamWithClause);
        return;
    }
    p.bump(); // [
    stream_range(p);
    p.expect(SyntaxKind::RBracket);
    m.complete(p, SyntaxKind::StreamWithClause);
}

fn stream_range(p: &mut Parser) {
    let m = p.start();
    expr_in_bracket(p);
    if p.eat(SyntaxKind::Colon) || eat_indexed_part_select_op(p) {
        expr(p);
    }
    m.complete(p, SyntaxKind::StreamRange);
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
        expr(p);
        m.complete(p, SyntaxKind::AssignmentPatternItem);
        return;
    }
    // Try to parse an expression
    let Some(cm) = expr(p) else {
        p.error("expected expression");
        return;
    };
    // Replication: count { items }
    if p.at(SyntaxKind::LBrace) {
        let m = cm.precede(p);
        p.bump(); // inner {
        if !p.at(SyntaxKind::RBrace) {
            expr(p);
            while p.eat(SyntaxKind::Comma) {
                expr(p);
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
        expr(p);
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
