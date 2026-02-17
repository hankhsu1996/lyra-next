use lyra_lexer::SyntaxKind;
use lyra_parser::SyntaxNode;

use crate::literal::{Base, parse_literal_shape};
use crate::types::ConstEvalError;

type EvalResult = Result<i64, ConstEvalError>;

/// Pure constant expression evaluator over the syntax tree.
///
/// `resolve_name` is called for `NameRef` and `QualifiedName` nodes.
/// The DB layer provides a callback that wires name resolution to
/// recursive `eval_const_int` calls.
pub fn eval_const_expr(
    node: &SyntaxNode,
    resolve_name: &dyn Fn(&SyntaxNode) -> EvalResult,
) -> EvalResult {
    match node.kind() {
        SyntaxKind::Literal => eval_literal(node),
        SyntaxKind::BinExpr => eval_bin_expr(node, resolve_name),
        SyntaxKind::PrefixExpr => eval_prefix_expr(node, resolve_name),
        SyntaxKind::Expression | SyntaxKind::ParenExpr => {
            let child = node.first_child().ok_or(ConstEvalError::Unsupported)?;
            eval_const_expr(&child, resolve_name)
        }
        SyntaxKind::NameRef | SyntaxKind::QualifiedName => resolve_name(node),
        SyntaxKind::CallExpr => eval_call_expr(node, resolve_name),
        _ => Err(ConstEvalError::Unsupported),
    }
}

fn eval_literal(node: &SyntaxNode) -> EvalResult {
    // Use parse_literal_shape for structural checks (base, xz, width)
    let shape = parse_literal_shape(node).ok_or(ConstEvalError::Unsupported)?;

    if shape.has_xz {
        return Err(ConstEvalError::NonConstant);
    }

    // Extract digit tokens for value computation
    let tokens: Vec<_> = node
        .children_with_tokens()
        .filter_map(|el| el.into_token())
        .filter(|tok| {
            matches!(
                tok.kind(),
                SyntaxKind::IntLiteral | SyntaxKind::BasedLiteral
            )
        })
        .collect();

    // Pure unsized decimal (plain IntLiteral like `42`): parse directly as i64.
    // Must check for IntLiteral token to exclude unsized based-decimal (`'d42`)
    // which also has base==Decimal && is_unsized==true.
    if shape.base == Base::Decimal
        && shape.is_unsized
        && tokens
            .first()
            .is_some_and(|t| t.kind() == SyntaxKind::IntLiteral)
        && tokens.len() == 1
    {
        let text = tokens[0].text().replace('_', "");
        return text.parse::<i64>().map_err(|_| ConstEvalError::Overflow);
    }

    // Based literal with signed prefix: not yet supported for const-eval
    if shape.signed {
        return Err(ConstEvalError::Unsupported);
    }

    // Based literal: extract digits from the BasedLiteral token
    let based_tok = tokens
        .iter()
        .find(|t| t.kind() == SyntaxKind::BasedLiteral)
        .ok_or(ConstEvalError::Unsupported)?;

    let digits_str = extract_based_digits(based_tok.text())?;
    let clean_digits: String = digits_str.chars().filter(|&c| c != '_').collect();

    if clean_digits.is_empty() {
        return Err(ConstEvalError::Unsupported);
    }

    let radix = base_radix(shape.base);
    let raw_value =
        u128::from_str_radix(&clean_digits, radix).map_err(|_| ConstEvalError::Overflow)?;

    if shape.is_unsized {
        i64::try_from(raw_value).map_err(|_| ConstEvalError::Overflow)
    } else {
        truncate_to_size(raw_value, shape.width)
    }
}

/// Extract the digit portion from a `BasedLiteral` token text (after tick + optional signed + base char).
fn extract_based_digits(text: &str) -> Result<&str, ConstEvalError> {
    let after_tick = text.strip_prefix('\'').ok_or(ConstEvalError::Unsupported)?;
    if after_tick.is_empty() {
        return Err(ConstEvalError::Unsupported);
    }
    // Skip optional signed prefix
    let rest = if after_tick
        .as_bytes()
        .first()
        .is_some_and(|&b| b == b's' || b == b'S')
    {
        &after_tick[1..]
    } else {
        after_tick
    };
    // Skip base character
    if rest.is_empty() {
        return Err(ConstEvalError::Unsupported);
    }
    Ok(&rest[1..])
}

fn base_radix(base: Base) -> u32 {
    match base {
        Base::Hex => 16,
        Base::Decimal => 10,
        Base::Octal => 8,
        Base::Binary => 2,
    }
}

fn truncate_to_size(value: u128, size: u32) -> EvalResult {
    if size == 0 {
        return Ok(0);
    }
    if size > 64 {
        return Err(ConstEvalError::Overflow);
    }
    let mask = if size == 128 {
        u128::MAX
    } else {
        (1u128 << size) - 1
    };
    let truncated = value & mask;
    // For sizes <= 64, the truncated value fits in u64 which fits in i64 unsigned range
    Ok(truncated as i64)
}

fn eval_bin_expr(
    node: &SyntaxNode,
    resolve_name: &dyn Fn(&SyntaxNode) -> EvalResult,
) -> EvalResult {
    let children: Vec<SyntaxNode> = node.children().collect();
    if children.len() < 2 {
        return Err(ConstEvalError::Unsupported);
    }

    let lhs = eval_const_expr(&children[0], resolve_name)?;

    // Find operator token between child nodes
    let op = node
        .children_with_tokens()
        .filter_map(|el| el.into_token())
        .find(|tok| tok.kind() != SyntaxKind::Whitespace)
        .ok_or(ConstEvalError::Unsupported)?;

    let rhs = eval_const_expr(&children[1], resolve_name)?;

    match op.kind() {
        SyntaxKind::Plus => lhs.checked_add(rhs).ok_or(ConstEvalError::Overflow),
        SyntaxKind::Minus => lhs.checked_sub(rhs).ok_or(ConstEvalError::Overflow),
        SyntaxKind::Star => lhs.checked_mul(rhs).ok_or(ConstEvalError::Overflow),
        SyntaxKind::Slash => {
            if rhs == 0 {
                Err(ConstEvalError::DivideByZero)
            } else {
                lhs.checked_div(rhs).ok_or(ConstEvalError::Overflow)
            }
        }
        SyntaxKind::Percent => {
            if rhs == 0 {
                Err(ConstEvalError::DivideByZero)
            } else {
                lhs.checked_rem(rhs).ok_or(ConstEvalError::Overflow)
            }
        }
        SyntaxKind::LtLt => {
            let shift = u32::try_from(rhs).map_err(|_| ConstEvalError::Overflow)?;
            lhs.checked_shl(shift).ok_or(ConstEvalError::Overflow)
        }
        SyntaxKind::GtGt => {
            let shift = u32::try_from(rhs).map_err(|_| ConstEvalError::Overflow)?;
            lhs.checked_shr(shift).ok_or(ConstEvalError::Overflow)
        }
        SyntaxKind::Amp => Ok(lhs & rhs),
        SyntaxKind::Pipe => Ok(lhs | rhs),
        SyntaxKind::Caret => Ok(lhs ^ rhs),
        _ => Err(ConstEvalError::Unsupported),
    }
}

fn eval_prefix_expr(
    node: &SyntaxNode,
    resolve_name: &dyn Fn(&SyntaxNode) -> EvalResult,
) -> EvalResult {
    let child = node.first_child().ok_or(ConstEvalError::Unsupported)?;
    let operand = eval_const_expr(&child, resolve_name)?;

    let op = node
        .children_with_tokens()
        .filter_map(|el| el.into_token())
        .find(|tok| tok.kind() != SyntaxKind::Whitespace)
        .ok_or(ConstEvalError::Unsupported)?;

    match op.kind() {
        SyntaxKind::Plus => Ok(operand),
        SyntaxKind::Minus => operand.checked_neg().ok_or(ConstEvalError::Overflow),
        SyntaxKind::Tilde => Ok(!operand),
        _ => Err(ConstEvalError::Unsupported),
    }
}

fn eval_call_expr(
    node: &SyntaxNode,
    resolve_name: &dyn Fn(&SyntaxNode) -> EvalResult,
) -> EvalResult {
    // CallExpr has a NameRef child (containing SystemIdent) and an ArgList child.
    // The NameRef wraps the function name token.
    let name_ref = node
        .children()
        .find(|c| c.kind() == SyntaxKind::NameRef)
        .ok_or(ConstEvalError::Unsupported)?;

    let func_token = name_ref
        .children_with_tokens()
        .filter_map(|el| el.into_token())
        .find(|tok| tok.kind() == SyntaxKind::SystemIdent)
        .ok_or(ConstEvalError::Unsupported)?;

    if func_token.text() != "$clog2" {
        return Err(ConstEvalError::Unsupported);
    }

    let arg_list = node
        .children()
        .find(|c| c.kind() == SyntaxKind::ArgList)
        .ok_or(ConstEvalError::Unsupported)?;

    let first_arg = arg_list
        .children()
        .next()
        .ok_or(ConstEvalError::Unsupported)?;

    let value = eval_const_expr(&first_arg, resolve_name)?;
    clog2(value)
}

fn clog2(n: i64) -> EvalResult {
    if n < 0 {
        return Err(ConstEvalError::InvalidArgument);
    }
    if n <= 1 {
        return Ok(0);
    }
    let v = (n - 1).cast_unsigned();
    Ok(64 - i64::from(v.leading_zeros()))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_expr(src: &str) -> SyntaxNode {
        // Wrap in a module to get a valid parse, then extract the expression
        let full = format!("module m; parameter P = {src}; endmodule");
        let tokens = lyra_lexer::lex(&full);
        let pp = lyra_preprocess::preprocess_identity(lyra_source::FileId(0), &tokens, &full);
        let parse = lyra_parser::parse(&pp.tokens, &pp.expanded_text);
        // Navigate: SourceFile > ModuleDecl > ModuleBody > ParamDecl > Declarator
        // The expr() call in the parser produces the expression node directly
        // (e.g. Literal, BinExpr) as a child of Declarator, not wrapped in Expression.
        let root = parse.syntax();
        find_declarator_init(&root).expect("should find init expression in parsed source")
    }

    fn find_declarator_init(node: &SyntaxNode) -> Option<SyntaxNode> {
        if node.kind() == SyntaxKind::Declarator {
            // The initializer expression is the first child node after Ident and '='
            return node.children().find(|c| is_expr_kind(c.kind()));
        }
        for child in node.children() {
            if let Some(found) = find_declarator_init(&child) {
                return Some(found);
            }
        }
        None
    }

    fn is_expr_kind(kind: SyntaxKind) -> bool {
        matches!(
            kind,
            SyntaxKind::Literal
                | SyntaxKind::BinExpr
                | SyntaxKind::PrefixExpr
                | SyntaxKind::ParenExpr
                | SyntaxKind::CondExpr
                | SyntaxKind::ConcatExpr
                | SyntaxKind::ReplicExpr
                | SyntaxKind::IndexExpr
                | SyntaxKind::RangeExpr
                | SyntaxKind::FieldExpr
                | SyntaxKind::CallExpr
                | SyntaxKind::NameRef
                | SyntaxKind::QualifiedName
                | SyntaxKind::Expression
        )
    }

    fn eval(src: &str) -> EvalResult {
        let node = parse_expr(src);
        eval_const_expr(&node, &|_| Err(ConstEvalError::Unresolved))
    }

    fn eval_with_names(src: &str, resolve: &dyn Fn(&SyntaxNode) -> EvalResult) -> EvalResult {
        let node = parse_expr(src);
        eval_const_expr(&node, resolve)
    }

    #[test]
    fn decimal_literal() {
        assert_eq!(eval("42"), Ok(42));
    }

    #[test]
    fn decimal_zero() {
        assert_eq!(eval("0"), Ok(0));
    }

    #[test]
    fn based_literal_hex() {
        assert_eq!(eval("8'hFF"), Ok(255));
    }

    #[test]
    fn based_literal_binary() {
        assert_eq!(eval("4'b1010"), Ok(10));
    }

    #[test]
    fn based_literal_decimal() {
        assert_eq!(eval("32'd100"), Ok(100));
    }

    #[test]
    fn based_literal_truncation() {
        assert_eq!(eval("4'hFF"), Ok(15));
    }

    #[test]
    fn based_literal_size_zero() {
        assert_eq!(eval("0'h1"), Ok(0));
    }

    #[test]
    fn based_literal_size_64() {
        assert_eq!(eval("64'hFFFFFFFFFFFFFFFF"), Ok(-1));
    }

    #[test]
    fn based_literal_size_65_overflow() {
        assert_eq!(eval("65'h1"), Err(ConstEvalError::Overflow));
    }

    #[test]
    fn based_literal_xz() {
        assert_eq!(eval("8'hxF"), Err(ConstEvalError::NonConstant));
    }

    #[test]
    fn based_literal_signed() {
        assert_eq!(eval("8'shFF"), Err(ConstEvalError::Unsupported));
    }

    #[test]
    fn based_literal_no_size() {
        assert_eq!(eval("'hFF"), Ok(255));
    }

    #[test]
    fn unsized_based_decimal() {
        assert_eq!(eval("'d42"), Ok(42));
    }

    #[test]
    fn bin_add() {
        assert_eq!(eval("3 + 4"), Ok(7));
    }

    #[test]
    fn bin_sub() {
        assert_eq!(eval("10 - 3"), Ok(7));
    }

    #[test]
    fn bin_mul() {
        assert_eq!(eval("6 * 7"), Ok(42));
    }

    #[test]
    fn bin_div() {
        assert_eq!(eval("42 / 6"), Ok(7));
    }

    #[test]
    fn bin_mod() {
        assert_eq!(eval("10 % 3"), Ok(1));
    }

    #[test]
    fn bin_shl() {
        assert_eq!(eval("1 << 8"), Ok(256));
    }

    #[test]
    fn bin_shr() {
        assert_eq!(eval("256 >> 4"), Ok(16));
    }

    #[test]
    fn bin_and() {
        assert_eq!(eval("255 & 15"), Ok(15));
    }

    #[test]
    fn bin_or() {
        assert_eq!(eval("240 | 15"), Ok(255));
    }

    #[test]
    fn bin_xor() {
        assert_eq!(eval("255 ^ 15"), Ok(240));
    }

    #[test]
    fn divide_by_zero() {
        assert_eq!(eval("1 / 0"), Err(ConstEvalError::DivideByZero));
    }

    #[test]
    fn prefix_plus() {
        assert_eq!(eval("+42"), Ok(42));
    }

    #[test]
    fn prefix_minus() {
        assert_eq!(eval("-42"), Ok(-42));
    }

    #[test]
    fn prefix_tilde() {
        assert_eq!(eval("~0"), Ok(-1));
    }

    #[test]
    fn paren_expr() {
        assert_eq!(eval("(3 + 4) * 2"), Ok(14));
    }

    #[test]
    fn precedence() {
        assert_eq!(eval("3 + 4 * 2"), Ok(11));
    }

    #[test]
    fn clog2_zero() {
        assert_eq!(eval("$clog2(0)"), Ok(0));
    }

    #[test]
    fn clog2_one() {
        assert_eq!(eval("$clog2(1)"), Ok(0));
    }

    #[test]
    fn clog2_two() {
        assert_eq!(eval("$clog2(2)"), Ok(1));
    }

    #[test]
    fn clog2_three() {
        assert_eq!(eval("$clog2(3)"), Ok(2));
    }

    #[test]
    fn clog2_four() {
        assert_eq!(eval("$clog2(4)"), Ok(2));
    }

    #[test]
    fn clog2_five() {
        assert_eq!(eval("$clog2(5)"), Ok(3));
    }

    #[test]
    fn clog2_256() {
        assert_eq!(eval("$clog2(256)"), Ok(8));
    }

    #[test]
    fn clog2_negative() {
        // $clog2(-1) should be InvalidArgument
        // Parse "-1" as PrefixExpr with Minus and 1
        assert_eq!(eval("$clog2(-1)"), Err(ConstEvalError::InvalidArgument));
    }

    #[test]
    fn unsupported_concat() {
        // Concat is not supported for const eval
        let full = "module m; parameter P = {8'h1, 8'h2}; endmodule";
        let tokens = lyra_lexer::lex(full);
        let pp = lyra_preprocess::preprocess_identity(lyra_source::FileId(0), &tokens, full);
        let parse = lyra_parser::parse(&pp.tokens, &pp.expanded_text);
        let root = parse.syntax();
        if let Some(expr) = find_declarator_init(&root) {
            let result = eval_const_expr(&expr, &|_| Err(ConstEvalError::Unresolved));
            assert_eq!(result, Err(ConstEvalError::Unsupported));
        }
    }

    #[test]
    fn overflow_add() {
        let src = format!("{} + 1", i64::MAX);
        assert_eq!(eval(&src), Err(ConstEvalError::Overflow));
    }

    #[test]
    fn name_ref_resolve_ok() {
        let result = eval_with_names("x", &|_| Ok(8));
        assert_eq!(result, Ok(8));
    }

    #[test]
    fn name_ref_resolve_err() {
        let result = eval_with_names("x", &|_| Err(ConstEvalError::Unresolved));
        assert_eq!(result, Err(ConstEvalError::Unresolved));
    }

    #[test]
    fn underscore_in_literal() {
        assert_eq!(eval("1_000"), Ok(1000));
    }

    #[test]
    fn underscore_in_based_literal() {
        assert_eq!(eval("8'hF_F"), Ok(255));
    }
}
