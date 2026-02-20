use lyra_lexer::SyntaxKind;
use lyra_parser::SyntaxNode;

/// Check whether a `SyntaxKind` represents an expression node.
///
/// Used by type inference, type checking, type extraction, and the def-index
/// builder to filter expression children from syntax trees.
pub fn is_expression_kind(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::Expression
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
            | SyntaxKind::SystemTfCall
            | SyntaxKind::ArgList
            | SyntaxKind::NameRef
            | SyntaxKind::Literal
            | SyntaxKind::QualifiedName
    )
}

/// Find the first non-whitespace operator token in a prefix expression.
pub fn find_operator_token(node: &SyntaxNode) -> Option<SyntaxKind> {
    node.children_with_tokens()
        .filter_map(|el| el.into_token())
        .find(|tok| tok.kind() != SyntaxKind::Whitespace)
        .map(|tok| tok.kind())
}

/// Binary operator classification for expression typing.
#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    BitAnd,
    BitOr,
    BitXor,
    BitXnor,
    Shl,
    Shr,
    Ashl,
    Ashr,
    Lt,
    LtEq,
    Gt,
    GtEq,
    Eq,
    Neq,
    CaseEq,
    CaseNeq,
    WildEq,
    WildNeq,
    LogAnd,
    LogOr,
    Power,
}

/// Extract the binary operator from a `BinExpr` node.
///
/// Collects operator tokens between the first and second expression children,
/// then maps the token sequence to a `BinaryOp`.
pub fn find_binary_op(node: &SyntaxNode) -> Option<BinaryOp> {
    let mut ops: Vec<SyntaxKind> = Vec::new();
    let mut seen_first_expr = false;
    for el in node.children_with_tokens() {
        if el.as_node().is_some_and(|n| is_expression_kind(n.kind())) {
            if seen_first_expr {
                break;
            }
            seen_first_expr = true;
            continue;
        }
        if let Some(tok) = el.as_token()
            && tok.kind() != SyntaxKind::Whitespace
            && seen_first_expr
        {
            ops.push(tok.kind());
        }
    }

    match ops.as_slice() {
        [SyntaxKind::Plus] => Some(BinaryOp::Add),
        [SyntaxKind::Minus] => Some(BinaryOp::Sub),
        [SyntaxKind::Star] => Some(BinaryOp::Mul),
        [SyntaxKind::Slash] => Some(BinaryOp::Div),
        [SyntaxKind::Percent] => Some(BinaryOp::Mod),
        [SyntaxKind::Amp] => Some(BinaryOp::BitAnd),
        [SyntaxKind::Pipe] => Some(BinaryOp::BitOr),
        [SyntaxKind::Caret] => Some(BinaryOp::BitXor),
        [SyntaxKind::TildeCaret | SyntaxKind::CaretTilde] => Some(BinaryOp::BitXnor),
        [SyntaxKind::LtLt] => Some(BinaryOp::Shl),
        [SyntaxKind::GtGt] => Some(BinaryOp::Shr),
        [SyntaxKind::LtLtLt] => Some(BinaryOp::Ashl),
        [SyntaxKind::GtGtGt] => Some(BinaryOp::Ashr),
        [SyntaxKind::Lt] => Some(BinaryOp::Lt),
        [SyntaxKind::LtEq] => Some(BinaryOp::LtEq),
        [SyntaxKind::Gt] => Some(BinaryOp::Gt),
        [SyntaxKind::GtEq] => Some(BinaryOp::GtEq),
        [SyntaxKind::EqEq] => Some(BinaryOp::Eq),
        [SyntaxKind::BangEq] => Some(BinaryOp::Neq),
        [SyntaxKind::EqEqEq] => Some(BinaryOp::CaseEq),
        [SyntaxKind::BangEqEq] => Some(BinaryOp::CaseNeq),
        [SyntaxKind::EqEqQuestion] => Some(BinaryOp::WildEq),
        [SyntaxKind::BangEqQuestion] => Some(BinaryOp::WildNeq),
        [SyntaxKind::AmpAmp] => Some(BinaryOp::LogAnd),
        [SyntaxKind::PipePipe] => Some(BinaryOp::LogOr),
        [SyntaxKind::StarStar] => Some(BinaryOp::Power),
        _ => None,
    }
}
