use lyra_ast::{AstNode, Expression, FieldExpr, IndexExpr, ParenExpr, RangeExpr};
use lyra_lexer::SyntaxKind;
use lyra_parser::SyntaxNode;

/// Classification of an expression appearing on the LHS of an assignment.
pub(crate) enum LhsClass {
    /// Simple assignable reference whose type can be queried via `expr_type`.
    Assignable(SyntaxNode),
    /// Valid SV lvalue form not yet type-checked by this engine
    /// (concat, streaming).
    Unsupported,
    /// Not a valid assignment target.
    NotAssignable,
}

/// Classify an LHS expression for assignment checking.
///
/// This is the single canonical classifier. Assignment type-checking,
/// modport lvalue checks, and builtin method arg validation all
/// derive their answers from this function.
pub(crate) fn classify_lhs(node: &SyntaxNode) -> LhsClass {
    match node.kind() {
        SyntaxKind::NameRef | SyntaxKind::QualifiedName => LhsClass::Assignable(node.clone()),
        SyntaxKind::FieldExpr => {
            if FieldExpr::cast(node.clone())
                .and_then(|f| f.base_expr())
                .is_some_and(|base| is_lvalue(&base))
            {
                LhsClass::Assignable(node.clone())
            } else {
                LhsClass::NotAssignable
            }
        }
        SyntaxKind::IndexExpr => {
            if IndexExpr::cast(node.clone())
                .and_then(|i| i.base_expr())
                .is_some_and(|base| is_lvalue(&base))
            {
                LhsClass::Assignable(node.clone())
            } else {
                LhsClass::NotAssignable
            }
        }
        SyntaxKind::RangeExpr => {
            if RangeExpr::cast(node.clone())
                .and_then(|r| r.base_expr())
                .is_some_and(|base| is_lvalue(&base))
            {
                LhsClass::Assignable(node.clone())
            } else {
                LhsClass::NotAssignable
            }
        }
        SyntaxKind::ParenExpr => match ParenExpr::cast(node.clone()).and_then(|p| p.inner()) {
            Some(inner) => classify_lhs(&inner),
            None => LhsClass::NotAssignable,
        },
        SyntaxKind::Expression => match Expression::cast(node.clone()).and_then(|e| e.inner()) {
            Some(inner) => classify_lhs(&inner),
            None => LhsClass::NotAssignable,
        },
        SyntaxKind::ConcatExpr | SyntaxKind::StreamExpr => LhsClass::Unsupported,
        _ => LhsClass::NotAssignable,
    }
}

/// Syntactically valid assignment target (Assignable or Unsupported).
/// Used by modport target legality checks.
pub fn is_lvalue(node: &SyntaxNode) -> bool {
    !matches!(classify_lhs(node), LhsClass::NotAssignable)
}

/// Simple assignable reference whose type can be queried.
/// Used by builtin method arg validation where a concrete
/// variable reference is required.
pub fn is_assignable_ref(node: &SyntaxNode) -> bool {
    matches!(classify_lhs(node), LhsClass::Assignable(_))
}
