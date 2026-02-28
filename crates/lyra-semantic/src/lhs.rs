use lyra_ast::{Expr, ExprKind};
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
    let Some(expr) = Expr::peel(node) else {
        return LhsClass::NotAssignable;
    };
    let Some(ek) = expr.classify() else {
        return LhsClass::NotAssignable;
    };
    let expr_node = ek.syntax().clone();
    match ek {
        ExprKind::NameRef(_) | ExprKind::QualifiedName(_) => LhsClass::Assignable(expr_node),
        ExprKind::FieldExpr(f) => {
            if f.base_expr().is_some_and(|base| is_lvalue(base.syntax())) {
                LhsClass::Assignable(expr_node)
            } else {
                LhsClass::NotAssignable
            }
        }
        ExprKind::IndexExpr(i) => {
            if i.base_expr().is_some_and(|base| is_lvalue(base.syntax())) {
                LhsClass::Assignable(expr_node)
            } else {
                LhsClass::NotAssignable
            }
        }
        ExprKind::RangeExpr(r) => {
            if r.base_expr().is_some_and(|base| is_lvalue(base.syntax())) {
                LhsClass::Assignable(expr_node)
            } else {
                LhsClass::NotAssignable
            }
        }
        ExprKind::ConcatExpr(_) | ExprKind::StreamExpr(_) => LhsClass::Unsupported,
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
