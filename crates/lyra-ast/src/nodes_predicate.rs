// Typed accessors for conditional-predicate syntax nodes (LRM 12.6).

use lyra_lexer::SyntaxKind;

use crate::node::AstNode;
use crate::nodes::{CasePatternItem, CondPredicate, CondPredicateGuard, MatchesExpr};
use crate::nodes_pattern::PatternNode;
use crate::support::{self, AstChildren};

impl MatchesExpr {
    /// The LHS expression (the value being tested).
    pub fn expr(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 0)
    }

    /// The RHS pattern.
    pub fn pattern(&self) -> Option<PatternNode> {
        self.syntax.children().find_map(PatternNode::cast)
    }
}

impl CondPredicate {
    /// The head expression or `MatchesExpr` (first child node).
    pub fn head_expr(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 0)
    }

    /// The `MatchesExpr` child, if the head is a `matches` form.
    pub fn matches_expr(&self) -> Option<MatchesExpr> {
        support::child(&self.syntax)
    }

    /// Iterate over `&&&` guard clauses.
    pub fn guards(&self) -> AstChildren<CondPredicateGuard> {
        support::children(&self.syntax)
    }
}

impl CondPredicateGuard {
    /// The guard expression after `&&&`.
    pub fn expr(&self) -> Option<crate::expr::Expr> {
        support::expr_children(&self.syntax).next()
    }
}

impl CasePatternItem {
    /// The pattern for this case item (None for default items).
    pub fn pattern(&self) -> Option<PatternNode> {
        self.syntax.children().find_map(PatternNode::cast)
    }

    /// The optional `&&&` guard expression.
    pub fn guard(&self) -> Option<CondPredicateGuard> {
        support::child(&self.syntax)
    }

    /// The body statement after the colon.
    pub fn body(&self) -> Option<crate::node::StmtNode> {
        let mut past_colon = false;
        for el in self.syntax.children_with_tokens() {
            match el {
                rowan::NodeOrToken::Token(tok) if tok.kind() == SyntaxKind::Colon => {
                    past_colon = true;
                }
                rowan::NodeOrToken::Node(node) if past_colon => {
                    return crate::node::StmtNode::cast(node);
                }
                _ => {}
            }
        }
        None
    }

    /// Whether this is a `default` case item.
    pub fn is_default(&self) -> bool {
        support::token(&self.syntax, SyntaxKind::DefaultKw).is_some()
    }
}
