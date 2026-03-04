// Custom accessors for statement nodes.
//
// Split from nodes.rs to stay under the 1200-line hard limit.

use lyra_lexer::SyntaxKind;
use lyra_parser::SyntaxToken;

use crate::node::AstNode;
use crate::nodes::{
    AssignStmt, BinExpr, BlockStmt, CaseInsideItem, CaseItem, CaseStmt, ContinuousAssign,
    DoWhileStmt, DriveStrength, ForStmt, ForeachStmt, ForeachVarList, ForeverStmt, IfStmt, NameRef,
    RangeList, RepeatStmt, ReturnStmt, TimingControl, ValueRange, WhileStmt,
};
use crate::nodes_expr::SyntaxAssignOp;
use crate::support::{self, AstChildren};

impl BlockStmt {
    /// Iterate over all statement-kind children.
    pub fn statements(&self) -> AstChildren<crate::node::StmtNode> {
        support::children(&self.syntax)
    }

    /// The block label identifier after `begin :`, if present.
    pub fn block_name(&self) -> Option<SyntaxToken> {
        let mut saw_begin = false;
        let mut saw_colon = false;
        for el in self.syntax.children_with_tokens() {
            match el {
                rowan::NodeOrToken::Token(tok) => match tok.kind() {
                    SyntaxKind::BeginKw => saw_begin = true,
                    SyntaxKind::Colon if saw_begin => saw_colon = true,
                    SyntaxKind::Ident if saw_colon => return Some(tok),
                    _ => {
                        if saw_begin && !saw_colon && !tok.kind().is_trivia() {
                            return None;
                        }
                    }
                },
                rowan::NodeOrToken::Node(_) => {
                    if saw_begin {
                        return None;
                    }
                }
            }
        }
        None
    }
}

impl IfStmt {
    /// The condition expression.
    pub fn condition(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 0)
    }

    /// The then-branch body (first statement-kind child).
    pub fn then_body(&self) -> Option<crate::node::StmtNode> {
        support::children::<crate::node::StmtNode>(&self.syntax).next()
    }

    /// The else-branch body (second statement-kind child), or `None`.
    pub fn else_body(&self) -> Option<crate::node::StmtNode> {
        support::children::<crate::node::StmtNode>(&self.syntax).nth(1)
    }

    /// Whether this if-statement has an `else` clause.
    pub fn has_else(&self) -> bool {
        support::token(&self.syntax, SyntaxKind::ElseKw).is_some()
    }
}

impl CaseStmt {
    /// The selector expression (first expression-kind node between parens).
    pub fn selector(&self) -> Option<crate::expr::Expr> {
        let mut in_parens = false;
        for el in self.syntax.children_with_tokens() {
            match el {
                rowan::NodeOrToken::Token(tok) => {
                    if tok.kind() == SyntaxKind::LParen {
                        in_parens = true;
                    } else if tok.kind() == SyntaxKind::RParen {
                        break;
                    }
                }
                rowan::NodeOrToken::Node(node) => {
                    if in_parens {
                        return crate::expr::Expr::cast(node);
                    }
                }
            }
        }
        None
    }

    /// Whether this is a `case ... inside` statement (LRM 12.5.4).
    pub fn is_inside(&self) -> bool {
        support::token(&self.syntax, SyntaxKind::InsideKw).is_some()
    }

    /// Iterate normal `CaseItem` children only (excludes `CaseInsideItem`).
    pub fn normal_items(&self) -> AstChildren<CaseItem> {
        support::children(&self.syntax)
    }

    /// Iterate all case items (normal or inside) as a unified type.
    pub fn items(&self) -> impl Iterator<Item = CaseItemLike> + '_ {
        self.syntax
            .children()
            .filter_map(|child| match child.kind() {
                SyntaxKind::CaseItem => CaseItem::cast(child).map(CaseItemLike::Normal),
                SyntaxKind::CaseInsideItem => CaseInsideItem::cast(child).map(CaseItemLike::Inside),
                _ => None,
            })
    }
}

/// A case item from either a normal `case` or a `case inside` statement.
#[derive(Debug, Clone)]
pub enum CaseItemLike {
    Normal(CaseItem),
    Inside(CaseInsideItem),
}

impl CaseInsideItem {
    /// Whether this is a `default` item.
    pub fn is_default(&self) -> bool {
        support::token(&self.syntax, SyntaxKind::DefaultKw).is_some()
    }

    /// The range list (None for default items).
    pub fn range_list(&self) -> Option<RangeList> {
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
}

impl RangeList {
    /// Iterate over individual value ranges.
    pub fn value_ranges(&self) -> AstChildren<ValueRange> {
        support::children(&self.syntax)
    }
}

/// Classification of a single value range element.
#[derive(Debug, Clone)]
pub enum ValueRangeKind {
    /// A single expression (e.g. `1`).
    SingleExpr(crate::expr::Expr),
    /// A bracketed range `[lo : hi]`.
    BracketRange {
        lo: crate::expr::Expr,
        hi: crate::expr::Expr,
    },
}

impl ValueRange {
    /// Classify this value range as a single expression or a bracketed range.
    pub fn kind(&self) -> Option<ValueRangeKind> {
        let has_bracket = support::token(&self.syntax, SyntaxKind::LBracket).is_some();
        if has_bracket {
            let mut exprs = support::expr_children(&self.syntax);
            let lo = exprs.next()?;
            let hi = exprs.next()?;
            Some(ValueRangeKind::BracketRange { lo, hi })
        } else {
            let expr = support::expr_children(&self.syntax).next()?;
            Some(ValueRangeKind::SingleExpr(expr))
        }
    }
}

impl CaseItem {
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

    /// Label expressions before the `Colon` token.
    pub fn labels(&self) -> impl Iterator<Item = crate::expr::Expr> + '_ {
        let mut it = self.syntax.children_with_tokens();
        std::iter::from_fn(move || {
            loop {
                match it.next()? {
                    rowan::NodeOrToken::Token(tok) => {
                        if tok.kind() == SyntaxKind::Colon {
                            return None;
                        }
                    }
                    rowan::NodeOrToken::Node(node) => {
                        if let Some(expr) = crate::expr::Expr::cast(node) {
                            return Some(expr);
                        }
                    }
                }
            }
        })
    }
}

impl ForStmt {
    /// The loop body (last statement-kind child).
    pub fn body(&self) -> Option<crate::node::StmtNode> {
        support::children::<crate::node::StmtNode>(&self.syntax).last()
    }

    /// The genvar name from the init clause (first `NameRef` between `(` and first `;`).
    pub fn init_name(&self) -> Option<NameRef> {
        let mut in_parens = false;
        for el in self.syntax.children_with_tokens() {
            match el {
                rowan::NodeOrToken::Token(tok) => match tok.kind() {
                    SyntaxKind::LParen => in_parens = true,
                    SyntaxKind::Semicolon => return None,
                    _ => {}
                },
                rowan::NodeOrToken::Node(node) => {
                    if in_parens && let Some(nr) = NameRef::cast(node) {
                        return Some(nr);
                    }
                }
            }
        }
        None
    }

    /// The init value expression (first expr-kind node after `=` in the init clause).
    pub fn init_value(&self) -> Option<crate::expr::Expr> {
        let mut in_parens = false;
        let mut past_assign = false;
        for el in self.syntax.children_with_tokens() {
            match el {
                rowan::NodeOrToken::Token(tok) => match tok.kind() {
                    SyntaxKind::LParen => in_parens = true,
                    SyntaxKind::Assign if in_parens => past_assign = true,
                    SyntaxKind::Semicolon => return None,
                    _ => {}
                },
                rowan::NodeOrToken::Node(node) => {
                    if past_assign && crate::node::is_expression_kind(node.kind()) {
                        return crate::expr::Expr::cast(node);
                    }
                }
            }
        }
        None
    }

    /// The condition expression (first expr-kind node between first and second `;`).
    pub fn condition(&self) -> Option<crate::expr::Expr> {
        self.node_between_semis(1).and_then(crate::expr::Expr::cast)
    }

    /// The condition as a `BinExpr` (generate-for context where the condition
    /// is always a binary comparison like `i < N`).
    pub fn condition_bin(&self) -> Option<BinExpr> {
        self.node_between_semis(1).and_then(BinExpr::cast)
    }

    /// First node child in the region after the `n`-th `;` and before the next
    /// `;` or `)`.
    fn node_between_semis(&self, target_semi: usize) -> Option<lyra_parser::SyntaxNode> {
        let mut semi_count = 0;
        for el in self.syntax.children_with_tokens() {
            match el {
                rowan::NodeOrToken::Token(tok) if tok.kind() == SyntaxKind::Semicolon => {
                    semi_count += 1;
                    if semi_count > target_semi {
                        return None;
                    }
                }
                rowan::NodeOrToken::Token(tok) if tok.kind() == SyntaxKind::RParen => {
                    return None;
                }
                rowan::NodeOrToken::Node(node) => {
                    if semi_count == target_semi {
                        return Some(node);
                    }
                }
                rowan::NodeOrToken::Token(_) => {}
            }
        }
        None
    }

    /// The step expression as a `BinExpr` (first `BinExpr` between the second `;` and `)`).
    ///
    /// The step region contains `NameRef = BinExpr`, so this skips the
    /// assignment target and returns the `BinExpr` directly.
    pub fn step_bin(&self) -> Option<BinExpr> {
        let mut semi_count = 0;
        for el in self.syntax.children_with_tokens() {
            match el {
                rowan::NodeOrToken::Token(tok) => match tok.kind() {
                    SyntaxKind::Semicolon => semi_count += 1,
                    SyntaxKind::RParen if semi_count >= 2 => return None,
                    _ => {}
                },
                rowan::NodeOrToken::Node(node) => {
                    if semi_count >= 2 && node.kind() == SyntaxKind::BinExpr {
                        return BinExpr::cast(node);
                    }
                }
            }
        }
        None
    }
}

impl WhileStmt {
    /// The loop body.
    pub fn body(&self) -> Option<crate::node::StmtNode> {
        support::child(&self.syntax)
    }
}

impl RepeatStmt {
    /// The loop body.
    pub fn body(&self) -> Option<crate::node::StmtNode> {
        support::child(&self.syntax)
    }
}

impl ForeverStmt {
    /// The loop body.
    pub fn body(&self) -> Option<crate::node::StmtNode> {
        support::child(&self.syntax)
    }
}

impl DoWhileStmt {
    /// The loop body (statement between `do` and `while`).
    pub fn body(&self) -> Option<crate::node::StmtNode> {
        support::child(&self.syntax)
    }

    /// The condition expression inside `while (...)`.
    pub fn condition(&self) -> Option<crate::expr::Expr> {
        let mut past_while = false;
        for el in self.syntax.children_with_tokens() {
            match el {
                rowan::NodeOrToken::Token(tok) if tok.kind() == SyntaxKind::WhileKw => {
                    past_while = true;
                }
                rowan::NodeOrToken::Node(node) if past_while => {
                    return crate::expr::Expr::cast(node);
                }
                _ => {}
            }
        }
        None
    }
}

impl ReturnStmt {
    /// The `return` keyword token.
    pub fn return_kw(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, SyntaxKind::ReturnKw)
    }

    /// The return value expression, if present.
    pub fn value(&self) -> Option<crate::expr::Expr> {
        support::expr_children(&self.syntax).next()
    }
}

impl ForeachStmt {
    /// The array expression before the loop-variable list.
    pub fn array_expr(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 0)
    }

    /// The `[var1, , var3]` loop-variable list.
    pub fn var_list(&self) -> Option<ForeachVarList> {
        support::child(&self.syntax)
    }

    /// The loop body statement.
    pub fn body(&self) -> Option<crate::node::StmtNode> {
        support::child(&self.syntax)
    }
}

impl TimingControl {
    /// The body statement following the timing control.
    pub fn body(&self) -> Option<crate::node::StmtNode> {
        support::child(&self.syntax)
    }
}

impl AssignStmt {
    pub fn op_token(&self) -> Option<SyntaxToken> {
        self.syntax
            .children_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token)
            .find(|tok| SyntaxAssignOp::from_token(tok.kind()).is_some())
    }

    pub fn assign_op(&self) -> Option<SyntaxAssignOp> {
        SyntaxAssignOp::from_token(self.op_token()?.kind())
    }

    pub fn lhs(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 0)
    }

    pub fn rhs(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 1)
    }

    /// Optional timing control (intra-assignment delay).
    pub fn timing_control(&self) -> Option<TimingControl> {
        support::child(&self.syntax)
    }
}

impl ContinuousAssign {
    pub fn lhs(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 0)
    }

    pub fn rhs(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 1)
    }

    /// The `=` assignment operator token.
    pub fn eq_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, SyntaxKind::Assign)
    }

    /// Optional timing control (delay) on the assign.
    pub fn timing_control(&self) -> Option<TimingControl> {
        support::child(&self.syntax)
    }

    /// Optional drive strength on the assign (LRM 6.3.2).
    pub fn drive_strength(&self) -> Option<DriveStrength> {
        support::child(&self.syntax)
    }
}
