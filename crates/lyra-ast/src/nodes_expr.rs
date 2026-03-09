// Custom accessors for expression nodes and operator classification enums.
//
// Split from nodes.rs to stay under the 800-line soft limit.

use lyra_lexer::SyntaxKind;
use lyra_parser::SyntaxToken;

use crate::nodes::{
    ArgList, BinExpr, CallExpr, CastExpr, ConcatExpr, CondExpr, EnumMember, EnumMemberRange,
    Expression, FieldExpr, IndexExpr, Literal, NewExpr, ParenExpr, PrefixExpr, RangeExpr,
    ReplicExpr, StreamExpr, StreamOperandItem, StreamOperands, StreamRange, StreamSliceSize,
    StreamWithClause, TaggedExpr, TypeSpec,
};
use crate::support;

impl Expression {
    /// The single wrapped expression child.
    pub fn inner(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 0)
    }

    /// The wrapped expression as an `Expr` handle (alias for `inner`).
    pub fn expr(&self) -> Option<crate::expr::Expr> {
        self.inner()
    }

    /// Unwrap the wrapper and peel any remaining wrapper forms.
    pub fn peeled_expr(&self) -> Option<crate::expr::Expr> {
        self.inner().and_then(|e| e.peeled())
    }
}

impl ParenExpr {
    /// The expression inside the parentheses.
    pub fn inner(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 0)
    }
}

impl BinExpr {
    pub fn op_token(&self) -> Option<SyntaxToken> {
        // The operator is the token between the two child nodes
        let mut saw_first_child = false;
        for el in self.syntax.children_with_tokens() {
            match el {
                rowan::NodeOrToken::Node(_) => {
                    if saw_first_child {
                        return None;
                    }
                    saw_first_child = true;
                }
                rowan::NodeOrToken::Token(tok) => {
                    if saw_first_child && tok.kind() != SyntaxKind::Whitespace {
                        return Some(tok);
                    }
                }
            }
        }
        None
    }

    pub fn lhs(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 0)
    }

    pub fn rhs(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 1)
    }

    pub fn binary_op(&self) -> Option<SyntaxBinaryOp> {
        SyntaxBinaryOp::from_token(self.op_token()?.kind())
    }
}

/// Syntactic binary operator classification.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SyntaxBinaryOp {
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

impl SyntaxBinaryOp {
    /// Flip a relational operator (`Lt` <-> `Gt`, `LtEq` <-> `GtEq`).
    /// Symmetric operators (`Eq`, `Neq`) return themselves. Non-comparison
    /// operators return `None`.
    pub fn flip(&self) -> Option<Self> {
        match self {
            Self::Lt => Some(Self::Gt),
            Self::Gt => Some(Self::Lt),
            Self::LtEq => Some(Self::GtEq),
            Self::GtEq => Some(Self::LtEq),
            Self::Eq | Self::Neq => Some(*self),
            _ => None,
        }
    }

    /// Whether this operator is a comparison (relational or equality).
    pub fn is_comparison(&self) -> bool {
        matches!(
            self,
            Self::Lt | Self::LtEq | Self::Gt | Self::GtEq | Self::Eq | Self::Neq
        )
    }

    pub fn from_token(kind: SyntaxKind) -> Option<SyntaxBinaryOp> {
        match kind {
            SyntaxKind::Plus => Some(SyntaxBinaryOp::Add),
            SyntaxKind::Minus => Some(SyntaxBinaryOp::Sub),
            SyntaxKind::Star => Some(SyntaxBinaryOp::Mul),
            SyntaxKind::Slash => Some(SyntaxBinaryOp::Div),
            SyntaxKind::Percent => Some(SyntaxBinaryOp::Mod),
            SyntaxKind::Amp => Some(SyntaxBinaryOp::BitAnd),
            SyntaxKind::Pipe => Some(SyntaxBinaryOp::BitOr),
            SyntaxKind::Caret => Some(SyntaxBinaryOp::BitXor),
            SyntaxKind::TildeCaret | SyntaxKind::CaretTilde => Some(SyntaxBinaryOp::BitXnor),
            SyntaxKind::LtLt => Some(SyntaxBinaryOp::Shl),
            SyntaxKind::GtGt => Some(SyntaxBinaryOp::Shr),
            SyntaxKind::LtLtLt => Some(SyntaxBinaryOp::Ashl),
            SyntaxKind::GtGtGt => Some(SyntaxBinaryOp::Ashr),
            SyntaxKind::Lt => Some(SyntaxBinaryOp::Lt),
            SyntaxKind::LtEq => Some(SyntaxBinaryOp::LtEq),
            SyntaxKind::Gt => Some(SyntaxBinaryOp::Gt),
            SyntaxKind::GtEq => Some(SyntaxBinaryOp::GtEq),
            SyntaxKind::EqEq => Some(SyntaxBinaryOp::Eq),
            SyntaxKind::BangEq => Some(SyntaxBinaryOp::Neq),
            SyntaxKind::EqEqEq => Some(SyntaxBinaryOp::CaseEq),
            SyntaxKind::BangEqEq => Some(SyntaxBinaryOp::CaseNeq),
            SyntaxKind::EqEqQuestion => Some(SyntaxBinaryOp::WildEq),
            SyntaxKind::BangEqQuestion => Some(SyntaxBinaryOp::WildNeq),
            SyntaxKind::AmpAmp => Some(SyntaxBinaryOp::LogAnd),
            SyntaxKind::PipePipe => Some(SyntaxBinaryOp::LogOr),
            SyntaxKind::StarStar => Some(SyntaxBinaryOp::Power),
            _ => None,
        }
    }
}

/// Syntactic assignment operator classification.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SyntaxAssignOp {
    Blocking,
    NonBlocking,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    AndAssign,
    OrAssign,
    XorAssign,
    ShlAssign,
    ShrAssign,
    AshlAssign,
    AshrAssign,
}

impl SyntaxAssignOp {
    pub fn from_token(kind: SyntaxKind) -> Option<SyntaxAssignOp> {
        match kind {
            SyntaxKind::Assign => Some(SyntaxAssignOp::Blocking),
            SyntaxKind::LtEq => Some(SyntaxAssignOp::NonBlocking),
            SyntaxKind::PlusEq => Some(SyntaxAssignOp::AddAssign),
            SyntaxKind::MinusEq => Some(SyntaxAssignOp::SubAssign),
            SyntaxKind::StarEq => Some(SyntaxAssignOp::MulAssign),
            SyntaxKind::SlashEq => Some(SyntaxAssignOp::DivAssign),
            SyntaxKind::PercentEq => Some(SyntaxAssignOp::ModAssign),
            SyntaxKind::AmpEq => Some(SyntaxAssignOp::AndAssign),
            SyntaxKind::PipeEq => Some(SyntaxAssignOp::OrAssign),
            SyntaxKind::CaretEq => Some(SyntaxAssignOp::XorAssign),
            SyntaxKind::LtLtEq => Some(SyntaxAssignOp::ShlAssign),
            SyntaxKind::GtGtEq => Some(SyntaxAssignOp::ShrAssign),
            SyntaxKind::LtLtLtEq => Some(SyntaxAssignOp::AshlAssign),
            SyntaxKind::GtGtGtEq => Some(SyntaxAssignOp::AshrAssign),
            _ => None,
        }
    }

    pub fn is_compound(&self) -> bool {
        !matches!(self, SyntaxAssignOp::Blocking | SyntaxAssignOp::NonBlocking)
    }
}

impl PrefixExpr {
    pub fn op_token(&self) -> Option<SyntaxToken> {
        self.syntax
            .children_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token)
            .find(|tok| tok.kind() != SyntaxKind::Whitespace)
    }

    pub fn operand(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 0)
    }

    pub fn prefix_op(&self) -> Option<SyntaxPrefixOp> {
        self.op_token()
            .and_then(|tok| SyntaxPrefixOp::from_token_kind(tok.kind()))
    }
}

/// Syntactic prefix operator classification.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SyntaxPrefixOp {
    Plus,
    Minus,
    LogNot,
    BitNot,
    RedAnd,
    RedOr,
    RedXor,
    RedNand,
    RedNor,
    RedXnor,
    Inc,
    Dec,
}

impl SyntaxPrefixOp {
    pub fn from_token_kind(kind: SyntaxKind) -> Option<Self> {
        match kind {
            SyntaxKind::Plus => Some(Self::Plus),
            SyntaxKind::Minus => Some(Self::Minus),
            SyntaxKind::Bang => Some(Self::LogNot),
            SyntaxKind::Tilde => Some(Self::BitNot),
            SyntaxKind::Amp => Some(Self::RedAnd),
            SyntaxKind::Pipe => Some(Self::RedOr),
            SyntaxKind::Caret => Some(Self::RedXor),
            SyntaxKind::TildeAmp => Some(Self::RedNand),
            SyntaxKind::TildePipe => Some(Self::RedNor),
            SyntaxKind::TildeCaret | SyntaxKind::CaretTilde => Some(Self::RedXnor),
            SyntaxKind::PlusPlus => Some(Self::Inc),
            SyntaxKind::MinusMinus => Some(Self::Dec),
            _ => None,
        }
    }

    pub fn is_reduction(self) -> bool {
        matches!(
            self,
            Self::RedAnd
                | Self::RedOr
                | Self::RedXor
                | Self::RedNand
                | Self::RedNor
                | Self::RedXnor
        )
    }

    pub fn is_inc_dec(self) -> bool {
        matches!(self, Self::Inc | Self::Dec)
    }
}

impl IndexExpr {
    /// The (base, index) expression pair. Returns None if not exactly 2
    /// expression children (error recovery).
    fn expr_pair(&self) -> Option<(crate::expr::Expr, crate::expr::Expr)> {
        let mut iter = support::expr_children(&self.syntax);
        let first = iter.next()?;
        let second = iter.next()?;
        if iter.next().is_some() {
            return None;
        }
        Some((first, second))
    }

    /// The base expression (left of the brackets).
    pub fn base_expr(&self) -> Option<crate::expr::Expr> {
        self.expr_pair().map(|(base, _)| base)
    }

    /// The index expression (inside the brackets).
    pub fn index_expr(&self) -> Option<crate::expr::Expr> {
        self.expr_pair().map(|(_, idx)| idx)
    }
}

impl FieldExpr {
    /// The base expression before the dot.
    pub fn base_expr(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 0)
    }

    /// The member name token after the dot.
    ///
    /// Accepts identifiers and keyword tokens used as method names
    /// by LRM 7.12 array manipulation methods (`and`, `or`, `xor`, `unique`).
    pub fn field_name(&self) -> Option<SyntaxToken> {
        self.syntax
            .children_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token)
            .filter(|tok| {
                matches!(
                    tok.kind(),
                    SyntaxKind::Ident
                        | SyntaxKind::EscapedIdent
                        | SyntaxKind::AndKw
                        | SyntaxKind::OrKw
                        | SyntaxKind::XorKw
                        | SyntaxKind::UniqueKw
                )
            })
            .last()
    }

    /// Member lookup name and token kind for this field access.
    ///
    /// Returns the payload used for member resolution: identifier tokens
    /// are normalized per LRM 5.6.1 (backslash stripped), while keyword
    /// method names (`and`, `or`, `xor`, `unique`) return their raw text.
    /// The `SyntaxKind` allows callers to distinguish keyword-based
    /// method dispatch from identifier-based member lookup.
    pub fn member_lookup_name(&self) -> Option<(SyntaxKind, smol_str::SmolStr)> {
        let tok = self.field_name()?;
        let text = match tok.kind() {
            SyntaxKind::Ident | SyntaxKind::EscapedIdent => crate::ident::semantic_spelling(&tok),
            _ => smol_str::SmolStr::new(tok.text()),
        };
        Some((tok.kind(), text))
    }
}

impl CondExpr {
    /// The condition expression (test position of `cond ? then : else`).
    pub fn condition(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 0)
    }

    pub fn then_expr(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 1)
    }

    pub fn else_expr(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 2)
    }
}

impl ConcatExpr {
    pub fn operands(&self) -> impl Iterator<Item = crate::expr::Expr> + '_ {
        support::expr_children(&self.syntax)
    }
}

impl crate::nodes::AssignmentPatternExpr {
    /// Expression children inside `AssignmentPatternItem` wrappers.
    pub fn operands(&self) -> impl Iterator<Item = crate::expr::Expr> + '_ {
        support::expr_children(&self.syntax)
    }
}

impl ReplicExpr {
    pub fn count(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 0)
    }

    pub fn body_exprs(&self) -> impl Iterator<Item = crate::expr::Expr> + '_ {
        support::expr_children(&self.syntax).skip(1)
    }
}

impl CallExpr {
    pub fn arg_list(&self) -> Option<ArgList> {
        support::child(&self.syntax)
    }

    pub fn callee(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 0)
    }

    /// The optional `with (expr)` clause (LRM 7.12).
    pub fn with_clause(&self) -> Option<crate::nodes::ArrayManipWithClause> {
        support::child(&self.syntax)
    }
}

impl crate::nodes::ArrayManipWithClause {
    /// The expression inside `with (expr)`.
    pub fn with_expr(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 0)
    }
}

/// Streaming direction: MSB-first (`>>`) or LSB-first (`<<`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum StreamDir {
    MsbFirst,
    LsbFirst,
}

impl StreamExpr {
    pub fn stream_operands(&self) -> Option<StreamOperands> {
        support::child(&self.syntax)
    }

    /// Streaming direction from the `>>` or `<<` direct child operator token.
    ///
    /// Uses `support::token` which searches only direct child tokens (not
    /// descendants), so this matches exactly the operator at the known
    /// position in the `StreamExpr` grammar.
    pub fn dir(&self) -> Option<StreamDir> {
        if support::token(&self.syntax, SyntaxKind::GtGt).is_some() {
            Some(StreamDir::MsbFirst)
        } else if support::token(&self.syntax, SyntaxKind::LtLt).is_some() {
            Some(StreamDir::LsbFirst)
        } else {
            None
        }
    }

    /// The optional `slice_size` child node.
    pub fn slice_size(&self) -> Option<StreamSliceSize> {
        support::child(&self.syntax)
    }

    /// Iterate streaming operand items in source order.
    pub fn operand_items(&self) -> impl Iterator<Item = StreamOperandItem> {
        self.stream_operands()
            .into_iter()
            .flat_map(|ops| ops.items())
    }
}

impl StreamSliceSize {
    /// The type specifier child, present when the slice size is a type.
    pub fn type_spec(&self) -> Option<TypeSpec> {
        support::child(&self.syntax)
    }

    /// The expression child, present when the slice size is an expression.
    pub fn expr(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 0)
    }
}

impl StreamOperands {
    pub fn items(&self) -> crate::support::AstChildren<StreamOperandItem> {
        support::children(&self.syntax)
    }
}

impl Literal {
    pub fn token(&self) -> Option<SyntaxToken> {
        support::token_in(
            &self.syntax,
            &[
                SyntaxKind::IntLiteral,
                SyntaxKind::RealLiteral,
                SyntaxKind::BasedLiteralPrefix,
                SyntaxKind::UnbasedUnsizedLiteral,
                SyntaxKind::StringLiteral,
            ],
        )
    }

    pub fn literal_kind(&self) -> Option<crate::expr::LiteralKind> {
        let tokens: Vec<SyntaxToken> = self
            .syntax
            .children_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token)
            .filter(|t| !t.kind().is_trivia())
            .collect();

        if tokens.is_empty() {
            return None;
        }

        // Check for BasedLiteralPrefix (new split representation)
        let prefix_pos = tokens
            .iter()
            .position(|t| t.kind() == SyntaxKind::BasedLiteralPrefix);
        if let Some(pp) = prefix_pos {
            let size_token = if pp > 0 && tokens[pp - 1].kind() == SyntaxKind::IntLiteral {
                Some(tokens[pp - 1].clone())
            } else {
                None
            };
            let digits_token = tokens
                .get(pp + 1)
                .filter(|t| t.kind() == SyntaxKind::BasedLiteralDigits)
                .cloned();
            return Some(crate::expr::LiteralKind::Based {
                size_token,
                prefix_token: tokens[pp].clone(),
                digits_token,
            });
        }

        let first = &tokens[0];
        match first.kind() {
            SyntaxKind::IntLiteral => Some(crate::expr::LiteralKind::Int {
                token: first.clone(),
            }),
            SyntaxKind::UnbasedUnsizedLiteral => Some(crate::expr::LiteralKind::UnbasedUnsized {
                token: first.clone(),
            }),
            SyntaxKind::RealLiteral => Some(crate::expr::LiteralKind::Real {
                token: first.clone(),
            }),
            SyntaxKind::StringLiteral => Some(crate::expr::LiteralKind::String {
                token: first.clone(),
            }),
            SyntaxKind::TimeLiteral => Some(crate::expr::LiteralKind::Time {
                token: first.clone(),
            }),
            _ => Some(crate::expr::LiteralKind::Unknown {
                token: first.clone(),
            }),
        }
    }
}

/// The kind of a range expression inside `[]`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RangeKind {
    /// `[hi:lo]` -- fixed part-select.
    Fixed,
    /// `[base+:width]` -- indexed part-select (ascending).
    IndexedPlus,
    /// `[base-:width]` -- indexed part-select (descending).
    IndexedMinus,
}

impl RangeExpr {
    /// Classify the range form by looking for direct `+` or `-` token children.
    ///
    /// Uses `support::token()` which only searches direct children of this
    /// node. A `+` inside a `BinExpr` child (e.g. `a[i+1:j]`) is NOT a
    /// direct child and will not trigger `IndexedPlus`.
    pub fn range_kind(&self) -> RangeKind {
        if support::token(&self.syntax, SyntaxKind::Plus).is_some() {
            RangeKind::IndexedPlus
        } else if support::token(&self.syntax, SyntaxKind::Minus).is_some() {
            RangeKind::IndexedMinus
        } else {
            RangeKind::Fixed
        }
    }

    /// The base object expression (before the brackets).
    /// For `a[hi:lo]` or `a[idx+:w]`, returns `a`.
    pub fn base_expr(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 0)
    }

    /// The two operand expressions inside the brackets.
    /// Fixed `[hi:lo]`: returns (hi, lo).
    /// Indexed `[idx+:w]` / `[idx-:w]`: returns (idx, w).
    pub fn operand_exprs(&self) -> Option<(crate::expr::Expr, crate::expr::Expr)> {
        let first = support::expr_child(&self.syntax, 1)?;
        let second = support::expr_child(&self.syntax, 2)?;
        Some((first, second))
    }
}

impl EnumMember {
    pub fn range_spec(&self) -> Option<EnumMemberRange> {
        support::child(&self.syntax)
    }

    pub fn init_expr(&self) -> Option<Expression> {
        support::child(&self.syntax)
    }
}

impl EnumMemberRange {
    pub fn first_expr(&self) -> Option<Expression> {
        support::children::<Expression>(&self.syntax).next()
    }

    pub fn second_expr(&self) -> Option<Expression> {
        support::children::<Expression>(&self.syntax).nth(1)
    }
}

impl NewExpr {
    /// Whether this is the dynamic-array form `new[...]` (vs class `new(...)`).
    pub fn has_brackets(&self) -> bool {
        support::token(&self.syntax, SyntaxKind::LBracket).is_some()
    }

    /// The size expression inside `new[size]`.
    pub fn size_expr(&self) -> Option<crate::expr::Expr> {
        if !self.has_brackets() {
            return None;
        }
        support::expr_child(&self.syntax, 0)
    }

    /// The initializer expression inside `new[size](init)`.
    pub fn init_arg(&self) -> Option<crate::expr::Expr> {
        let arg_list: Option<ArgList> = support::child(&self.syntax);
        let al = arg_list?;
        support::expr_child(&al.syntax, 0)
    }

    /// Count of expression children inside the `ArgList` child.
    pub fn init_arg_count(&self) -> usize {
        let arg_list: Option<ArgList> = support::child(&self.syntax);
        match arg_list {
            Some(al) => support::expr_children(&al.syntax).count(),
            None => 0,
        }
    }
}

impl TaggedExpr {
    /// The member name identifier token (direct child of `TaggedExpr`).
    ///
    /// The grammar is `tagged Ident [expr]`. The operand expression, even
    /// if it is a bare name, is wrapped in a child node (e.g. `NameRef`),
    /// so the only direct `Ident` token child is the member name.
    pub fn member_name(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, SyntaxKind::Ident)
    }

    /// The optional operand expression (LRM 11.9).
    pub fn operand(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 0)
    }
}

impl CastExpr {
    pub fn cast_type(&self) -> Option<TypeSpec> {
        support::child(&self.syntax)
    }

    /// The inner expression of the cast (the operand inside parentheses).
    /// Returns the first expression-kind direct child that is not a `TypeSpec`.
    pub fn inner_expr(&self) -> Option<crate::expr::Expr> {
        self.syntax
            .children()
            .find(|c| c.kind() != SyntaxKind::TypeSpec && crate::node::is_expression_kind(c.kind()))
            .and_then(crate::expr::Expr::cast)
    }
}

/// Classification of the range form inside a `StreamRange` node.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StreamRangeOp {
    /// `[expr]` -- single element.
    Single,
    /// `[lo : hi]` -- fixed range.
    Fixed,
    /// `[base +: width]` -- indexed ascending.
    IndexedPlus,
    /// `[base -: width]` -- indexed descending.
    IndexedMinus,
}

impl StreamOperandItem {
    /// The operand expression (first expression-kind child).
    pub fn expr(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 0)
    }

    /// The optional `with [...]` clause.
    pub fn with_clause(&self) -> Option<StreamWithClause> {
        support::child(&self.syntax)
    }
}

impl StreamWithClause {
    /// The range inside `[...]`.
    pub fn range(&self) -> Option<StreamRange> {
        support::child(&self.syntax)
    }
}

impl StreamRange {
    /// Classify the range form by inspecting direct token children.
    ///
    /// A `+` inside a `BinExpr` child (e.g. `[a + b]`) is NOT a direct
    /// token of `StreamRange`, so it cannot trigger `IndexedPlus`.
    pub fn op(&self) -> Option<StreamRangeOp> {
        let expr_count = support::expr_children(&self.syntax).count();
        match expr_count {
            1 => Some(StreamRangeOp::Single),
            2 => {
                // Collect direct non-trivia tokens between expression children
                let direct_tokens: Vec<SyntaxKind> = self
                    .syntax
                    .children_with_tokens()
                    .filter_map(rowan::NodeOrToken::into_token)
                    .map(|t| t.kind())
                    .filter(|k| {
                        !matches!(
                            k,
                            SyntaxKind::Whitespace
                                | SyntaxKind::LineComment
                                | SyntaxKind::BlockComment
                        )
                    })
                    .collect();
                // Look for Plus+Colon, Minus+Colon, or lone Colon
                let has_plus_colon = direct_tokens
                    .windows(2)
                    .any(|w| w[0] == SyntaxKind::Plus && w[1] == SyntaxKind::Colon);
                let has_minus_colon = direct_tokens
                    .windows(2)
                    .any(|w| w[0] == SyntaxKind::Minus && w[1] == SyntaxKind::Colon);
                if has_plus_colon {
                    Some(StreamRangeOp::IndexedPlus)
                } else if has_minus_colon {
                    Some(StreamRangeOp::IndexedMinus)
                } else if direct_tokens.contains(&SyntaxKind::Colon) {
                    Some(StreamRangeOp::Fixed)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// First expression child (the index/base).
    pub fn lhs(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 0)
    }

    /// Second expression child (upper bound/width), `None` for `Single`.
    pub fn rhs(&self) -> Option<crate::expr::Expr> {
        support::expr_child(&self.syntax, 1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::node::AstNode;

    fn find_range(node: &lyra_parser::SyntaxNode) -> Option<RangeExpr> {
        if node.kind() == SyntaxKind::RangeExpr {
            return RangeExpr::cast(node.clone());
        }
        for child in node.children() {
            if let Some(r) = find_range(&child) {
                return Some(r);
            }
        }
        None
    }

    fn parse_range_expr(src: &str) -> RangeExpr {
        let tokens = lyra_lexer::lex(src);
        let pp = lyra_preprocess::preprocess_identity(lyra_source::FileId(0), &tokens, src);
        let parse = lyra_parser::parse(&pp.tokens, &pp.expanded_text);
        find_range(&parse.syntax()).expect("should contain a RangeExpr")
    }

    #[test]
    fn range_kind_indexed_plus() {
        let re = parse_range_expr("module m; parameter P = a[i+:4]; endmodule");
        assert_eq!(re.range_kind(), RangeKind::IndexedPlus);
    }

    #[test]
    fn range_kind_indexed_minus() {
        let re = parse_range_expr("module m; parameter P = a[i-:4]; endmodule");
        assert_eq!(re.range_kind(), RangeKind::IndexedMinus);
    }

    #[test]
    fn range_kind_fixed_with_plus_in_expr() {
        // a[i+1:j] -- the + is inside BinExpr, not a direct child
        let re = parse_range_expr("module m; parameter P = a[i+1:j]; endmodule");
        assert_eq!(re.range_kind(), RangeKind::Fixed);
    }
}
