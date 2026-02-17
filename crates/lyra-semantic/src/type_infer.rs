use lyra_lexer::SyntaxKind;
use lyra_parser::SyntaxNode;

use crate::literal::parse_literal_shape;
use crate::types::{ConstEvalError, ConstInt, RealKw, SymbolType, Ty};

/// Signedness for bit-vector expression typing.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Signedness {
    Signed,
    Unsigned,
}

/// Bit width: known or unknown (e.g. unevaluated dim bounds).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BitWidth {
    Known(u32),
    Unknown,
}

/// A bit-vector value type (integral expressions).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BitVecType {
    pub width: BitWidth,
    pub signed: Signedness,
}

/// Reasons an expression's type could not be determined.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExprTypeErrorKind {
    Unresolved,
    NameRefIsTypeNotValue,
    UnsupportedExprKind,
    UnsupportedLiteralKind,
    UnsupportedBinaryOp,
    UnsupportedSystemCall,
    NonBitOperand,
    ConcatNonBitOperand,
    CondBranchTypeMismatch,
    InvalidReplicationCount,
    ReplicationConstEvalFailed(ConstEvalError),
    IndexNonIndexable,
    FieldAccessUnsupported,
    UserCallUnsupported,
    RangeUnsupported,
}

/// Result of typing an expression (self-determined).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExprType {
    /// Integral bit-vector (logic/int/bit/etc). All expression operations
    /// normalize the keyword to logic -- we don't track keyword in `BitVecType`.
    BitVec(BitVecType),
    /// Non-bit type. Wraps `Ty` directly (Real, String, Chandle, Event, Void)
    /// to avoid duplicating the `Ty` variant set.
    NonBit(Ty),
    Error(ExprTypeErrorKind),
}

impl ExprType {
    /// Convert a `Ty` to `ExprType`. The only `Ty` -> `ExprType` conversion path.
    pub fn from_ty(ty: &Ty) -> ExprType {
        match ty {
            Ty::Integral(i) => {
                let width = match i.try_packed_width() {
                    Some(w) => BitWidth::Known(w),
                    None => BitWidth::Unknown,
                };
                let signed = if i.signed {
                    Signedness::Signed
                } else {
                    Signedness::Unsigned
                };
                ExprType::BitVec(BitVecType { width, signed })
            }
            Ty::Real(_) | Ty::String | Ty::Chandle | Ty::Event | Ty::Void => {
                ExprType::NonBit(ty.clone())
            }
            Ty::Error => ExprType::Error(ExprTypeErrorKind::Unresolved),
        }
    }

    /// Map `SymbolType` to `ExprType`.
    pub fn from_symbol_type(st: &SymbolType) -> ExprType {
        match st {
            SymbolType::Value(ty) => ExprType::from_ty(ty),
            SymbolType::Net(net) => ExprType::from_ty(&net.data),
            SymbolType::TypeAlias(_) => ExprType::Error(ExprTypeErrorKind::NameRefIsTypeNotValue),
            SymbolType::Error(_) => ExprType::Error(ExprTypeErrorKind::Unresolved),
        }
    }

    /// Unsized decimal literal default: `BitVec(32, Signed)`.
    pub fn int_literal() -> ExprType {
        ExprType::BitVec(BitVecType {
            width: BitWidth::Known(32),
            signed: Signedness::Signed,
        })
    }

    /// Sized bit-vector.
    pub fn bits(width: u32, signed: Signedness) -> ExprType {
        ExprType::BitVec(BitVecType {
            width: BitWidth::Known(width),
            signed,
        })
    }

    /// Single-bit unsigned (reduction/logical results).
    pub fn one_bit() -> ExprType {
        ExprType::bits(1, Signedness::Unsigned)
    }
}

/// Callbacks for the inference engine. No DB access -- pure.
pub trait InferCtx {
    /// Resolve a `NameRef`/`QualifiedName` to its type.
    fn type_of_name(&self, name_node: &SyntaxNode) -> ExprType;
    /// Resolve a `NameRef`/`QualifiedName` to its `SymbolType` (for unpacked dim peeling).
    fn symbol_type_of_name(&self, name_node: &SyntaxNode) -> Option<SymbolType>;
    /// Evaluate a constant expression (for replication count).
    fn const_eval(&self, expr_node: &SyntaxNode) -> ConstInt;
}

/// Infer the self-determined type of an expression node.
///
/// `expected` is `None` for M4 (self-determined only).
/// M5 will pass `Some(target_type)` for context-determined sizing.
pub fn infer_expr_type(
    expr: &SyntaxNode,
    ctx: &dyn InferCtx,
    _expected: Option<&ExprType>,
) -> ExprType {
    match expr.kind() {
        SyntaxKind::NameRef | SyntaxKind::QualifiedName => ctx.type_of_name(expr),
        SyntaxKind::Literal => infer_literal(expr),
        SyntaxKind::Expression | SyntaxKind::ParenExpr => match expr.first_child() {
            Some(child) => infer_expr_type(&child, ctx, None),
            None => ExprType::Error(ExprTypeErrorKind::UnsupportedExprKind),
        },
        SyntaxKind::PrefixExpr => infer_prefix(expr, ctx),
        SyntaxKind::BinExpr => infer_binary(expr, ctx),
        SyntaxKind::CondExpr => infer_cond(expr, ctx),
        SyntaxKind::ConcatExpr => infer_concat(expr, ctx),
        SyntaxKind::ReplicExpr => infer_replic(expr, ctx),
        SyntaxKind::IndexExpr => infer_index(expr, ctx),
        SyntaxKind::RangeExpr => ExprType::Error(ExprTypeErrorKind::RangeUnsupported),
        SyntaxKind::FieldExpr => ExprType::Error(ExprTypeErrorKind::FieldAccessUnsupported),
        SyntaxKind::CallExpr => infer_call(expr, ctx),
        _ => ExprType::Error(ExprTypeErrorKind::UnsupportedExprKind),
    }
}

fn infer_literal(node: &SyntaxNode) -> ExprType {
    // Check for real literal
    let has_real = node.children_with_tokens().any(|el| {
        el.as_token()
            .is_some_and(|t| t.kind() == SyntaxKind::RealLiteral)
    });
    if has_real {
        return ExprType::NonBit(Ty::Real(RealKw::Real));
    }

    // Check for string literal
    let has_string = node.children_with_tokens().any(|el| {
        el.as_token()
            .is_some_and(|t| t.kind() == SyntaxKind::StringLiteral)
    });
    if has_string {
        return ExprType::NonBit(Ty::String);
    }

    // Numeric literal: use shared shape parser
    match parse_literal_shape(node) {
        Some(shape) => {
            let signed = if shape.signed {
                Signedness::Signed
            } else {
                Signedness::Unsigned
            };
            ExprType::bits(shape.width, signed)
        }
        None => ExprType::Error(ExprTypeErrorKind::UnsupportedLiteralKind),
    }
}

fn infer_prefix(node: &SyntaxNode, ctx: &dyn InferCtx) -> ExprType {
    let Some(op) = find_operator_token(node) else {
        return ExprType::Error(ExprTypeErrorKind::UnsupportedExprKind);
    };

    let Some(operand_node) = node.children().find(|c| is_expression_kind(c.kind())) else {
        return ExprType::Error(ExprTypeErrorKind::UnsupportedExprKind);
    };

    let operand = infer_expr_type(&operand_node, ctx, None);

    match op {
        // Logical negation: always 1-bit unsigned
        SyntaxKind::Bang
        // Reduction operators: always 1-bit unsigned
        | SyntaxKind::Amp
        | SyntaxKind::Pipe
        | SyntaxKind::Caret
        // Compound reduction operators: ~& ~| ~^ ^~ (lexed as single tokens)
        | SyntaxKind::TildeAmp
        | SyntaxKind::TildePipe
        | SyntaxKind::TildeCaret
        | SyntaxKind::CaretTilde => ExprType::one_bit(),
        // Bitwise NOT or unary +/-: same width and sign as operand
        SyntaxKind::Tilde | SyntaxKind::Plus | SyntaxKind::Minus => match &operand {
            ExprType::BitVec(_) | ExprType::Error(_) => operand,
            ExprType::NonBit(_) => ExprType::Error(ExprTypeErrorKind::NonBitOperand),
        },
        _ => ExprType::Error(ExprTypeErrorKind::UnsupportedExprKind),
    }
}

fn infer_binary(node: &SyntaxNode, ctx: &dyn InferCtx) -> ExprType {
    let children: Vec<SyntaxNode> = node
        .children()
        .filter(|c| is_expression_kind(c.kind()))
        .collect();
    if children.len() < 2 {
        return ExprType::Error(ExprTypeErrorKind::UnsupportedExprKind);
    }

    let lhs = infer_expr_type(&children[0], ctx, None);
    let rhs = infer_expr_type(&children[1], ctx, None);

    let Some(op) = find_binary_op(node) else {
        return ExprType::Error(ExprTypeErrorKind::UnsupportedExprKind);
    };

    match op {
        // Arithmetic and bitwise: merge_bitvec
        BinaryOp::Add
        | BinaryOp::Sub
        | BinaryOp::Mul
        | BinaryOp::Div
        | BinaryOp::Mod
        | BinaryOp::BitAnd
        | BinaryOp::BitOr
        | BinaryOp::BitXor
        | BinaryOp::BitXnor => match (&lhs, &rhs) {
            (ExprType::BitVec(a), ExprType::BitVec(b)) => ExprType::BitVec(merge_bitvec(a, b)),
            (ExprType::Error(_), _) => lhs,
            (_, ExprType::Error(_)) => rhs,
            _ => ExprType::Error(ExprTypeErrorKind::NonBitOperand),
        },
        // Shift: width=lhs, sign=lhs
        BinaryOp::Shl | BinaryOp::Shr | BinaryOp::Ashl | BinaryOp::Ashr => match &lhs {
            ExprType::BitVec(_) => match &rhs {
                ExprType::BitVec(_) | ExprType::Error(_) => lhs,
                ExprType::NonBit(_) => ExprType::Error(ExprTypeErrorKind::NonBitOperand),
            },
            ExprType::Error(_) => lhs,
            ExprType::NonBit(_) => ExprType::Error(ExprTypeErrorKind::NonBitOperand),
        },
        // Relational, equality, logical -- all produce 1-bit unsigned
        BinaryOp::Lt
        | BinaryOp::LtEq
        | BinaryOp::Gt
        | BinaryOp::GtEq
        | BinaryOp::Eq
        | BinaryOp::Neq
        | BinaryOp::CaseEq
        | BinaryOp::CaseNeq
        | BinaryOp::WildEq
        | BinaryOp::WildNeq
        | BinaryOp::LogAnd
        | BinaryOp::LogOr => ExprType::one_bit(),
        // Power: unsupported for M4
        BinaryOp::Power => ExprType::Error(ExprTypeErrorKind::UnsupportedBinaryOp),
    }
}

fn infer_cond(node: &SyntaxNode, ctx: &dyn InferCtx) -> ExprType {
    let children: Vec<SyntaxNode> = node
        .children()
        .filter(|c| is_expression_kind(c.kind()))
        .collect();
    if children.len() < 3 {
        return ExprType::Error(ExprTypeErrorKind::UnsupportedExprKind);
    }

    // condition is self-determined independently (not used for result type)
    let _cond = infer_expr_type(&children[0], ctx, None);
    let then_ty = infer_expr_type(&children[1], ctx, None);
    let else_ty = infer_expr_type(&children[2], ctx, None);

    match (&then_ty, &else_ty) {
        (ExprType::BitVec(a), ExprType::BitVec(b)) => ExprType::BitVec(merge_bitvec(a, b)),
        (ExprType::NonBit(a), ExprType::NonBit(b)) if a == b => then_ty,
        (ExprType::Error(_), _) => then_ty,
        (_, ExprType::Error(_)) => else_ty,
        _ => ExprType::Error(ExprTypeErrorKind::CondBranchTypeMismatch),
    }
}

fn infer_concat(node: &SyntaxNode, ctx: &dyn InferCtx) -> ExprType {
    let mut total_width: Option<u32> = Some(0);
    let mut all_known = true;

    for child in node.children() {
        if !is_expression_kind(child.kind()) {
            continue;
        }
        let child_ty = infer_expr_type(&child, ctx, None);
        match &child_ty {
            ExprType::BitVec(bv) => match bv.width {
                BitWidth::Known(w) => {
                    if let Some(ref mut tw) = total_width {
                        *tw = tw.saturating_add(w);
                    }
                }
                BitWidth::Unknown => {
                    all_known = false;
                }
            },
            ExprType::NonBit(_) => {
                return ExprType::Error(ExprTypeErrorKind::ConcatNonBitOperand);
            }
            ExprType::Error(_) => return child_ty,
        }
    }

    let width = if all_known {
        BitWidth::Known(total_width.unwrap_or(0))
    } else {
        BitWidth::Unknown
    };

    ExprType::BitVec(BitVecType {
        width,
        signed: Signedness::Unsigned,
    })
}

fn infer_replic(node: &SyntaxNode, ctx: &dyn InferCtx) -> ExprType {
    let children: Vec<SyntaxNode> = node.children().collect();

    let expr_children: Vec<SyntaxNode> = children
        .iter()
        .filter(|c| is_expression_kind(c.kind()))
        .cloned()
        .collect();

    if expr_children.is_empty() {
        return ExprType::Error(ExprTypeErrorKind::UnsupportedExprKind);
    }

    // First expression child is the replication count
    let count_node = &expr_children[0];
    let count = ctx.const_eval(count_node);

    let replic_count = match count {
        ConstInt::Known(n) => {
            if n <= 0 {
                return ExprType::Error(ExprTypeErrorKind::InvalidReplicationCount);
            }
            u32::try_from(n).ok()
        }
        ConstInt::Error(e) => {
            return ExprType::Error(ExprTypeErrorKind::ReplicationConstEvalFailed(e));
        }
        ConstInt::Unevaluated(_) => None,
    };

    // Remaining children are the inner items (to be concatenated)
    let inner_items = &expr_children[1..];

    // Check for ConcatExpr child
    let concat_child = children.iter().find(|c| c.kind() == SyntaxKind::ConcatExpr);

    let inner_width = if let Some(concat) = concat_child {
        let inner = infer_concat(concat, ctx);
        match &inner {
            ExprType::BitVec(bv) => bv.width,
            ExprType::Error(_) => return inner,
            ExprType::NonBit(_) => {
                return ExprType::Error(ExprTypeErrorKind::ConcatNonBitOperand);
            }
        }
    } else if inner_items.is_empty() {
        return ExprType::Error(ExprTypeErrorKind::UnsupportedExprKind);
    } else {
        // Sum widths of inner items
        let mut total: Option<u32> = Some(0);
        let mut all_known = true;
        for item in inner_items {
            let item_ty = infer_expr_type(item, ctx, None);
            match &item_ty {
                ExprType::BitVec(bv) => match bv.width {
                    BitWidth::Known(w) => {
                        if let Some(ref mut t) = total {
                            *t = t.saturating_add(w);
                        }
                    }
                    BitWidth::Unknown => all_known = false,
                },
                ExprType::NonBit(_) => {
                    return ExprType::Error(ExprTypeErrorKind::ConcatNonBitOperand);
                }
                ExprType::Error(_) => return item_ty,
            }
        }
        if all_known {
            BitWidth::Known(total.unwrap_or(0))
        } else {
            BitWidth::Unknown
        }
    };

    let result_width = match (replic_count, inner_width) {
        (Some(n), BitWidth::Known(w)) => BitWidth::Known(n.saturating_mul(w)),
        _ => BitWidth::Unknown,
    };

    ExprType::BitVec(BitVecType {
        width: result_width,
        signed: Signedness::Unsigned,
    })
}

fn infer_index(node: &SyntaxNode, ctx: &dyn InferCtx) -> ExprType {
    let children: Vec<SyntaxNode> = node
        .children()
        .filter(|c| is_expression_kind(c.kind()))
        .collect();
    if children.is_empty() {
        return ExprType::Error(ExprTypeErrorKind::UnsupportedExprKind);
    }

    let base_node = &children[0];

    // Try to get the full symbol type (with unpacked dims) for the base
    let base_sym_ty = if matches!(
        base_node.kind(),
        SyntaxKind::NameRef | SyntaxKind::QualifiedName
    ) {
        ctx.symbol_type_of_name(base_node)
    } else {
        None
    };

    // Check for unpacked dim peeling first
    if let Some(ref st) = base_sym_ty {
        let ty = match st {
            SymbolType::Value(ty) | SymbolType::TypeAlias(ty) => Some(ty),
            SymbolType::Net(net) => Some(&net.data),
            SymbolType::Error(_) => None,
        };
        if let Some(peeled) = ty.and_then(|t| t.peel_unpacked_dim()) {
            return ExprType::from_ty(&peeled);
        }
    }

    // No unpacked dims to peel -- check if base is bitvec for packed bit-select
    let base_ty = infer_expr_type(base_node, ctx, None);
    match &base_ty {
        ExprType::BitVec(_) => ExprType::one_bit(),
        ExprType::Error(_) => base_ty,
        ExprType::NonBit(_) => ExprType::Error(ExprTypeErrorKind::IndexNonIndexable),
    }
}

fn infer_call(node: &SyntaxNode, _ctx: &dyn InferCtx) -> ExprType {
    let Some(name_ref) = node.children().find(|c| c.kind() == SyntaxKind::NameRef) else {
        return ExprType::Error(ExprTypeErrorKind::UserCallUnsupported);
    };

    let sys_token = name_ref
        .children_with_tokens()
        .filter_map(|el| el.into_token())
        .find(|tok| tok.kind() == SyntaxKind::SystemIdent);

    if let Some(tok) = sys_token {
        match tok.text() {
            "$clog2" => ExprType::BitVec(BitVecType {
                width: BitWidth::Known(32),
                signed: Signedness::Signed,
            }),
            _ => ExprType::Error(ExprTypeErrorKind::UnsupportedSystemCall),
        }
    } else {
        ExprType::Error(ExprTypeErrorKind::UserCallUnsupported)
    }
}

#[derive(Debug, Clone, Copy)]
enum BinaryOp {
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

fn find_binary_op(node: &SyntaxNode) -> Option<BinaryOp> {
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

fn find_operator_token(node: &SyntaxNode) -> Option<SyntaxKind> {
    node.children_with_tokens()
        .filter_map(|el| el.into_token())
        .find(|tok| tok.kind() != SyntaxKind::Whitespace)
        .map(|tok| tok.kind())
}

fn merge_bitvec(a: &BitVecType, b: &BitVecType) -> BitVecType {
    BitVecType {
        width: match (a.width, b.width) {
            (BitWidth::Known(wa), BitWidth::Known(wb)) => BitWidth::Known(wa.max(wb)),
            _ => BitWidth::Unknown,
        },
        signed: if a.signed == Signedness::Signed && b.signed == Signedness::Signed {
            Signedness::Signed
        } else {
            Signedness::Unsigned
        },
    }
}

fn is_expression_kind(kind: SyntaxKind) -> bool {
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
            | SyntaxKind::NameRef
            | SyntaxKind::Literal
            | SyntaxKind::QualifiedName
    )
}
