use crate::type_infer::{BitVecType, BitWidth, Signedness};
use lyra_ast::SyntaxBinaryOp;

/// Target context for integral expression coercion (LRM 11.6).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IntegralCtx {
    pub width: Option<u32>,
    pub signed: Signedness,
    pub four_state: bool,
}

/// LRM 11.6.1 operator context categories.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OpCategory {
    /// Both operands and result sized to max(operands, context).
    /// Arithmetic: +, -, *, /, %, ** and bitwise: &, |, ^, ~^
    ContextBoth,
    /// Result is lhs-width only, rhs is self-determined.
    /// Shifts: <<, >>, <<<, >>>
    ShiftLike,
    /// Operands sized to max of each other (not outer context). Result is 1 bit.
    /// Relational: <, <=, >, >= and equality: ==, !=, ===, !==, ==?, !=?
    Comparison,
    /// Operands self-determined. Result is 1 bit.
    /// Logical: &&, ||
    Logical,
}

/// Whether an operator's result can produce X (4-state) or is always boolean (2-state).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ResultState {
    /// Case equality (===, !==) always produces 0/1.
    TwoState,
    /// Relational/equality can produce X when operands contain X/Z.
    FourState,
    /// Logical &&, || can produce X when operand is X (LRM 11.4.7).
    LogicalXable,
    /// Arithmetic/bitwise: 4-state if any operand is 4-state.
    FromOperands,
}

/// Per-operator specification: category + result state.
#[derive(Debug, Clone, Copy)]
pub struct OpSpec {
    pub category: OpCategory,
    pub result_state: ResultState,
}

/// Look up the operator specification for a binary operator.
pub fn op_spec(op: SyntaxBinaryOp) -> OpSpec {
    match op {
        SyntaxBinaryOp::Add
        | SyntaxBinaryOp::Sub
        | SyntaxBinaryOp::Mul
        | SyntaxBinaryOp::Div
        | SyntaxBinaryOp::Mod
        | SyntaxBinaryOp::Power
        | SyntaxBinaryOp::BitAnd
        | SyntaxBinaryOp::BitOr
        | SyntaxBinaryOp::BitXor
        | SyntaxBinaryOp::BitXnor => OpSpec {
            category: OpCategory::ContextBoth,
            result_state: ResultState::FromOperands,
        },
        SyntaxBinaryOp::Shl | SyntaxBinaryOp::Shr | SyntaxBinaryOp::Ashl | SyntaxBinaryOp::Ashr => {
            OpSpec {
                category: OpCategory::ShiftLike,
                result_state: ResultState::FromOperands,
            }
        }
        SyntaxBinaryOp::Lt
        | SyntaxBinaryOp::LtEq
        | SyntaxBinaryOp::Gt
        | SyntaxBinaryOp::GtEq
        | SyntaxBinaryOp::Eq
        | SyntaxBinaryOp::Neq
        | SyntaxBinaryOp::WildEq
        | SyntaxBinaryOp::WildNeq => OpSpec {
            category: OpCategory::Comparison,
            result_state: ResultState::FourState,
        },
        SyntaxBinaryOp::CaseEq | SyntaxBinaryOp::CaseNeq => OpSpec {
            category: OpCategory::Comparison,
            result_state: ResultState::TwoState,
        },
        SyntaxBinaryOp::LogAnd | SyntaxBinaryOp::LogOr => OpSpec {
            category: OpCategory::Logical,
            result_state: ResultState::LogicalXable,
        },
    }
}

/// Central coercion: extend/truncate width, convert sign, propagate 4-state.
///
/// This is the single place that performs width coercion for integral types.
pub fn coerce_integral(self_ty: &BitVecType, ctx: &IntegralCtx) -> BitVecType {
    let width = match (self_ty.width, ctx.width) {
        (BitWidth::ContextDependent, None) => BitWidth::Known(1),
        (BitWidth::ContextDependent | BitWidth::Known(_), Some(w)) => BitWidth::Known(w),
        (bw, _) => bw,
    };
    BitVecType {
        width,
        signed: ctx.signed,
        four_state: self_ty.four_state || ctx.four_state,
    }
}

/// Compute operator's own result type from operands (before outer context).
pub fn operator_result_self(
    category: OpCategory,
    result_state: ResultState,
    lhs: &BitVecType,
    rhs: &BitVecType,
) -> BitVecType {
    let result_four_state = match result_state {
        ResultState::TwoState => false,
        ResultState::FourState | ResultState::LogicalXable => true,
        ResultState::FromOperands => lhs.four_state || rhs.four_state,
    };

    match category {
        OpCategory::ContextBoth => {
            let width = merge_width(lhs.width, rhs.width);
            let signed = merge_signedness(lhs.signed, rhs.signed);
            BitVecType {
                width,
                signed,
                four_state: result_four_state,
            }
        }
        OpCategory::ShiftLike => BitVecType {
            width: lhs.width,
            signed: lhs.signed,
            four_state: result_four_state,
        },
        OpCategory::Comparison | OpCategory::Logical => BitVecType {
            width: BitWidth::Known(1),
            signed: Signedness::Unsigned,
            four_state: result_four_state,
        },
    }
}

/// Combine operator's self result with outer context.
///
/// Only `ContextBoth` merges with outer; others hard-ignore it.
pub fn apply_outer_context(
    category: OpCategory,
    result_self: &BitVecType,
    outer: Option<&IntegralCtx>,
) -> IntegralCtx {
    match category {
        OpCategory::ContextBoth | OpCategory::ShiftLike => {
            let Some(outer) = outer else {
                return IntegralCtx {
                    width: result_self.width.self_determined(),
                    signed: result_self.signed,
                    four_state: result_self.four_state,
                };
            };
            let width = match (result_self.width.self_determined(), outer.width) {
                (Some(a), Some(b)) => Some(a.max(b)),
                (Some(a), None) => Some(a),
                (None, w) => w,
            };
            IntegralCtx {
                width,
                signed: merge_signedness(result_self.signed, outer.signed),
                four_state: result_self.four_state || outer.four_state,
            }
        }
        OpCategory::Comparison | OpCategory::Logical => IntegralCtx {
            width: Some(1),
            signed: Signedness::Unsigned,
            four_state: result_self.four_state,
        },
    }
}

/// Build the context for comparison operands (max of each other, no outer).
pub fn comparison_context(lhs: &BitVecType, rhs: &BitVecType) -> IntegralCtx {
    let width = match (lhs.width.self_determined(), rhs.width.self_determined()) {
        (Some(a), Some(b)) => Some(a.max(b)),
        _ => None,
    };
    IntegralCtx {
        width,
        signed: merge_signedness(lhs.signed, rhs.signed),
        four_state: lhs.four_state || rhs.four_state,
    }
}

/// Build an assignment context from the LHS type.
pub fn assignment_context(lhs: &BitVecType) -> IntegralCtx {
    IntegralCtx {
        width: lhs.width.self_determined(),
        signed: lhs.signed,
        four_state: lhs.four_state,
    }
}

fn merge_width(a: BitWidth, b: BitWidth) -> BitWidth {
    match (a.self_determined(), b.self_determined()) {
        (Some(wa), Some(wb)) => BitWidth::Known(wa.max(wb)),
        _ => BitWidth::Unknown,
    }
}

fn merge_signedness(a: Signedness, b: Signedness) -> Signedness {
    if a == Signedness::Signed && b == Signedness::Signed {
        Signedness::Signed
    } else {
        Signedness::Unsigned
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn bv(width: u32, signed: Signedness, four_state: bool) -> BitVecType {
        BitVecType {
            width: BitWidth::Known(width),
            signed,
            four_state,
        }
    }

    fn bv_u(width: u32) -> BitVecType {
        bv(width, Signedness::Unsigned, false)
    }

    fn bv_u4(width: u32) -> BitVecType {
        bv(width, Signedness::Unsigned, true)
    }

    #[test]
    fn op_spec_add_is_context_both() {
        let s = op_spec(SyntaxBinaryOp::Add);
        assert_eq!(s.category, OpCategory::ContextBoth);
        assert_eq!(s.result_state, ResultState::FromOperands);
    }

    #[test]
    fn op_spec_shift_is_shift_like() {
        let s = op_spec(SyntaxBinaryOp::Shl);
        assert_eq!(s.category, OpCategory::ShiftLike);
    }

    #[test]
    fn op_spec_eq_is_comparison_four_state() {
        let s = op_spec(SyntaxBinaryOp::Eq);
        assert_eq!(s.category, OpCategory::Comparison);
        assert_eq!(s.result_state, ResultState::FourState);
    }

    #[test]
    fn op_spec_case_eq_is_two_state() {
        let s = op_spec(SyntaxBinaryOp::CaseEq);
        assert_eq!(s.category, OpCategory::Comparison);
        assert_eq!(s.result_state, ResultState::TwoState);
    }

    #[test]
    fn op_spec_logical_and() {
        let s = op_spec(SyntaxBinaryOp::LogAnd);
        assert_eq!(s.category, OpCategory::Logical);
        assert_eq!(s.result_state, ResultState::LogicalXable);
    }

    #[test]
    fn coerce_context_dependent_expands() {
        let cd = BitVecType {
            width: BitWidth::ContextDependent,
            signed: Signedness::Unsigned,
            four_state: false,
        };
        let ctx = IntegralCtx {
            width: Some(16),
            signed: Signedness::Unsigned,
            four_state: false,
        };
        let result = coerce_integral(&cd, &ctx);
        assert_eq!(result.width, BitWidth::Known(16));
    }

    #[test]
    fn coerce_context_dependent_defaults_to_one() {
        let cd = BitVecType {
            width: BitWidth::ContextDependent,
            signed: Signedness::Unsigned,
            four_state: false,
        };
        let ctx = IntegralCtx {
            width: None,
            signed: Signedness::Unsigned,
            four_state: false,
        };
        let result = coerce_integral(&cd, &ctx);
        assert_eq!(result.width, BitWidth::Known(1));
    }

    #[test]
    fn operator_result_self_add() {
        let lhs = bv_u(8);
        let rhs = bv_u(4);
        let result = operator_result_self(
            OpCategory::ContextBoth,
            ResultState::FromOperands,
            &lhs,
            &rhs,
        );
        assert_eq!(result.width, BitWidth::Known(8));
        assert_eq!(result.signed, Signedness::Unsigned);
        assert!(!result.four_state);
    }

    #[test]
    fn operator_result_self_add_four_state() {
        let lhs = bv_u4(8);
        let rhs = bv_u(4);
        let result = operator_result_self(
            OpCategory::ContextBoth,
            ResultState::FromOperands,
            &lhs,
            &rhs,
        );
        assert!(result.four_state);
    }

    #[test]
    fn operator_result_comparison_is_one_bit() {
        let lhs = bv_u(8);
        let rhs = bv_u(8);
        let result =
            operator_result_self(OpCategory::Comparison, ResultState::FourState, &lhs, &rhs);
        assert_eq!(result.width, BitWidth::Known(1));
        assert!(result.four_state);
    }

    #[test]
    fn apply_outer_context_both_merges() {
        let result_self = bv_u(8);
        let outer = IntegralCtx {
            width: Some(16),
            signed: Signedness::Unsigned,
            four_state: false,
        };
        let ctx = apply_outer_context(OpCategory::ContextBoth, &result_self, Some(&outer));
        assert_eq!(ctx.width, Some(16));
    }

    #[test]
    fn apply_outer_comparison_ignores_outer() {
        let result_self = BitVecType {
            width: BitWidth::Known(1),
            signed: Signedness::Unsigned,
            four_state: true,
        };
        let outer = IntegralCtx {
            width: Some(16),
            signed: Signedness::Unsigned,
            four_state: false,
        };
        let ctx = apply_outer_context(OpCategory::Comparison, &result_self, Some(&outer));
        assert_eq!(ctx.width, Some(1));
    }

    #[test]
    fn assignment_context_from_lhs() {
        let lhs = bv_u4(8);
        let ctx = assignment_context(&lhs);
        assert_eq!(ctx.width, Some(8));
        assert_eq!(ctx.signed, Signedness::Unsigned);
        assert!(ctx.four_state);
    }

    #[test]
    fn comparison_context_max_width() {
        let lhs = bv_u(8);
        let rhs = bv_u(4);
        let ctx = comparison_context(&lhs, &rhs);
        assert_eq!(ctx.width, Some(8));
    }
}
