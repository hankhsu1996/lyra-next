use lyra_ast::{
    ConcatExpr, Expr, ExprKind, ReplicExpr, StreamExpr, StreamRangeOp, StreamWithClause,
};

use super::expr_type::{
    BitVecType, BitWidth, ExprType, ExprTypeErrorKind, ExprView, InferCtx, Signedness,
    try_integral_view,
};
use super::infer_expr;
use crate::types::{ConstInt, Ty};

pub(super) fn infer_concat(concat: &ConcatExpr, ctx: &dyn InferCtx) -> ExprType {
    let mut total_width: Option<u32> = Some(0);
    let mut all_known = true;
    let mut any_four_state = false;

    for child in concat.operands() {
        let child_ty = infer_expr(&child, ctx, None);
        if let ExprView::Error(_) = &child_ty.view {
            return child_ty;
        }
        let Some(bv) = try_integral_view(&child_ty, ctx) else {
            return ExprType::error(ExprTypeErrorKind::ConcatNonBitOperand);
        };
        any_four_state = any_four_state || bv.four_state;
        match bv.width.self_determined() {
            Some(w) => {
                if let Some(ref mut tw) = total_width {
                    *tw = tw.saturating_add(w);
                }
            }
            None => {
                all_known = false;
            }
        }
    }

    let width = if all_known {
        BitWidth::Known(total_width.unwrap_or(0))
    } else {
        BitWidth::Unknown
    };

    ExprType::bitvec(BitVecType {
        width,
        signed: Signedness::Unsigned,
        four_state: any_four_state,
    })
}

/// Pack-width accumulator with explicit merge semantics.
enum StreamWidth {
    Known(u32),
    Dynamic,
    Error,
}

impl StreamWidth {
    fn add(self, rhs: StreamWidth) -> StreamWidth {
        match (self, rhs) {
            (StreamWidth::Error, _) | (_, StreamWidth::Error) => StreamWidth::Error,
            (StreamWidth::Dynamic, _) | (_, StreamWidth::Dynamic) => StreamWidth::Dynamic,
            (StreamWidth::Known(a), StreamWidth::Known(b)) => match a.checked_add(b) {
                Some(total) => StreamWidth::Known(total),
                None => StreamWidth::Error,
            },
        }
    }

    fn mul(count: u32, elem_width: u32) -> StreamWidth {
        match count.checked_mul(elem_width) {
            Some(total) => StreamWidth::Known(total),
            None => StreamWidth::Error,
        }
    }
}

/// Normalized view of an array element type for streaming width computation.
struct StreamElemView {
    bit_width: Option<u32>,
    four_state: bool,
}

fn stream_elem_view(elem_ty: &Ty, ctx: &dyn InferCtx) -> Option<StreamElemView> {
    match elem_ty {
        Ty::Integral(i) => Some(StreamElemView {
            bit_width: i.try_packed_width(),
            four_state: i.keyword.four_state(),
        }),
        Ty::Enum(id) => {
            let bv = ctx.enum_integral_view(id)?;
            Some(StreamElemView {
                bit_width: bv.width.self_determined(),
                four_state: bv.four_state,
            })
        }
        _ => None,
    }
}

pub(super) fn infer_stream(stream: &StreamExpr, ctx: &dyn InferCtx) -> ExprType {
    let Some(operands) = stream.stream_operands() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };

    let mut width = StreamWidth::Known(0);
    let mut any_four_state = false;

    for item in operands.items() {
        let Some(expr_node) = item.expr() else {
            width = width.add(StreamWidth::Error);
            continue;
        };

        let child_ty = infer_expr(&expr_node, ctx, None);
        if let ExprView::Error(_) = &child_ty.view {
            width = width.add(StreamWidth::Error);
            continue;
        }

        if let Some(with_clause) = item.with_clause() {
            // Operand has `with [range]` clause
            let operand_width =
                infer_stream_with_operand(&child_ty, &with_clause, &mut any_four_state, ctx);
            width = width.add(operand_width);
        } else {
            // No with-clause: existing path -- require integral
            let Some(bv) = try_integral_view(&child_ty, ctx) else {
                width = width.add(StreamWidth::Error);
                continue;
            };
            any_four_state = any_four_state || bv.four_state;
            match bv.width.self_determined() {
                Some(w) => width = width.add(StreamWidth::Known(w)),
                None => width = width.add(StreamWidth::Dynamic),
            }
        }
    }

    let result_width = match width {
        StreamWidth::Known(n) => BitWidth::Known(n),
        StreamWidth::Dynamic => BitWidth::Unknown,
        StreamWidth::Error => {
            return ExprType::error(ExprTypeErrorKind::InternalStreamOperandError);
        }
    };

    ExprType::bitvec(BitVecType {
        width: result_width,
        signed: Signedness::Unsigned,
        four_state: any_four_state,
    })
}

fn infer_stream_with_operand(
    operand_ty: &ExprType,
    with_clause: &StreamWithClause,
    any_four_state: &mut bool,
    ctx: &dyn InferCtx,
) -> StreamWidth {
    let Some(range) = with_clause.range() else {
        return StreamWidth::Error;
    };

    // Operand must be an array type
    let Ty::Array { elem, .. } = &operand_ty.ty else {
        return StreamWidth::Error;
    };

    let Some(ev) = stream_elem_view(elem, ctx) else {
        // Element type is not bit-streamable (e.g. real, packed struct)
        return StreamWidth::Error;
    };
    *any_four_state = *any_four_state || ev.four_state;

    let Some(op) = range.op() else {
        return StreamWidth::Error;
    };

    let elem_count: StreamWidth = match op {
        StreamRangeOp::Single => StreamWidth::Known(1),
        StreamRangeOp::IndexedPlus | StreamRangeOp::IndexedMinus => {
            let Some(width_node) = range.rhs() else {
                return StreamWidth::Error;
            };
            match ctx.const_eval(&width_node) {
                ConstInt::Known(w) if w > 0 => match u32::try_from(w) {
                    Ok(v) => StreamWidth::Known(v),
                    Err(_) => StreamWidth::Dynamic,
                },
                ConstInt::Known(_) | ConstInt::Unevaluated(_) | ConstInt::Error(_) => {
                    StreamWidth::Dynamic
                }
            }
        }
        StreamRangeOp::Fixed => {
            let (Some(lo_node), Some(hi_node)) = (range.lhs(), range.rhs()) else {
                return StreamWidth::Error;
            };
            match (ctx.const_eval(&lo_node), ctx.const_eval(&hi_node)) {
                (ConstInt::Known(lo), ConstInt::Known(hi)) => {
                    let diff = (i128::from(hi) - i128::from(lo)).unsigned_abs() + 1;
                    match u32::try_from(diff) {
                        Ok(v) => StreamWidth::Known(v),
                        Err(_) => StreamWidth::Error,
                    }
                }
                _ => StreamWidth::Dynamic,
            }
        }
    };

    match (elem_count, ev.bit_width) {
        (StreamWidth::Known(count), Some(bw)) => StreamWidth::mul(count, bw),
        (StreamWidth::Error, _) => StreamWidth::Error,
        _ => StreamWidth::Dynamic,
    }
}

pub(super) fn infer_replic(replic: &ReplicExpr, ctx: &dyn InferCtx) -> ExprType {
    let Some(count_expr) = replic.count() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let count = ctx.const_eval(&count_expr);

    let replic_count = match count {
        ConstInt::Known(n) => {
            if n <= 0 {
                return ExprType::error(ExprTypeErrorKind::InvalidReplicationCount);
            }
            u32::try_from(n).ok()
        }
        ConstInt::Error(e) => {
            return ExprType::error(ExprTypeErrorKind::ReplicationConstEvalFailed(e));
        }
        ConstInt::Unevaluated(_) => None,
    };

    // Body expressions after count (typically a single ConcatExpr)
    let body: Vec<Expr> = replic.body_exprs().collect();

    let body_concat = if body.len() == 1 {
        body[0].classify().and_then(|ek| match ek {
            ExprKind::ConcatExpr(c) => Some(c),
            _ => None,
        })
    } else {
        None
    };
    let (inner_width, inner_four_state) = if let Some(ref c) = body_concat {
        let inner = infer_concat(c, ctx);
        if let ExprView::Error(_) = &inner.view {
            return inner;
        }
        let Some(bv) = try_integral_view(&inner, ctx) else {
            return ExprType::error(ExprTypeErrorKind::ConcatNonBitOperand);
        };
        (bv.width, bv.four_state)
    } else if body.is_empty() {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    } else {
        // Sum widths of inner items
        let mut total: Option<u32> = Some(0);
        let mut all_known = true;
        let mut any_four_state = false;
        for item in &body {
            let item_ty = infer_expr(item, ctx, None);
            if let ExprView::Error(_) = &item_ty.view {
                return item_ty;
            }
            let Some(bv) = try_integral_view(&item_ty, ctx) else {
                return ExprType::error(ExprTypeErrorKind::ConcatNonBitOperand);
            };
            any_four_state = any_four_state || bv.four_state;
            match bv.width.self_determined() {
                Some(w) => {
                    if let Some(ref mut t) = total {
                        *t = t.saturating_add(w);
                    }
                }
                None => all_known = false,
            }
        }
        let width = if all_known {
            BitWidth::Known(total.unwrap_or(0))
        } else {
            BitWidth::Unknown
        };
        (width, any_four_state)
    };

    let result_width = match (replic_count, inner_width.self_determined()) {
        (Some(n), Some(w)) => BitWidth::Known(n.saturating_mul(w)),
        _ => BitWidth::Unknown,
    };

    ExprType::bitvec(BitVecType {
        width: result_width,
        signed: Signedness::Unsigned,
        four_state: inner_four_state,
    })
}
