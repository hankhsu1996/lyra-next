use lyra_ast::{FieldExpr, IndexExpr};

use super::expr_type::{ExprType, ExprTypeErrorKind, ExprView, InferCtx};
use super::infer_expr;
use crate::member::{MemberKind, MemberLookupError};
use crate::types::Ty;

/// What kind of indexing operation `a[i]` performs on a given base type.
enum IndexKind {
    /// Base is `Ty::Array` -- peel outermost unpacked dimension.
    UnpackedElem,
    /// Base is integral with >= 2 packed dims -- peel outermost packed dim.
    PackedArrayDim,
    /// Base is integral with <= 1 packed dim -- bit-select (1-bit result).
    BitSelect,
}

fn classify_index(ty: &Ty) -> Result<IndexKind, ExprTypeErrorKind> {
    match ty {
        Ty::Array { .. } => Ok(IndexKind::UnpackedElem),
        Ty::Integral(i) if i.packed.len() >= 2 => Ok(IndexKind::PackedArrayDim),
        Ty::Integral(_) => Ok(IndexKind::BitSelect),
        _ => Err(ExprTypeErrorKind::IndexNonIndexable),
    }
}

pub(super) fn infer_index(idx_expr: &IndexExpr, ctx: &dyn InferCtx) -> ExprType {
    let Some(base_node) = idx_expr.base_expr() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let Some(index_node) = idx_expr.index_expr() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };

    let base = infer_expr(&base_node, ctx, None);
    if matches!(base.view, ExprView::Error(_)) {
        return base;
    }

    let idx = infer_expr(&index_node, ctx, None);
    if matches!(idx.view, ExprView::Error(_)) {
        return idx;
    }

    apply_index(&base)
}

/// Apply one index to a base type.
fn apply_index(base: &ExprType) -> ExprType {
    match classify_index(&base.ty) {
        Ok(IndexKind::UnpackedElem) => match base.ty.peel_unpacked_dim() {
            Some(elem) => ExprType::from_ty(&elem),
            None => ExprType::error(ExprTypeErrorKind::IndexNonIndexable),
        },
        Ok(IndexKind::PackedArrayDim) => match base.ty.peel_packed_array_dim() {
            Some(inner) => ExprType::from_ty(&inner),
            None => ExprType::error(ExprTypeErrorKind::IndexNonIndexable),
        },
        Ok(IndexKind::BitSelect) => match base.ty.bit_select_result() {
            Some(bit) => ExprType::from_ty(&bit),
            None => ExprType::error(ExprTypeErrorKind::IndexNonIndexable),
        },
        Err(e) => ExprType::error(e),
    }
}

pub(super) fn infer_field_access(field_expr: &FieldExpr, ctx: &dyn InferCtx) -> ExprType {
    let Some(lhs) = field_expr.base_expr() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let Some(field_tok) = field_expr.field_name() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };

    let lhs_type = infer_expr(&lhs, ctx, None);
    if let ExprView::Error(_) = &lhs_type.view {
        return lhs_type;
    }

    match ctx.member_lookup(&lhs_type.ty, field_tok.text()) {
        Ok(info) => match info.kind {
            MemberKind::BuiltinMethod(_) => ExprType::error(ExprTypeErrorKind::MethodRequiresCall),
            _ => ExprType::from_ty(&info.ty),
        },
        Err(MemberLookupError::NoMembersOnType) => {
            ExprType::error(ExprTypeErrorKind::NoMembersOnReceiver)
        }
        Err(MemberLookupError::UnknownMember) => ExprType::error(ExprTypeErrorKind::UnknownMember),
        Err(MemberLookupError::NotInModport) => {
            ExprType::error(ExprTypeErrorKind::MemberNotInModport)
        }
        Err(MemberLookupError::MethodNotValidOnReceiver(reason)) => {
            ExprType::error(ExprTypeErrorKind::MethodNotValidOnReceiver(reason))
        }
    }
}
