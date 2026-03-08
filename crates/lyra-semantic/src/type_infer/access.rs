use super::expr_type::{ExprType, ExprTypeErrorKind, ExprView, InferCtx, try_integral_view};
use super::infer_expr;
use crate::member::{MemberKind, MemberLookupError};
use crate::site;
use crate::types::{AssocIndex, Ty, UnpackedDim};
use lyra_ast::{FieldExpr, IndexExpr};

/// Result of checking an index expression against a typed associative array key.
enum AssocKeyCheck {
    Compatible,
    Incompatible { expected: Ty, actual: Ty },
    Deferred,
}

fn check_typed_assoc_key_compat(declared_key_ty: &Ty, actual_key_ty: &Ty) -> AssocKeyCheck {
    match declared_key_ty {
        Ty::String => {
            if matches!(actual_key_ty, Ty::String) {
                AssocKeyCheck::Compatible
            } else {
                AssocKeyCheck::Incompatible {
                    expected: declared_key_ty.clone(),
                    actual: actual_key_ty.clone(),
                }
            }
        }
        Ty::Integral(_) => {
            if matches!(actual_key_ty, Ty::Integral(_) | Ty::Enum(_)) {
                AssocKeyCheck::Compatible
            } else {
                AssocKeyCheck::Incompatible {
                    expected: declared_key_ty.clone(),
                    actual: actual_key_ty.clone(),
                }
            }
        }
        Ty::Enum(declared_id) => match actual_key_ty {
            Ty::Integral(_) => AssocKeyCheck::Compatible,
            Ty::Enum(actual_id) if actual_id == declared_id => AssocKeyCheck::Compatible,
            _ => AssocKeyCheck::Incompatible {
                expected: declared_key_ty.clone(),
                actual: actual_key_ty.clone(),
            },
        },
        _ => AssocKeyCheck::Deferred,
    }
}

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

    // Associative array key validation
    if let Ty::Array {
        dim: UnpackedDim::Assoc(assoc_index),
        ..
    } = &base.ty
    {
        match assoc_index {
            // LRM 7.8.1: wildcard associative array requires integral index expression.
            AssocIndex::Wildcard => {
                if try_integral_view(&idx, ctx).is_none() {
                    let index_site = site::opt_site_of(ctx.ast_id_map(), &index_node)
                        .or_else(|| site::opt_site_of(ctx.ast_id_map(), idx_expr))
                        .unwrap_or_else(|| lyra_ast::ErasedAstId::placeholder(ctx.file_id()));
                    return ExprType::error(ExprTypeErrorKind::IndexKeyNotIntegral { index_site });
                }
            }
            // Typed associative array key compatibility check.
            AssocIndex::Typed(key_ty) => {
                if let AssocKeyCheck::Incompatible { expected, actual } =
                    check_typed_assoc_key_compat(key_ty, &idx.ty)
                {
                    let index_site = site::opt_site_of(ctx.ast_id_map(), &index_node)
                        .or_else(|| site::opt_site_of(ctx.ast_id_map(), idx_expr))
                        .unwrap_or_else(|| lyra_ast::ErasedAstId::placeholder(ctx.file_id()));
                    return ExprType::error(ExprTypeErrorKind::AssocIndexKeyMismatch {
                        index_site,
                        expected: Box::new(expected),
                        actual: Box::new(actual),
                    });
                }
            }
        }
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
    let Some((_kind, text)) = field_expr.member_lookup_name() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };

    let lhs_type = infer_expr(&lhs, ctx, None);
    if let ExprView::Error(_) = &lhs_type.view {
        return lhs_type;
    }

    match ctx.member_lookup(&lhs_type.ty, &text) {
        Ok(info) => match info.kind {
            MemberKind::BuiltinMethod(_) | MemberKind::InterfaceCallable { .. } => {
                ExprType::error(ExprTypeErrorKind::MethodRequiresCall)
            }
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
