use crate::enum_def::EnumId;
use crate::type_infer::{BitVecType, BitWidth, ExprType, ExprView};
use crate::types::Ty;

/// Fixed streaming width in bits for an expression type.
///
/// Returns `Some(bits)` when the type has a statically known fixed
/// streaming width; `None` for dynamic/unsupported types.
pub fn fixed_stream_width_bits_of_expr_type(
    et: &ExprType,
    enum_bits: &dyn Fn(&EnumId) -> Option<u32>,
) -> Option<u32> {
    match &et.view {
        ExprView::BitVec(BitVecType {
            width: BitWidth::Known(w),
            ..
        }) => Some(*w),
        ExprView::Plain => fixed_stream_width_bits_of_ty(&et.ty, enum_bits),
        _ => None,
    }
}

/// Recursive helper for computing fixed streaming width from a `Ty`.
fn fixed_stream_width_bits_of_ty(
    ty: &Ty,
    enum_bits: &dyn Fn(&EnumId) -> Option<u32>,
) -> Option<u32> {
    match ty {
        Ty::Integral(i) => i.try_packed_width(),
        Ty::Enum(id) => enum_bits(id),
        Ty::Array { elem, dim } => {
            let len = dim.fixed_len()?;
            let elem_bits = fixed_stream_width_bits_of_ty(elem, enum_bits)?;
            len.checked_mul(elem_bits)
        }
        _ => None,
    }
}
