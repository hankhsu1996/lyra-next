use crate::enum_def::EnumId;
use crate::type_infer::{BitVecType, BitWidth, ExprType, ExprView};
use crate::types::{DataTyView, Ty};

/// Fixed streaming width in bits for an expression type.
///
/// Returns `Some(bits)` when the type has a statically known fixed
/// streaming width; `None` for dynamic/unsupported types.
/// Non-data types (interface, event, void, error) are rejected up front
/// via `DataTyView` projection.
pub fn fixed_stream_width_bits_of_expr_type(
    et: &ExprType,
    enum_bits: &dyn Fn(&EnumId) -> Option<u32>,
) -> Option<u32> {
    match &et.view {
        ExprView::BitVec(BitVecType {
            width: BitWidth::Known(w),
            ..
        }) => Some(*w),
        ExprView::Plain => {
            let dv = et.ty.as_data_view()?;
            fixed_stream_width_bits_of_data_ty(dv, enum_bits)
        }
        _ => None,
    }
}

/// Compute fixed streaming width for a validated data type.
///
/// Entry point requires `DataTyView` to enforce the data-type invariant.
/// Array recursion uses the private `&Ty` helper since the leaf is already
/// validated by `DataTyView` construction.
fn fixed_stream_width_bits_of_data_ty(
    view: DataTyView<'_>,
    enum_bits: &dyn Fn(&EnumId) -> Option<u32>,
) -> Option<u32> {
    fixed_stream_width_bits_of_ty(view.ty(), enum_bits)
}

/// Recursive helper operating on raw `Ty` for array descent.
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
