use lyra_semantic::types::{ConstInt, IntegralKw, PackedDim, Ty, UnpackedDim, collect_array_dims};

/// Shape of a single dimension for array query functions (LRM 20.7).
///
/// Represents the dimension info needed by $left, $right, $low, $high,
/// $size, and $increment. Packed and unpacked dimensions are unified
/// into a single enum so the query functions can treat them uniformly.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum DimShape {
    Packed { msb: i64, lsb: i64 },
    FixedRange { msb: i64, lsb: i64 },
    FixedSize { size: i64 },
    Dynamic,
    Queue { bound: Option<i64> },
    Assoc,
}

impl DimShape {
    pub(crate) fn left(&self) -> i64 {
        match self {
            Self::Packed { msb, .. } | Self::FixedRange { msb, .. } => *msb,
            Self::FixedSize { .. } | Self::Dynamic | Self::Queue { .. } | Self::Assoc => 0,
        }
    }

    pub(crate) fn right(&self) -> Option<i64> {
        match self {
            Self::Packed { lsb, .. } | Self::FixedRange { lsb, .. } => Some(*lsb),
            Self::FixedSize { size } => Some(*size - 1),
            Self::Dynamic | Self::Queue { .. } | Self::Assoc => None,
        }
    }

    pub(crate) fn increment(&self) -> i64 {
        match self {
            Self::Packed { msb, lsb } | Self::FixedRange { msb, lsb } => {
                if msb >= lsb {
                    1
                } else {
                    -1
                }
            }
            Self::FixedSize { .. } | Self::Dynamic | Self::Queue { .. } | Self::Assoc => -1,
        }
    }

    pub(crate) fn low(&self) -> Option<i64> {
        let right = self.right()?;
        Some(self.left().min(right))
    }

    pub(crate) fn high(&self) -> Option<i64> {
        let right = self.right()?;
        Some(self.left().max(right))
    }

    pub(crate) fn size(&self) -> Option<i64> {
        let low = self.low()?;
        let high = self.high()?;
        Some(high - low + 1)
    }
}

/// Select the Nth dimension (1-indexed, LRM numbering).
///
/// LRM 20.7: "The slowest varying dimension is dimension 1."
/// Unpacked dims outermost-first, then packed dims outermost-first.
/// Returns None for out-of-range or no dimensions.
pub(crate) fn select_dim(ty: &Ty, dim_1based: u32) -> Option<DimShape> {
    if dim_1based == 0 {
        return None;
    }
    let idx = (dim_1based - 1) as usize;

    let (unpacked_count, packed_shapes) = dim_parts(ty);

    if idx < unpacked_count {
        let (_, unpacked_dims) = collect_array_dims(ty);
        let udim = unpacked_dims[idx];
        Some(unpacked_dim_to_shape(udim))
    } else {
        let packed_idx = idx - unpacked_count;
        packed_shapes.and_then(|shapes| shapes.into_iter().nth(packed_idx))
    }
}

/// Count dimensions as (unpacked, packed).
///
/// For predefined-width integrals with no explicit packed dims: packed=1.
/// For string: (0, 1) -- LRM special case.
/// For error types: (0, 0).
pub(crate) fn dim_counts(ty: &Ty) -> (u32, u32) {
    let (base, unpacked_dims) = collect_array_dims(ty);
    let unpacked = unpacked_dims.len() as u32;
    let packed = packed_dim_count(base);
    (unpacked, packed)
}

/// Internal: count packed dimensions for the base (non-Array) type.
fn packed_dim_count(base: &Ty) -> u32 {
    match base {
        Ty::Integral(i) => {
            if i.packed.is_empty() {
                // All integral types have at least one packed dimension.
                // Scalar logic/reg/bit are 1-bit vectors; int/byte/etc.
                // have their keyword base width as an implicit packed dim.
                1
            } else {
                i.packed.len() as u32
            }
        }
        Ty::String | Ty::Enum(_) => 1,
        _ => 0,
    }
}

/// Internal: get the unpacked dim count and packed dim shapes.
fn dim_parts(ty: &Ty) -> (usize, Option<Vec<DimShape>>) {
    let (base, unpacked_dims) = collect_array_dims(ty);
    let unpacked_count = unpacked_dims.len();

    let packed_shapes = match base {
        Ty::Integral(i) => {
            if i.packed.is_empty() {
                let w = i.keyword.base_width();
                Some(vec![DimShape::Packed {
                    msb: i64::from(w) - 1,
                    lsb: 0,
                }])
            } else {
                Some(packed_dims_to_shapes(i.packed.as_slice(), i.keyword))
            }
        }
        Ty::String => Some(vec![DimShape::Packed { msb: 0, lsb: 0 }]),
        Ty::Enum(_) => Some(vec![DimShape::Packed { msb: 31, lsb: 0 }]),
        _ => None,
    };

    (unpacked_count, packed_shapes)
}

/// Convert packed dims slice + keyword to dim shapes.
///
/// When explicit packed dims exist, `PackedDim` entries represent the
/// declared ranges directly. The keyword `base_width` is the implicit
/// innermost dim only when no packed dims are given. With explicit
/// packed dims, each dim maps 1:1 to a shape, and the keyword's base
/// width is appended as an additional innermost packed dim for multi-bit
/// keywords (int, byte, etc.).
fn packed_dims_to_shapes(dims: &[PackedDim], kw: IntegralKw) -> Vec<DimShape> {
    let mut shapes: Vec<DimShape> = dims
        .iter()
        .filter_map(|d| match (&d.msb, &d.lsb) {
            (ConstInt::Known(m), ConstInt::Known(l)) => Some(DimShape::Packed { msb: *m, lsb: *l }),
            _ => None,
        })
        .collect();

    // For multi-bit keywords (int, byte, etc.) with explicit packed dims,
    // the keyword's base width acts as an additional innermost packed dim.
    if !dims.is_empty() && kw.base_width() > 1 {
        let w = i64::from(kw.base_width());
        shapes.push(DimShape::Packed { msb: w - 1, lsb: 0 });
    }

    shapes
}

fn unpacked_dim_to_shape(dim: &UnpackedDim) -> DimShape {
    match dim {
        UnpackedDim::Range { msb, lsb } => match (msb, lsb) {
            (ConstInt::Known(m), ConstInt::Known(l)) => DimShape::FixedRange { msb: *m, lsb: *l },
            _ => DimShape::Dynamic,
        },
        UnpackedDim::Size(c) => match c {
            ConstInt::Known(s) => DimShape::FixedSize { size: *s },
            _ => DimShape::Dynamic,
        },
        UnpackedDim::Unsized => DimShape::Dynamic,
        UnpackedDim::Queue { bound } => {
            let b = bound.as_ref().and_then(|c| match c {
                ConstInt::Known(v) => Some(*v),
                _ => None,
            });
            DimShape::Queue { bound: b }
        }
        UnpackedDim::Assoc(_) => DimShape::Assoc,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lyra_semantic::types::{AssocIndex, PackedDims};

    fn logic_vec(msb: i64, lsb: i64) -> Ty {
        Ty::logic(
            PackedDims::from(vec![PackedDim {
                msb: ConstInt::Known(msb),
                lsb: ConstInt::Known(lsb),
            }]),
            false,
        )
    }

    fn logic_2d(msb1: i64, lsb1: i64, msb2: i64, lsb2: i64) -> Ty {
        Ty::logic(
            PackedDims::from(vec![
                PackedDim {
                    msb: ConstInt::Known(msb1),
                    lsb: ConstInt::Known(lsb1),
                },
                PackedDim {
                    msb: ConstInt::Known(msb2),
                    lsb: ConstInt::Known(lsb2),
                },
            ]),
            false,
        )
    }

    fn with_unpacked_size(base: Ty, size: i64) -> Ty {
        Ty::Array {
            elem: Box::new(base),
            dim: UnpackedDim::Size(ConstInt::Known(size)),
        }
    }

    fn with_unpacked_range(base: Ty, msb: i64, lsb: i64) -> Ty {
        Ty::Array {
            elem: Box::new(base),
            dim: UnpackedDim::Range {
                msb: ConstInt::Known(msb),
                lsb: ConstInt::Known(lsb),
            },
        }
    }

    fn with_dynamic(base: Ty) -> Ty {
        Ty::Array {
            elem: Box::new(base),
            dim: UnpackedDim::Unsized,
        }
    }

    fn with_queue(base: Ty) -> Ty {
        Ty::Array {
            elem: Box::new(base),
            dim: UnpackedDim::Queue { bound: None },
        }
    }

    fn with_assoc(base: Ty) -> Ty {
        Ty::Array {
            elem: Box::new(base),
            dim: UnpackedDim::Assoc(AssocIndex::Wildcard),
        }
    }

    // select_dim tests

    #[test]
    fn dim_0_out_of_range() {
        let ty = logic_vec(7, 0);
        assert_eq!(select_dim(&ty, 0), None);
    }

    #[test]
    fn logic_vec_dim1() {
        let ty = logic_vec(7, 0);
        assert_eq!(
            select_dim(&ty, 1),
            Some(DimShape::Packed { msb: 7, lsb: 0 })
        );
    }

    #[test]
    fn logic_vec_dim2_out_of_range() {
        let ty = logic_vec(7, 0);
        assert_eq!(select_dim(&ty, 2), None);
    }

    #[test]
    fn logic_2d_dim1() {
        let ty = logic_2d(3, 0, 7, 0);
        assert_eq!(
            select_dim(&ty, 1),
            Some(DimShape::Packed { msb: 3, lsb: 0 })
        );
    }

    #[test]
    fn logic_2d_dim2() {
        let ty = logic_2d(3, 0, 7, 0);
        assert_eq!(
            select_dim(&ty, 2),
            Some(DimShape::Packed { msb: 7, lsb: 0 })
        );
    }

    #[test]
    fn int_scalar_dim1() {
        let ty = Ty::int();
        assert_eq!(
            select_dim(&ty, 1),
            Some(DimShape::Packed { msb: 31, lsb: 0 })
        );
    }

    #[test]
    fn byte_scalar_dim1() {
        let ty = Ty::byte();
        assert_eq!(
            select_dim(&ty, 1),
            Some(DimShape::Packed { msb: 7, lsb: 0 })
        );
    }

    #[test]
    fn int_with_unpacked_size() {
        // int x [10]
        let ty = with_unpacked_size(Ty::int(), 10);
        assert_eq!(select_dim(&ty, 1), Some(DimShape::FixedSize { size: 10 }));
        assert_eq!(
            select_dim(&ty, 2),
            Some(DimShape::Packed { msb: 31, lsb: 0 })
        );
    }

    #[test]
    fn int_with_unpacked_range() {
        // int x [3:0]
        let ty = with_unpacked_range(Ty::int(), 3, 0);
        assert_eq!(
            select_dim(&ty, 1),
            Some(DimShape::FixedRange { msb: 3, lsb: 0 })
        );
        assert_eq!(
            select_dim(&ty, 2),
            Some(DimShape::Packed { msb: 31, lsb: 0 })
        );
    }

    #[test]
    fn logic_vec_with_two_unpacked() {
        // logic [7:0] x [3:0][1:0]
        let inner = with_unpacked_range(logic_vec(7, 0), 1, 0);
        let ty = with_unpacked_range(inner, 3, 0);
        assert_eq!(
            select_dim(&ty, 1),
            Some(DimShape::FixedRange { msb: 3, lsb: 0 })
        );
        assert_eq!(
            select_dim(&ty, 2),
            Some(DimShape::FixedRange { msb: 1, lsb: 0 })
        );
        assert_eq!(
            select_dim(&ty, 3),
            Some(DimShape::Packed { msb: 7, lsb: 0 })
        );
    }

    #[test]
    fn int_with_two_unpacked() {
        // int x [10][3:0]
        let inner = with_unpacked_range(Ty::int(), 3, 0);
        let ty = with_unpacked_size(inner, 10);
        assert_eq!(select_dim(&ty, 1), Some(DimShape::FixedSize { size: 10 }));
        assert_eq!(
            select_dim(&ty, 2),
            Some(DimShape::FixedRange { msb: 3, lsb: 0 })
        );
        assert_eq!(
            select_dim(&ty, 3),
            Some(DimShape::Packed { msb: 31, lsb: 0 })
        );
    }

    #[test]
    fn logic_2d_with_two_unpacked() {
        // logic [1:0][7:0] x [10][3:0]
        let base = logic_2d(1, 0, 7, 0);
        let inner = with_unpacked_range(base, 3, 0);
        let ty = with_unpacked_size(inner, 10);
        assert_eq!(select_dim(&ty, 1), Some(DimShape::FixedSize { size: 10 }));
        assert_eq!(
            select_dim(&ty, 2),
            Some(DimShape::FixedRange { msb: 3, lsb: 0 })
        );
        assert_eq!(
            select_dim(&ty, 3),
            Some(DimShape::Packed { msb: 1, lsb: 0 })
        );
        assert_eq!(
            select_dim(&ty, 4),
            Some(DimShape::Packed { msb: 7, lsb: 0 })
        );
    }

    #[test]
    fn dynamic_array() {
        // int x []
        let ty = with_dynamic(Ty::int());
        assert_eq!(select_dim(&ty, 1), Some(DimShape::Dynamic));
    }

    #[test]
    fn queue_array() {
        // int x [$]
        let ty = with_queue(Ty::int());
        assert_eq!(select_dim(&ty, 1), Some(DimShape::Queue { bound: None }));
    }

    #[test]
    fn assoc_array() {
        // int x [*]
        let ty = with_assoc(Ty::int());
        assert_eq!(select_dim(&ty, 1), Some(DimShape::Assoc));
    }

    // LRM example: logic [3:0][2:1] n [1:5][2:8]
    // Dim 1 = unpacked [1:5], Dim 2 = unpacked [2:8],
    // Dim 3 = packed [3:0], Dim 4 = packed [2:1]
    #[test]
    fn lrm_example_dim_numbering() {
        let base = logic_2d(3, 0, 2, 1);
        let inner = with_unpacked_range(base, 2, 8);
        let ty = with_unpacked_range(inner, 1, 5);

        assert_eq!(
            select_dim(&ty, 1),
            Some(DimShape::FixedRange { msb: 1, lsb: 5 })
        );
        assert_eq!(
            select_dim(&ty, 2),
            Some(DimShape::FixedRange { msb: 2, lsb: 8 })
        );
        assert_eq!(
            select_dim(&ty, 3),
            Some(DimShape::Packed { msb: 3, lsb: 0 })
        );
        assert_eq!(
            select_dim(&ty, 4),
            Some(DimShape::Packed { msb: 2, lsb: 1 })
        );
        assert_eq!(select_dim(&ty, 5), None);
    }

    // dim_counts tests

    #[test]
    fn dim_counts_logic_vec_with_unpacked() {
        // logic [7:0] x [3:0]
        let ty = with_unpacked_range(logic_vec(7, 0), 3, 0);
        assert_eq!(dim_counts(&ty), (1, 1));
    }

    #[test]
    fn dim_counts_logic_2d_packed() {
        // logic [3:0][7:0]
        let ty = logic_2d(3, 0, 7, 0);
        assert_eq!(dim_counts(&ty), (0, 2));
    }

    #[test]
    fn dim_counts_int_two_unpacked() {
        // int x [3:0][1:0] -- int has implicit [31:0]
        let inner = with_unpacked_range(Ty::int(), 1, 0);
        let ty = with_unpacked_range(inner, 3, 0);
        assert_eq!(dim_counts(&ty), (2, 1));
    }

    #[test]
    fn dim_counts_int_scalar() {
        assert_eq!(dim_counts(&Ty::int()), (0, 1));
    }

    #[test]
    fn dim_counts_string() {
        assert_eq!(dim_counts(&Ty::String), (0, 1));
    }

    #[test]
    fn dim_counts_real() {
        assert_eq!(
            dim_counts(&Ty::Real(lyra_semantic::types::RealKw::Real)),
            (0, 0)
        );
    }

    #[test]
    fn dim_counts_error() {
        assert_eq!(dim_counts(&Ty::Error), (0, 0));
    }

    // DimShape method tests

    #[test]
    fn shape_packed_descending() {
        let s = DimShape::Packed { msb: 7, lsb: 0 };
        assert_eq!(s.left(), 7);
        assert_eq!(s.right(), Some(0));
        assert_eq!(s.increment(), 1);
        assert_eq!(s.low(), Some(0));
        assert_eq!(s.high(), Some(7));
        assert_eq!(s.size(), Some(8));
    }

    #[test]
    fn shape_packed_ascending() {
        let s = DimShape::Packed { msb: 0, lsb: 7 };
        assert_eq!(s.left(), 0);
        assert_eq!(s.right(), Some(7));
        assert_eq!(s.increment(), -1);
        assert_eq!(s.low(), Some(0));
        assert_eq!(s.high(), Some(7));
        assert_eq!(s.size(), Some(8));
    }

    #[test]
    fn shape_fixed_range_descending() {
        let s = DimShape::FixedRange { msb: 3, lsb: 0 };
        assert_eq!(s.left(), 3);
        assert_eq!(s.right(), Some(0));
        assert_eq!(s.increment(), 1);
        assert_eq!(s.low(), Some(0));
        assert_eq!(s.high(), Some(3));
        assert_eq!(s.size(), Some(4));
    }

    #[test]
    fn shape_fixed_range_ascending() {
        let s = DimShape::FixedRange { msb: 0, lsb: 3 };
        assert_eq!(s.left(), 0);
        assert_eq!(s.right(), Some(3));
        assert_eq!(s.increment(), -1);
        assert_eq!(s.low(), Some(0));
        assert_eq!(s.high(), Some(3));
        assert_eq!(s.size(), Some(4));
    }

    #[test]
    fn shape_fixed_size() {
        let s = DimShape::FixedSize { size: 10 };
        assert_eq!(s.left(), 0);
        assert_eq!(s.right(), Some(9));
        assert_eq!(s.increment(), -1);
        assert_eq!(s.low(), Some(0));
        assert_eq!(s.high(), Some(9));
        assert_eq!(s.size(), Some(10));
    }

    #[test]
    fn shape_dynamic() {
        let s = DimShape::Dynamic;
        assert_eq!(s.left(), 0);
        assert_eq!(s.right(), None);
        assert_eq!(s.increment(), -1);
        assert_eq!(s.low(), None);
        assert_eq!(s.high(), None);
        assert_eq!(s.size(), None);
    }

    #[test]
    fn shape_queue() {
        let s = DimShape::Queue { bound: None };
        assert_eq!(s.left(), 0);
        assert_eq!(s.right(), None);
        assert_eq!(s.increment(), -1);
        assert_eq!(s.low(), None);
        assert_eq!(s.high(), None);
        assert_eq!(s.size(), None);
    }

    #[test]
    fn shape_assoc() {
        let s = DimShape::Assoc;
        assert_eq!(s.left(), 0);
        assert_eq!(s.right(), None);
        assert_eq!(s.increment(), -1);
        assert_eq!(s.low(), None);
        assert_eq!(s.high(), None);
        assert_eq!(s.size(), None);
    }

    // Scalar logic (1-bit, no explicit dims) -- $dimensions returns 1
    #[test]
    fn dim_counts_scalar_logic() {
        let ty = Ty::simple_logic();
        assert_eq!(dim_counts(&ty), (0, 1));
    }

    #[test]
    fn scalar_logic_dim1() {
        let ty = Ty::simple_logic();
        assert_eq!(
            select_dim(&ty, 1),
            Some(DimShape::Packed { msb: 0, lsb: 0 })
        );
    }
}
