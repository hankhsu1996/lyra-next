// Foreach iteration dimension model (LRM 12.7.3).
//
// Centralizes "iterable dimension" semantics: how many dimensions a
// foreach loop can iterate, and what the index type of each slot is.

use std::sync::Arc;

use super::{AssocIndex, PackedDims, Ty, UnpackedDim, collect_array_dims};

/// The index type of a single foreach dimension slot.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ForeachDimIndexType {
    /// Standard integer index (fixed, dynamic, queue, wildcard assoc, packed).
    Int,
    /// Typed associative index -- the key type.
    Typed(Arc<Ty>),
}

/// Iterable dimensions for a foreach loop.
///
/// Constructed from the iterated expression's type via `from_iterated_type`.
/// Dimensions are ordered: unpacked (outermost first), then packed.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForeachDims {
    slots: Box<[ForeachDimIndexType]>,
}

impl ForeachDims {
    /// Compute the iterable dimensions from the type of the array expression.
    ///
    /// Rules (LRM 12.7.3):
    /// - Unpacked dims come first (outermost to innermost), then packed dims.
    /// - `string` behaves as 1 iterable dimension with index type `int`.
    /// - Typed associative dims use their key type; all others use `int`.
    /// - Non-array/non-string with no packed dims yields 0 dims.
    pub fn from_iterated_type(ty: &Ty) -> Self {
        let mut slots = Vec::new();

        match ty {
            Ty::String => {
                slots.push(ForeachDimIndexType::Int);
                return Self {
                    slots: slots.into_boxed_slice(),
                };
            }
            Ty::Array { .. } => {
                let (base, unpacked) = collect_array_dims(ty);
                for dim in &unpacked {
                    slots.push(unpacked_dim_index_type(dim));
                }
                if let Ty::Integral(integ) = base {
                    append_packed_dims(&integ.packed, &mut slots);
                }
            }
            Ty::Integral(integ) => {
                append_packed_dims(&integ.packed, &mut slots);
            }
            _ => {}
        }

        Self {
            slots: slots.into_boxed_slice(),
        }
    }

    /// Number of iterable dimension slots.
    pub fn len(&self) -> usize {
        self.slots.len()
    }

    /// Whether there are no iterable dimensions.
    pub fn is_empty(&self) -> bool {
        self.slots.is_empty()
    }

    /// Index type for a given slot (0-based).
    pub fn slot_index_type(&self, slot: usize) -> Option<&ForeachDimIndexType> {
        self.slots.get(slot)
    }
}

fn unpacked_dim_index_type(dim: &UnpackedDim) -> ForeachDimIndexType {
    match dim {
        UnpackedDim::Assoc(AssocIndex::Typed(inner_ty)) => {
            ForeachDimIndexType::Typed(Arc::new(inner_ty.as_ref().clone()))
        }
        _ => ForeachDimIndexType::Int,
    }
}

fn append_packed_dims(dims: &PackedDims, slots: &mut Vec<ForeachDimIndexType>) {
    for _ in dims {
        slots.push(ForeachDimIndexType::Int);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::*;

    #[test]
    fn string_is_one_dim() {
        let dims = ForeachDims::from_iterated_type(&Ty::String);
        assert_eq!(dims.len(), 1);
        assert_eq!(dims.slot_index_type(0), Some(&ForeachDimIndexType::Int));
    }

    #[test]
    fn scalar_int_no_dims() {
        let dims = ForeachDims::from_iterated_type(&Ty::int());
        assert_eq!(dims.len(), 0);
    }

    #[test]
    fn packed_vector_has_dims() {
        // logic [7:0] -> 1 packed dim
        let ty = Ty::Integral(Integral {
            keyword: IntegralKw::Logic,
            signed: false,
            packed: PackedDims::from(vec![PackedDim {
                msb: ConstInt::Known(7),
                lsb: ConstInt::Known(0),
            }]),
        });
        let dims = ForeachDims::from_iterated_type(&ty);
        assert_eq!(dims.len(), 1);
        assert_eq!(dims.slot_index_type(0), Some(&ForeachDimIndexType::Int));
    }

    #[test]
    fn unpacked_array() {
        // int arr [4] -> 1 unpacked dim
        let ty = Ty::Array {
            elem: Box::new(Ty::int()),
            dim: UnpackedDim::Size(ConstInt::Known(4)),
        };
        let dims = ForeachDims::from_iterated_type(&ty);
        assert_eq!(dims.len(), 1);
    }

    #[test]
    fn mixed_unpacked_and_packed() {
        // logic [7:0] arr [3] -> 1 unpacked + 1 packed = 2
        let inner = Ty::Integral(Integral {
            keyword: IntegralKw::Logic,
            signed: false,
            packed: PackedDims::from(vec![PackedDim {
                msb: ConstInt::Known(7),
                lsb: ConstInt::Known(0),
            }]),
        });
        let ty = Ty::Array {
            elem: Box::new(inner),
            dim: UnpackedDim::Size(ConstInt::Known(3)),
        };
        let dims = ForeachDims::from_iterated_type(&ty);
        assert_eq!(dims.len(), 2);
    }

    #[test]
    fn assoc_typed_index() {
        // int aa [string] -> 1 dim with Typed(string)
        let ty = Ty::Array {
            elem: Box::new(Ty::int()),
            dim: UnpackedDim::Assoc(AssocIndex::Typed(Box::new(Ty::String))),
        };
        let dims = ForeachDims::from_iterated_type(&ty);
        assert_eq!(dims.len(), 1);
        assert_eq!(
            dims.slot_index_type(0),
            Some(&ForeachDimIndexType::Typed(Arc::new(Ty::String)))
        );
    }

    #[test]
    fn assoc_wildcard_index() {
        // int aa [*] -> 1 dim with Int
        let ty = Ty::Array {
            elem: Box::new(Ty::int()),
            dim: UnpackedDim::Assoc(AssocIndex::Wildcard),
        };
        let dims = ForeachDims::from_iterated_type(&ty);
        assert_eq!(dims.len(), 1);
        assert_eq!(dims.slot_index_type(0), Some(&ForeachDimIndexType::Int));
    }

    #[test]
    fn void_no_dims() {
        let dims = ForeachDims::from_iterated_type(&Ty::Void);
        assert_eq!(dims.len(), 0);
    }

    #[test]
    fn error_no_dims() {
        let dims = ForeachDims::from_iterated_type(&Ty::Error);
        assert_eq!(dims.len(), 0);
    }
}
