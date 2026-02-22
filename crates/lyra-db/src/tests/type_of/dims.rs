use super::*;

#[test]
fn type_of_unpacked() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; logic x [3:0]; endmodule");
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "x");
    match ty {
        SymbolType::Value(Ty::Array { ref elem, ref dim }) => {
            match elem.as_ref() {
                Ty::Integral(i) => {
                    assert_eq!(i.keyword, IntegralKw::Logic);
                    assert!(i.packed.is_empty());
                }
                other => panic!("expected Integral base, got {other:?}"),
            }
            assert_eq!(
                *dim,
                UnpackedDim::Range {
                    msb: ConstInt::Known(3),
                    lsb: ConstInt::Known(0),
                }
            );
        }
        other => panic!("expected Value(Array), got {other:?}"),
    }
}

#[test]
fn type_of_real_array() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; real x [4]; endmodule");
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "x");
    match ty {
        SymbolType::Value(Ty::Array { ref elem, ref dim }) => {
            assert!(
                matches!(**elem, Ty::Real(_)),
                "inner type should be Real, got {elem:?}"
            );
            assert_eq!(*dim, UnpackedDim::Size(ConstInt::Known(4)));
        }
        other => panic!("expected Value(Array(Real, 4)), got {other:?}"),
    }
}

#[test]
fn type_of_string_array() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; string s [2]; endmodule");
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "s");
    match ty {
        SymbolType::Value(Ty::Array { ref elem, ref dim }) => {
            assert_eq!(**elem, Ty::String);
            assert_eq!(*dim, UnpackedDim::Size(ConstInt::Known(2)));
        }
        other => panic!("expected Value(Array(String, 2)), got {other:?}"),
    }
}

#[test]
fn type_of_logic_multi_unpacked() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; logic [7:0] x [2][3]; endmodule");
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "x");
    match ty {
        SymbolType::Value(ref outer @ Ty::Array { .. }) => {
            assert_eq!(outer.pretty(), "logic [7:0] [2] [3]");
        }
        other => panic!("expected Value(Array(Array(Integral))), got {other:?}"),
    }
}

#[test]
fn type_of_unsized_dim() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; int a[]; endmodule");
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "a");
    match ty {
        SymbolType::Value(Ty::Array { ref dim, .. }) => {
            assert_eq!(*dim, UnpackedDim::Unsized);
        }
        other => panic!("expected Value(Array(Unsized)), got {other:?}"),
    }
    assert_eq!(ty.pretty(), "int []");
}

#[test]
fn type_of_queue_no_bound() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; int q[$]; endmodule");
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "q");
    match ty {
        SymbolType::Value(Ty::Array { ref dim, .. }) => {
            assert_eq!(*dim, UnpackedDim::Queue { bound: None });
        }
        other => panic!("expected Value(Array(Queue{{None}})), got {other:?}"),
    }
    assert_eq!(ty.pretty(), "int [$]");
}

#[test]
fn type_of_queue_with_bound() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; int q[$:8]; endmodule");
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "q");
    match ty {
        SymbolType::Value(Ty::Array { ref dim, .. }) => {
            assert_eq!(
                *dim,
                UnpackedDim::Queue {
                    bound: Some(ConstInt::Known(8))
                }
            );
        }
        other => panic!("expected Value(Array(Queue{{Some(8)}})), got {other:?}"),
    }
    assert_eq!(ty.pretty(), "int [$:8]");
}

#[test]
fn type_of_assoc_wildcard() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; int a[*]; endmodule");
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "a");
    match ty {
        SymbolType::Value(Ty::Array { ref dim, .. }) => {
            assert_eq!(*dim, UnpackedDim::Assoc(AssocIndex::Wildcard));
        }
        other => panic!("expected Value(Array(Assoc(Wildcard))), got {other:?}"),
    }
    assert_eq!(ty.pretty(), "int [*]");
}

#[test]
fn type_of_assoc_string() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; int a[string]; endmodule");
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "a");
    match ty {
        SymbolType::Value(Ty::Array { ref dim, .. }) => match dim {
            UnpackedDim::Assoc(AssocIndex::Typed(key_ty)) => {
                assert_eq!(**key_ty, Ty::String);
            }
            other => panic!("expected Assoc(Typed), got {other:?}"),
        },
        other => panic!("expected Value(Array(Assoc(Typed))), got {other:?}"),
    }
    assert_eq!(ty.pretty(), "int [string]");
}

#[test]
fn type_of_ident_dim_stays_as_size() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; int a[my_type]; endmodule");
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "a");
    match ty {
        SymbolType::Value(Ty::Array { ref dim, .. }) => {
            assert!(
                matches!(dim, UnpackedDim::Size(_)),
                "[ident] should fall through to Size, got {dim:?}"
            );
        }
        other => panic!("expected Value(Array(Size)), got {other:?}"),
    }
}

#[test]
fn existing_size_dim_unchanged() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; int a[8]; endmodule");
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "a");
    match ty {
        SymbolType::Value(Ty::Array { ref dim, .. }) => {
            assert_eq!(*dim, UnpackedDim::Size(ConstInt::Known(8)));
        }
        other => panic!("expected Value(Array(Size(8))), got {other:?}"),
    }
}

#[test]
fn existing_range_dim_unchanged() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; int a[7:0]; endmodule");
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "a");
    match ty {
        SymbolType::Value(Ty::Array { ref dim, .. }) => {
            assert_eq!(
                *dim,
                UnpackedDim::Range {
                    msb: ConstInt::Known(7),
                    lsb: ConstInt::Known(0),
                }
            );
        }
        other => panic!("expected Value(Array(Range)), got {other:?}"),
    }
}
