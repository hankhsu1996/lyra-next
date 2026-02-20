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
