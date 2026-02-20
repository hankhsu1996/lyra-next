use super::*;

#[test]
fn type_of_typedef_simple() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef logic [7:0] my_t; my_t x; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "x");
    match ty {
        SymbolType::Value(Ty::Integral(ref i)) => {
            assert_eq!(i.keyword, IntegralKw::Logic);
            assert_eq!(i.packed.len(), 1);
            assert_eq!(i.packed[0].msb, ConstInt::Known(7));
            assert_eq!(i.packed[0].lsb, ConstInt::Known(0));
        }
        other => panic!("expected Value(Integral) via typedef, got {other:?}"),
    }
}

#[test]
fn type_of_typedef_chain() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef logic [7:0] base_t; typedef base_t alias_t; alias_t x; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "x");
    match ty {
        SymbolType::Value(Ty::Integral(ref i)) => {
            assert_eq!(i.keyword, IntegralKw::Logic);
            assert_eq!(i.packed.len(), 1);
            assert_eq!(i.packed[0].msb, ConstInt::Known(7));
            assert_eq!(i.packed[0].lsb, ConstInt::Known(0));
        }
        other => panic!("expected Value(Integral) via typedef chain, got {other:?}"),
    }
}

#[test]
fn type_of_typedef_cycle() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef b_t a_t; typedef a_t b_t; a_t x; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "x");
    assert_eq!(ty, SymbolType::Error(SymbolTypeError::TypedefCycle));
}

#[test]
fn type_of_typedef_preserves_alias() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef logic [7:0] byte_t; byte_t x; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "byte_t");
    assert!(
        matches!(ty, SymbolType::TypeAlias(_)),
        "typedef symbol should return TypeAlias, got {ty:?}"
    );
    let var_ty = get_type(&db, file, unit, "x");
    assert!(
        matches!(var_ty, SymbolType::Value(_)),
        "variable should return Value, got {var_ty:?}"
    );
}

#[test]
fn type_of_port_with_typedef() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m(input byte_t a); typedef logic [7:0] byte_t; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "a");
    match ty {
        SymbolType::Value(Ty::Integral(ref i)) => {
            assert_eq!(i.packed.len(), 1, "should have packed dim from typedef");
            assert_eq!(i.packed[0].msb, ConstInt::Known(7));
            assert_eq!(i.packed[0].lsb, ConstInt::Known(0));
        }
        other => panic!("expected Value(Integral) for port with typedef, got {other:?}"),
    }
}

#[test]
fn type_of_typedef_with_unpacked_dims_merge() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef logic [7:0] T [2]; T a [3]; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "a");
    match ty {
        SymbolType::Value(Ty::Array { ref elem, ref dim }) => {
            assert_eq!(*dim, UnpackedDim::Size(ConstInt::Known(3)));
            match elem.as_ref() {
                Ty::Array {
                    elem: inner,
                    dim: inner_dim,
                } => {
                    assert_eq!(*inner_dim, UnpackedDim::Size(ConstInt::Known(2)));
                    match inner.as_ref() {
                        Ty::Integral(i) => {
                            assert_eq!(i.packed.len(), 1);
                            assert_eq!(i.packed[0].msb, ConstInt::Known(7));
                            assert_eq!(i.packed[0].lsb, ConstInt::Known(0));
                        }
                        other => panic!("expected Integral base, got {other:?}"),
                    }
                }
                other => panic!("expected inner Array, got {other:?}"),
            }
        }
        other => panic!("expected Value(Array(Array(Integral))), got {other:?}"),
    }
}

#[test]
fn type_of_typedef_string_with_dims() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef string str_t; str_t a [3]; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "a");
    match ty {
        SymbolType::Value(Ty::Array { ref elem, ref dim }) => {
            assert_eq!(**elem, Ty::String);
            assert_eq!(*dim, UnpackedDim::Size(ConstInt::Known(3)));
        }
        other => panic!("expected Value(Array(String, 3)), got {other:?}"),
    }
}

#[test]
fn type_of_typedef_string_no_dims() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; typedef string str_t; str_t a; endmodule");
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "a");
    assert_eq!(ty, SymbolType::Value(Ty::String));
}

#[test]
fn type_of_typedef_alias_vs_value_layers() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef logic [7:0] byte_t; byte_t x; endmodule",
    );
    let unit = single_file_unit(&db, file);

    let raw_td = get_type_raw(&db, file, unit, "byte_t");
    assert!(
        matches!(raw_td, SymbolType::TypeAlias(Ty::Integral(_))),
        "raw typedef should be TypeAlias(Integral), got {raw_td:?}"
    );

    let raw_var = get_type_raw(&db, file, unit, "x");
    assert!(
        matches!(raw_var, SymbolType::Value(Ty::Integral(_))),
        "raw variable should be Value(Integral), got {raw_var:?}"
    );

    let norm_td = get_type(&db, file, unit, "byte_t");
    assert!(
        matches!(norm_td, SymbolType::TypeAlias(Ty::Integral(_))),
        "normalized typedef should be TypeAlias(Integral), got {norm_td:?}"
    );

    let norm_var = get_type(&db, file, unit, "x");
    assert!(
        matches!(norm_var, SymbolType::Value(Ty::Integral(_))),
        "normalized variable should be Value(Integral), got {norm_var:?}"
    );
}

#[test]
fn type_of_typedef_no_dims_followed_by_decl() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef logic [7:0] byte_t; logic [3:0] x; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let td = get_type(&db, file, unit, "byte_t");
    match td {
        SymbolType::TypeAlias(Ty::Integral(ref i)) => {
            assert_eq!(i.packed.len(), 1);
            assert_eq!(i.packed[0].msb, ConstInt::Known(7));
        }
        other => panic!("expected TypeAlias(Integral), got {other:?}"),
    }
    let var = get_type(&db, file, unit, "x");
    match var {
        SymbolType::Value(Ty::Integral(ref i)) => {
            assert_eq!(i.packed.len(), 1);
            assert_eq!(i.packed[0].msb, ConstInt::Known(3));
            assert_eq!(i.packed[0].lsb, ConstInt::Known(0));
        }
        other => panic!("expected Value(Integral), got {other:?}"),
    }
}

#[test]
fn type_of_typedef_enum_with_dims_merge() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef enum { A, B } E; E x [3]; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let var = get_type(&db, file, unit, "x");
    match var {
        SymbolType::Value(Ty::Array { ref elem, ref dim }) => {
            assert!(
                matches!(**elem, Ty::Enum(_)),
                "inner type should be Enum, got {elem:?}"
            );
            assert_eq!(*dim, UnpackedDim::Size(ConstInt::Known(3)));
        }
        other => panic!("expected Value(Array(Enum, 3)), got {other:?}"),
    }
}

#[test]
fn type_of_typedef_enum_with_typedef_unpacked_dims() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; typedef enum { A, B } E [4]; endmodule");
    let unit = single_file_unit(&db, file);
    let td = get_type(&db, file, unit, "E");
    match td {
        SymbolType::TypeAlias(Ty::Array { ref elem, ref dim }) => {
            assert!(
                matches!(**elem, Ty::Enum(_)),
                "inner type should be Enum, got {elem:?}"
            );
            assert_eq!(*dim, UnpackedDim::Size(ConstInt::Known(4)));
        }
        other => panic!("expected TypeAlias(Array(Enum, 4)), got {other:?}"),
    }
}

#[test]
fn type_of_typedef_integral_unpacked() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef logic [7:0] T [4]; T x; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "x");
    match ty {
        SymbolType::Value(Ty::Array { ref elem, ref dim }) => {
            match elem.as_ref() {
                Ty::Integral(i) => {
                    assert_eq!(i.keyword, IntegralKw::Logic);
                    assert_eq!(i.packed.len(), 1);
                    assert_eq!(i.packed[0].msb, ConstInt::Known(7));
                    assert_eq!(i.packed[0].lsb, ConstInt::Known(0));
                }
                other => panic!("expected Integral base, got {other:?}"),
            }
            assert_eq!(*dim, UnpackedDim::Size(ConstInt::Known(4)));
        }
        other => panic!("expected Value(Array(Integral, 4)), got {other:?}"),
    }
}
