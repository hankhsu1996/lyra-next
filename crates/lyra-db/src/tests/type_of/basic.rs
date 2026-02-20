use super::*;

#[test]
fn type_of_logic_simple() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; logic x; endmodule");
    let unit = single_file_unit(&db, file);
    assert_eq!(
        get_type(&db, file, unit, "x"),
        SymbolType::Value(Ty::simple_logic())
    );
}

#[test]
fn type_of_logic_packed() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; logic [7:0] x; endmodule");
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "x");
    match ty {
        SymbolType::Value(Ty::Integral(ref i)) => {
            assert_eq!(i.keyword, IntegralKw::Logic);
            assert!(!i.signed);
            assert_eq!(i.packed.len(), 1);
            assert_eq!(i.packed[0].msb, ConstInt::Known(7));
            assert_eq!(i.packed[0].lsb, ConstInt::Known(0));
        }
        other => panic!("expected Value(Integral), got {other:?}"),
    }
}

#[test]
fn type_of_int() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; int x; endmodule");
    let unit = single_file_unit(&db, file);
    assert_eq!(get_type(&db, file, unit, "x"), SymbolType::Value(Ty::int()));
}

#[test]
fn type_of_param_int() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; parameter int W = 8; endmodule");
    let unit = single_file_unit(&db, file);
    assert_eq!(get_type(&db, file, unit, "W"), SymbolType::Value(Ty::int()));
}

#[test]
fn type_of_param_no_typespec() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; parameter W = 8; endmodule");
    let unit = single_file_unit(&db, file);
    assert_eq!(get_type(&db, file, unit, "W"), SymbolType::Value(Ty::int()));
}

#[test]
fn type_of_wire() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; wire [3:0] w; endmodule");
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "w");
    match ty {
        SymbolType::Net(ref net) => {
            assert_eq!(net.kind, NetKind::Wire);
            match &net.data {
                Ty::Integral(i) => {
                    assert_eq!(i.keyword, IntegralKw::Logic);
                    assert_eq!(i.packed.len(), 1);
                    assert_eq!(i.packed[0].msb, ConstInt::Known(3));
                    assert_eq!(i.packed[0].lsb, ConstInt::Known(0));
                }
                other => panic!("expected Integral, got {other:?}"),
            }
        }
        other => panic!("expected Net, got {other:?}"),
    }
}

#[test]
fn type_of_wire_signed() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; wire signed [7:0] w; endmodule");
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "w");
    match ty {
        SymbolType::Net(ref net) => {
            assert_eq!(net.kind, NetKind::Wire);
            match &net.data {
                Ty::Integral(i) => {
                    assert!(i.signed);
                    assert_eq!(i.packed.len(), 1);
                    assert_eq!(i.packed[0].msb, ConstInt::Known(7));
                    assert_eq!(i.packed[0].lsb, ConstInt::Known(0));
                }
                other => panic!("expected Integral, got {other:?}"),
            }
        }
        other => panic!("expected Net, got {other:?}"),
    }
}

#[test]
fn type_of_port() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m(input logic [7:0] a); endmodule");
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "a");
    match ty {
        SymbolType::Value(Ty::Integral(ref i)) => {
            assert_eq!(i.keyword, IntegralKw::Logic);
            assert_eq!(i.packed.len(), 1);
            assert_eq!(i.packed[0].msb, ConstInt::Known(7));
            assert_eq!(i.packed[0].lsb, ConstInt::Known(0));
        }
        other => panic!("expected Value(Integral), got {other:?}"),
    }
}

#[test]
fn type_of_port_no_typespec() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m(input a); endmodule");
    let unit = single_file_unit(&db, file);
    assert_eq!(
        get_type(&db, file, unit, "a"),
        SymbolType::Value(Ty::simple_logic())
    );
}

#[test]
fn type_of_with_param_dim() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; parameter int W = 8; logic [W-1:0] x; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "x");
    match ty {
        SymbolType::Value(Ty::Integral(ref i)) => {
            assert_eq!(i.packed.len(), 1);
            assert_eq!(i.packed[0].msb, ConstInt::Known(7));
            assert_eq!(i.packed[0].lsb, ConstInt::Known(0));
        }
        other => panic!("expected Value(Integral), got {other:?}"),
    }
}

#[test]
fn type_of_cross_file_dim() {
    let db = LyraDatabase::default();
    let pkg = new_file(&db, 0, "package pkg; parameter int W = 16; endpackage");
    let mod_file = new_file(
        &db,
        1,
        "module m; import pkg::*; logic [pkg::W-1:0] x; endmodule",
    );
    let unit = new_compilation_unit(&db, vec![pkg, mod_file]);
    let ty = get_type(&db, mod_file, unit, "x");
    match ty {
        SymbolType::Value(Ty::Integral(ref i)) => {
            assert_eq!(i.packed.len(), 1);
            assert_eq!(i.packed[0].msb, ConstInt::Known(15));
            assert_eq!(i.packed[0].lsb, ConstInt::Known(0));
        }
        other => panic!("expected Value(Integral), got {other:?}"),
    }
}

#[test]
fn type_of_param_chain_dim() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; parameter int A = 8; parameter int B = A - 1; logic [B:0] x; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "x");
    match ty {
        SymbolType::Value(Ty::Integral(ref i)) => {
            assert_eq!(i.packed.len(), 1);
            assert_eq!(i.packed[0].msb, ConstInt::Known(7));
            assert_eq!(i.packed[0].lsb, ConstInt::Known(0));
        }
        other => panic!("expected Value(Integral), got {other:?}"),
    }
}

#[test]
fn type_of_error_dim() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; parameter int A = 0; logic [1/A:0] x; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "x");
    match ty {
        SymbolType::Value(Ty::Integral(ref i)) => {
            assert_eq!(i.packed.len(), 1);
            assert_eq!(
                i.packed[0].msb,
                ConstInt::Error(ConstEvalError::DivideByZero)
            );
            assert_eq!(i.packed[0].lsb, ConstInt::Known(0));
        }
        other => panic!("expected Value(Integral), got {other:?}"),
    }
}

#[test]
fn type_of_module_symbol() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; endmodule");
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "m");
    assert_eq!(
        ty,
        SymbolType::Error(SymbolTypeError::UnsupportedSymbolKind)
    );
}

#[test]
fn type_of_raw_has_unevaluated() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; parameter int W = 8; logic [W-1:0] x; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let raw = get_type_raw(&db, file, unit, "x");
    match raw {
        SymbolType::Value(Ty::Integral(ref i)) => {
            assert_eq!(i.packed.len(), 1);
            assert!(
                matches!(i.packed[0].msb, ConstInt::Unevaluated(_)),
                "expected Unevaluated msb in raw, got {:?}",
                i.packed[0].msb
            );
            assert!(
                matches!(i.packed[0].lsb, ConstInt::Unevaluated(_)),
                "expected Unevaluated lsb in raw, got {:?}",
                i.packed[0].lsb
            );
        }
        other => panic!("expected Value(Integral) with unevaluated dims, got {other:?}"),
    }
}

#[test]
fn type_of_unsupported_dim_expr() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; logic [{1'b0, 1'b1}:0] x; endmodule");
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "x");
    match ty {
        SymbolType::Value(Ty::Integral(ref i)) => {
            assert_eq!(i.packed.len(), 1);
            assert_eq!(
                i.packed[0].msb,
                ConstInt::Error(ConstEvalError::Unsupported),
                "concat in dim should be Unsupported"
            );
            assert_eq!(i.packed[0].lsb, ConstInt::Known(0));
        }
        other => panic!("expected Value(Integral) with error dim, got {other:?}"),
    }
}
