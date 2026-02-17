use lyra_semantic::symbols::GlobalSymbolId;
use lyra_semantic::types::{
    ConstEvalError, ConstInt, IntegralKw, NetKind, SymbolType, SymbolTypeError, Ty, UnpackedDim,
};

use super::*;

// Helper: find a symbol by name in a single-file unit, return its GlobalSymbolId.
fn find_symbol(db: &dyn salsa::Database, file: SourceFile, name: &str) -> GlobalSymbolId {
    let def = def_index_file(db, file);
    let (local_id, _sym) = def
        .symbols
        .iter()
        .find(|(_, s)| s.name == name)
        .unwrap_or_else(|| panic!("symbol '{name}' not found"));
    GlobalSymbolId {
        file: file.file_id(db),
        local: local_id,
    }
}

// Helper: get type_of_symbol for a named symbol in a single-file unit.
fn get_type(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
    name: &str,
) -> SymbolType {
    let gsym = find_symbol(db, file, name);
    let sym_ref = SymbolRef::new(db, unit, gsym);
    type_of_symbol(db, sym_ref)
}

// Helper: get type_of_symbol_raw for a named symbol in a single-file unit.
fn get_type_raw(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
    name: &str,
) -> SymbolType {
    let gsym = find_symbol(db, file, name);
    let sym_ref = SymbolRef::new(db, unit, gsym);
    type_of_symbol_raw(db, sym_ref)
}

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
    // Value parameter with no TypeSpec defaults to int (LRM 6.20.2)
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
    // M4 provisional: default to logic
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
fn type_of_unpacked() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; logic x [3:0]; endmodule");
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "x");
    match ty {
        SymbolType::Value(Ty::Integral(ref i)) => {
            assert_eq!(i.keyword, IntegralKw::Logic);
            assert!(i.packed.is_empty());
            assert_eq!(i.unpacked.len(), 1);
            assert_eq!(
                i.unpacked[0],
                UnpackedDim::Range {
                    msb: ConstInt::Known(3),
                    lsb: ConstInt::Known(0),
                }
            );
        }
        other => panic!("expected Value(Integral), got {other:?}"),
    }
}

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
            // Raw query should have Unevaluated dims, not Known
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
fn type_of_typedef_preserves_alias() {
    // Typedef symbols should return TypeAlias, not Value
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
    // The variable using the typedef should be Value
    let var_ty = get_type(&db, file, unit, "x");
    assert!(
        matches!(var_ty, SymbolType::Value(_)),
        "variable should return Value, got {var_ty:?}"
    );
}

#[test]
fn type_of_port_with_typedef() {
    // Port with user-defined type should resolve through typedef
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
