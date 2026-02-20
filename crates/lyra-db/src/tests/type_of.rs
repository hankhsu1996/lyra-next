use lyra_semantic::record::{Packing, RecordKind};
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

#[test]
fn type_of_typedef_with_unpacked_dims_merge() {
    // typedef logic [7:0] T [2]; T a [3];
    // Result: Array(Array(Integral(logic [7:0]), Size(2)), Size(3))
    // Outermost is [3] (use-site), inner is [2] (typedef)
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef logic [7:0] T [2]; T a [3]; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "a");
    // Outermost dim is use-site [3]
    match ty {
        SymbolType::Value(Ty::Array { ref elem, ref dim }) => {
            assert_eq!(*dim, UnpackedDim::Size(ConstInt::Known(3)));
            // Inner dim is typedef [2]
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
    // typedef string str_t; str_t a [3];
    // Should produce Array { elem: String, dim: Size(3) }
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
    // Typedef to string WITHOUT use-site dims should work (no dim merge needed)
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; typedef string str_t; str_t a; endmodule");
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "a");
    assert_eq!(ty, SymbolType::Value(Ty::String));
}

#[test]
fn type_of_unsupported_dim_expr() {
    // Concat expression in a dimension should propagate as ConstInt::Error(Unsupported)
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

#[test]
fn type_of_typedef_alias_vs_value_layers() {
    // Raw query on typedef symbol -> TypeAlias
    // Raw query on variable using typedef -> Value (after expansion)
    // Normalized query preserves the same classification
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef logic [7:0] byte_t; byte_t x; endmodule",
    );
    let unit = single_file_unit(&db, file);

    // Raw: typedef symbol itself
    let raw_td = get_type_raw(&db, file, unit, "byte_t");
    assert!(
        matches!(raw_td, SymbolType::TypeAlias(Ty::Integral(_))),
        "raw typedef should be TypeAlias(Integral), got {raw_td:?}"
    );

    // Raw: variable using typedef
    let raw_var = get_type_raw(&db, file, unit, "x");
    assert!(
        matches!(raw_var, SymbolType::Value(Ty::Integral(_))),
        "raw variable should be Value(Integral), got {raw_var:?}"
    );

    // Normalized: same classification preserved
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
    // Regression: typedef without unpacked dims followed by a variable decl.
    // The parser's optional unpacked dim loop must not consume the next decl.
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef logic [7:0] byte_t; logic [3:0] x; endmodule",
    );
    let unit = single_file_unit(&db, file);
    // typedef should have no unpacked dims (no Array wrapping)
    let td = get_type(&db, file, unit, "byte_t");
    match td {
        SymbolType::TypeAlias(Ty::Integral(ref i)) => {
            assert_eq!(i.packed.len(), 1);
            assert_eq!(i.packed[0].msb, ConstInt::Known(7));
        }
        other => panic!("expected TypeAlias(Integral), got {other:?}"),
    }
    // Following variable should parse and type correctly
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

// Enum/struct type tests

#[test]
fn type_of_typedef_enum() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef enum { A, B, C } abc_t; abc_t x; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let td = get_type(&db, file, unit, "abc_t");
    assert!(
        matches!(td, SymbolType::TypeAlias(Ty::Enum(_))),
        "typedef enum should be TypeAlias(Enum), got {td:?}"
    );
    let var = get_type(&db, file, unit, "x");
    assert!(
        matches!(var, SymbolType::Value(Ty::Enum(_))),
        "variable of typedef enum should be Value(Enum), got {var:?}"
    );
}

#[test]
fn type_of_typedef_struct_packed() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef struct packed { logic [7:0] data; } pkt_t; pkt_t y; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let td = get_type(&db, file, unit, "pkt_t");
    assert!(
        matches!(td, SymbolType::TypeAlias(Ty::Record(_))),
        "typedef struct should be TypeAlias(Record), got {td:?}"
    );
    let var = get_type(&db, file, unit, "y");
    assert!(
        matches!(var, SymbolType::Value(Ty::Record(_))),
        "variable of typedef struct should be Value(Record), got {var:?}"
    );
}

#[test]
fn type_of_inline_enum() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; enum { P, Q } pq; endmodule");
    let unit = single_file_unit(&db, file);
    let var = get_type(&db, file, unit, "pq");
    assert!(
        matches!(var, SymbolType::Value(Ty::Enum(_))),
        "inline enum variable should be Value(Enum), got {var:?}"
    );
}

#[test]
fn type_of_inline_struct() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; struct { int a; int b; } point; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let var = get_type(&db, file, unit, "point");
    assert!(
        matches!(var, SymbolType::Value(Ty::Record(_))),
        "inline struct variable should be Value(Record), got {var:?}"
    );
}

#[test]
fn type_of_enum_array() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef enum { A, B } ab_t; ab_t x [4]; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let var = get_type(&db, file, unit, "x");
    match var {
        SymbolType::Value(Ty::Array { ref dim, .. }) => {
            assert_eq!(*dim, UnpackedDim::Size(ConstInt::Known(4)));
        }
        other => panic!("expected Value(Array), got {other:?}"),
    }
}

#[test]
fn type_of_enum_array_2d_peel() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef enum { A, B } ab_t; ab_t x [2][3]; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let var = get_type(&db, file, unit, "x");
    match var {
        SymbolType::Value(ref ty @ Ty::Array { .. }) => {
            // Outermost dim is [2]
            let peeled = ty.peel_unpacked_dim();
            assert!(peeled.is_some(), "should peel outermost dim");
            match peeled {
                Some(Ty::Array { ref dim, .. }) => {
                    assert_eq!(*dim, UnpackedDim::Size(ConstInt::Known(3)));
                }
                other => panic!("after peel, expected Array with dim 3, got {other:?}"),
            }
        }
        other => panic!("expected Value(Array), got {other:?}"),
    }
}

#[test]
fn enum_def_variants_stable() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef enum { X, Y, Z } xyz_t; endmodule",
    );
    let def = def_index_file(&db, file);
    assert_eq!(def.enum_defs.len(), 1);
    let names: Vec<&str> = def.enum_defs[0]
        .variants
        .iter()
        .map(|v| v.name.as_str())
        .collect();
    assert_eq!(names, vec!["X", "Y", "Z"]);
}

#[test]
fn struct_def_fields_stable() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef struct packed { logic [7:0] data; logic valid; } pkt_t; endmodule",
    );
    let def = def_index_file(&db, file);
    assert_eq!(def.record_defs.len(), 1);
    let names: Vec<&str> = def.record_defs[0]
        .fields
        .iter()
        .map(|f| f.name.as_str())
        .collect();
    assert_eq!(names, vec!["data", "valid"]);
    assert_eq!(
        def.record_defs[0].packing,
        lyra_semantic::record::Packing::Packed
    );
    assert_eq!(
        def.record_defs[0].kind,
        lyra_semantic::record::RecordKind::Struct
    );
}

// Interface type tests

#[test]
fn interface_typed_port() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "interface my_bus; logic data; endinterface module m(my_bus b); endmodule",
    );
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "b");
    match ty {
        SymbolType::Value(Ty::Interface(_)) => {}
        other => panic!("expected Value(Interface), got {other:?}"),
    }
}

#[test]
fn interface_typed_variable() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "interface my_bus; logic data; endinterface module m; my_bus b; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "b");
    match ty {
        SymbolType::Value(Ty::Interface(_)) => {}
        other => panic!("expected Value(Interface), got {other:?}"),
    }
}

#[test]
fn interface_identity_resolves_to_interface_kind() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "interface unique_bus; endinterface module m; unique_bus v; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "v");
    let iface_def = match ty {
        SymbolType::Value(Ty::Interface(ref it)) => it.iface,
        other => panic!("expected Value(Interface), got {other:?}"),
    };
    let global = crate::semantic::global_def_index(&db, unit);
    let actual_kind = global.def_kind(iface_def.global_def());
    assert_eq!(
        actual_kind,
        Some(lyra_semantic::global_index::DefinitionKind::Interface),
        "InterfaceDefId should resolve to DefinitionKind::Interface"
    );
}

#[test]
fn enum_id_churn_contained() {
    // Two modules with enums. Adding an enum in module a should not affect module b's IDs.
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module a; typedef enum { X } e1; typedef enum { Y } e2; endmodule\n\
         module b; typedef enum { Z } e3; endmodule",
    );
    let def = def_index_file(&db, file);
    // Module a has 2 enums (ordinals 0, 1 under owner "a")
    // Module b has 1 enum (ordinal 0 under owner "b")
    assert_eq!(def.enum_defs.len(), 3);
    assert_eq!(def.enum_defs[0].owner.as_deref(), Some("a"));
    assert_eq!(def.enum_defs[0].ordinal, 0);
    assert_eq!(def.enum_defs[1].owner.as_deref(), Some("a"));
    assert_eq!(def.enum_defs[1].ordinal, 1);
    assert_eq!(def.enum_defs[2].owner.as_deref(), Some("b"));
    assert_eq!(def.enum_defs[2].ordinal, 0);
}

#[test]
fn type_of_typedef_enum_with_dims_merge() {
    // typedef enum { A, B } E; E x [3];
    // Should produce Array { elem: Enum, dim: Size(3) }
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
    // typedef enum { A, B } E [4];
    // The typedef itself has unpacked dims -- these must be preserved.
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
fn type_of_real_array() {
    // real x[4] -> Array(Real, Size(4)), pretty = "real [4]"
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
    // string s[2] -> Array(String, Size(2)), pretty = "string [2]"
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
    // logic [7:0] x [2][3] -> Array(Array(Integral(logic [7:0]), Size(3)), Size(2))
    // pretty = "logic [7:0] [2] [3]"
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; logic [7:0] x [2][3]; endmodule");
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "x");
    match ty {
        SymbolType::Value(ref outer @ Ty::Array { .. }) => {
            // Verify dims order via pretty printing
            assert_eq!(outer.pretty(), "logic [7:0] [2] [3]");
        }
        other => panic!("expected Value(Array(Array(Integral))), got {other:?}"),
    }
}

#[test]
fn type_of_typedef_integral_unpacked() {
    // typedef logic [7:0] T [4]; T x; -> Array(Integral(logic [7:0]), Size(4))
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

// Union type tests

#[test]
fn type_of_typedef_union_unpacked() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef union { int i; shortreal f; } num; num x; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let var = get_type(&db, file, unit, "x");
    match var {
        SymbolType::Value(Ty::Record(ref id)) => {
            let def = def_index_file(&db, file);
            let rec = &def.record_defs[0];
            assert_eq!(rec.kind, RecordKind::Union);
            assert_eq!(rec.packing, Packing::Unpacked);
            assert_eq!(id.owner.as_deref(), Some("m"));
        }
        other => panic!("expected Value(Record) for union, got {other:?}"),
    }
}

#[test]
fn type_of_typedef_union_packed() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef union packed { logic [7:0] a; logic [7:0] b; } u_t; u_t x; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let var = get_type(&db, file, unit, "x");
    match var {
        SymbolType::Value(Ty::Record(ref id)) => {
            let def = def_index_file(&db, file);
            let rec = &def.record_defs[0];
            assert_eq!(rec.kind, RecordKind::Union);
            assert_eq!(rec.packing, Packing::Packed);
            assert_eq!(id.owner.as_deref(), Some("m"));
        }
        other => panic!("expected Value(Record) for packed union, got {other:?}"),
    }
}

#[test]
fn type_of_inline_union() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; union { int a; logic [31:0] b; } u; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let var = get_type(&db, file, unit, "u");
    assert!(
        matches!(var, SymbolType::Value(Ty::Record(_))),
        "inline union should be Value(Record), got {var:?}"
    );
}

#[test]
fn union_def_fields_stable() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef union packed { logic [7:0] data; logic [7:0] alt; } u_t; endmodule",
    );
    let def = def_index_file(&db, file);
    assert_eq!(def.record_defs.len(), 1);
    let names: Vec<&str> = def.record_defs[0]
        .fields
        .iter()
        .map(|f| f.name.as_str())
        .collect();
    assert_eq!(names, vec!["data", "alt"]);
    assert_eq!(def.record_defs[0].kind, RecordKind::Union);
    assert_eq!(def.record_defs[0].packing, Packing::Packed);
}

#[test]
fn tagged_union_rejected() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef union tagged { int a; int b; } instr_t; instr_t x; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let var = get_type(&db, file, unit, "x");
    assert_eq!(
        var,
        SymbolType::Value(Ty::Error),
        "tagged union typedef should resolve to Ty::Error"
    );
    let diags = file_diagnostics(&db, file, unit);
    let has_tagged = diags.iter().any(|d| {
        d.render_message()
            .contains("tagged unions are not yet supported")
    });
    assert!(has_tagged, "should emit tagged union diagnostic: {diags:?}");
}

// Enum member name resolution tests

#[test]
fn type_of_enum_member() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef enum { IDLE, RUNNING } state_t; state_t s; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let idle_ty = get_type(&db, file, unit, "IDLE");
    let s_ty = get_type(&db, file, unit, "s");
    // Both should be Value(Enum) with the same EnumId
    match (&idle_ty, &s_ty) {
        (SymbolType::Value(Ty::Enum(id1)), SymbolType::Value(Ty::Enum(id2))) => {
            assert_eq!(id1, id2, "IDLE and s should share the same EnumId");
        }
        _ => panic!("expected Value(Enum) for both, got IDLE={idle_ty:?}, s={s_ty:?}"),
    }
}

#[test]
fn type_of_inline_enum_member() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; enum { A, B } x; endmodule");
    let unit = single_file_unit(&db, file);
    let a_ty = get_type(&db, file, unit, "A");
    assert!(
        matches!(a_ty, SymbolType::Value(Ty::Enum(_))),
        "inline enum member A should be Value(Enum), got {a_ty:?}"
    );
}

#[test]
fn type_of_enum_member_variant_id() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef enum { IDLE, RUNNING } state_t; endmodule",
    );
    let def = def_index_file(&db, file);
    // Find IDLE and RUNNING symbols and verify their origins
    let idle = def
        .symbols
        .iter()
        .find(|(_, s)| s.name == "IDLE")
        .expect("IDLE symbol");
    let running = def
        .symbols
        .iter()
        .find(|(_, s)| s.name == "RUNNING")
        .expect("RUNNING symbol");
    match idle.1.origin {
        lyra_semantic::record::SymbolOrigin::EnumVariant {
            variant_ordinal, ..
        } => {
            assert_eq!(variant_ordinal, 0, "IDLE should be ordinal 0");
        }
        other => panic!("expected EnumVariant origin for IDLE, got {other:?}"),
    }
    match running.1.origin {
        lyra_semantic::record::SymbolOrigin::EnumVariant {
            variant_ordinal, ..
        } => {
            assert_eq!(variant_ordinal, 1, "RUNNING should be ordinal 1");
        }
        other => panic!("expected EnumVariant origin for RUNNING, got {other:?}"),
    }
}

#[test]
fn enum_member_name_collision() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         typedef enum { IDLE, RUN } a_t;\n\
         typedef enum { IDLE, STOP } b_t;\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    let diags = file_diagnostics(&db, file, unit);
    let dup_diags: Vec<_> = diags
        .iter()
        .filter(|d| d.render_message().contains("duplicate"))
        .collect();
    assert!(
        !dup_diags.is_empty(),
        "should diagnose duplicate IDLE in value namespace: {diags:?}"
    );
}
