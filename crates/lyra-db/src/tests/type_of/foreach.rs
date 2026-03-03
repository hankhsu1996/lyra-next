use lyra_ast::{AstNode, ForeachStmt, HasSyntax};
use lyra_semantic::type_infer::ExprType;

use super::*;

// Helper: get the ExprType of the foreach array header expression.
// Finds the first ForeachStmt in the file and types its array_expr.
fn foreach_array_expr_type(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
) -> ExprType {
    let parse = parse_file(db, file);
    let map = ast_id_map(db, file);
    let root = parse.syntax();
    let foreach_node = root
        .descendants()
        .find(|n| ForeachStmt::cast(n.clone()).is_some())
        .expect("should have a ForeachStmt");
    let foreach_stmt = ForeachStmt::cast(foreach_node).expect("cast ok");
    let array_expr = foreach_stmt.array_expr().expect("should have array_expr");
    let expr_site = map
        .erased_ast_id(array_expr.syntax())
        .expect("should have ast id");
    let expr_ref = ExprRef::new(db, unit, expr_site);
    type_of_expr(db, expr_ref)
}

// Foreach loop variable type: fixed-size array -> int
#[test]
fn foreach_var_fixed_array() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\
         int arr [4];\
         initial foreach (arr[i]) begin end\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(get_type(&db, file, unit, "i"), SymbolType::Value(Ty::int()));
}

// Foreach loop variable type: dynamic array -> int
#[test]
fn foreach_var_dynamic_array() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\
         int d [];\
         initial foreach (d[i]) begin end\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(get_type(&db, file, unit, "i"), SymbolType::Value(Ty::int()));
}

// Foreach loop variable type: queue -> int
#[test]
fn foreach_var_queue() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\
         int q [$];\
         initial foreach (q[i]) begin end\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(get_type(&db, file, unit, "i"), SymbolType::Value(Ty::int()));
}

// Foreach loop variable type: associative array with string index
#[test]
fn foreach_var_assoc_string_index() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\
         int aa [string];\
         initial foreach (aa[k]) begin end\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        get_type(&db, file, unit, "k"),
        SymbolType::Value(Ty::String)
    );
}

// Foreach loop variable type: associative array with int index
#[test]
fn foreach_var_assoc_int_index() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\
         int aa [int];\
         initial foreach (aa[k]) begin end\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(get_type(&db, file, unit, "k"), SymbolType::Value(Ty::int()));
}

// Foreach loop variable type: associative array with wildcard index -> int
#[test]
fn foreach_var_assoc_wildcard_index() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\
         int aa [*];\
         initial foreach (aa[k]) begin end\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(get_type(&db, file, unit, "k"), SymbolType::Value(Ty::int()));
}

// Foreach over multidimensional array: each var gets int
#[test]
fn foreach_var_multidim() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\
         int matrix [3][4];\
         initial foreach (matrix[i,j]) begin end\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(get_type(&db, file, unit, "i"), SymbolType::Value(Ty::int()));
    assert_eq!(get_type(&db, file, unit, "j"), SymbolType::Value(Ty::int()));
}

// Foreach with skipped slot: only present vars are typed
#[test]
fn foreach_var_skipped_slot() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\
         int cube [2][3][4];\
         initial foreach (cube[i,,k]) begin end\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    // i maps to slot 0 (dim [2]) -> int
    assert_eq!(get_type(&db, file, unit, "i"), SymbolType::Value(Ty::int()));
    // k maps to slot 2 (dim [4]) -> int
    assert_eq!(get_type(&db, file, unit, "k"), SymbolType::Value(Ty::int()));
}

// Foreach over mixed associative + fixed dims
#[test]
fn foreach_var_mixed_assoc_and_fixed() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\
         int mixed [string][4];\
         initial foreach (mixed[s,i]) begin end\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    // Slot 0: assoc[string] -> string
    assert_eq!(
        get_type(&db, file, unit, "s"),
        SymbolType::Value(Ty::String)
    );
    // Slot 1: fixed[4] -> int
    assert_eq!(get_type(&db, file, unit, "i"), SymbolType::Value(Ty::int()));
}

// Foreach with indexed array reference: a[0] as header
#[test]
fn foreach_var_indexed_array_ref() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\
         int a [3][4];\
         initial foreach (a[0][j]) begin end\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(get_type(&db, file, unit, "j"), SymbolType::Value(Ty::int()));
}

// Foreach with multidim partial index: m[0] leaves two dims for [i,j]
#[test]
fn foreach_var_multidim_partial_index() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\
         int x [2][3][4];\
         initial foreach (x[0][i,j]) begin end\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(get_type(&db, file, unit, "i"), SymbolType::Value(Ty::int()));
    assert_eq!(get_type(&db, file, unit, "j"), SymbolType::Value(Ty::int()));
}

// Foreach with field access: struct member as array ref
#[test]
fn foreach_var_field_access() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\
         typedef struct { int arr [3]; } foo_t;\
         foo_t s;\
         initial foreach (s.arr[i]) begin end\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(get_type(&db, file, unit, "i"), SymbolType::Value(Ty::int()));
}

// Verify that type_of_expr on indexed array ref peels one unpacked dim.
// int a[3][4] -> a[0] should be int[4] (one dim remaining).
#[test]
fn foreach_array_expr_indexed_peels_dim() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\
         int a [3][4];\
         initial foreach (a[0][j]) begin end\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    let et = foreach_array_expr_type(&db, file, unit);
    // a[0] should yield int[4]: one remaining unpacked dim
    let (_base, dims) = lyra_semantic::types::collect_array_dims(&et.ty);
    assert_eq!(
        dims.len(),
        1,
        "a[0] should have exactly 1 remaining unpacked dim"
    );
}

// Indexed header leaving an associative dim: foreach var gets string, not int.
// int mixed[3][string] -> mixed[0] leaves [string], var gets string.
#[test]
fn foreach_var_indexed_into_assoc_remaining() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\
         int mixed [3][string];\
         initial foreach (mixed[0][k]) begin end\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    // Verify the array expr type has one associative dim remaining
    let et = foreach_array_expr_type(&db, file, unit);
    let (_base, dims) = lyra_semantic::types::collect_array_dims(&et.ty);
    assert_eq!(dims.len(), 1, "mixed[0] should have 1 remaining dim");
    // The foreach var should be string (not int)
    assert_eq!(
        get_type(&db, file, unit, "k"),
        SymbolType::Value(Ty::String)
    );
}

// Verify type_of_expr on a 3D array with one index peels to 2D.
// int x[2][3][4] -> x[0] should be int[3][4] (two dims remaining).
#[test]
fn foreach_array_expr_partial_index_peels_one_dim() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\
         int x [2][3][4];\
         initial foreach (x[0][i,j]) begin end\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    let et = foreach_array_expr_type(&db, file, unit);
    let (_base, dims) = lyra_semantic::types::collect_array_dims(&et.ty);
    assert_eq!(
        dims.len(),
        2,
        "x[0] should have exactly 2 remaining unpacked dims"
    );
}

// Foreach over package-qualified field: p::obj.arr[i]
#[test]
fn foreach_var_package_qualified_field() {
    let db = LyraDatabase::default();
    let pkg_file = new_file(
        &db,
        0,
        "package p;\
         typedef struct { int arr [3]; } obj_t;\
         obj_t obj;\
         endpackage",
    );
    let mod_file = new_file(
        &db,
        1,
        "module m;\
         initial foreach (p::obj.arr[i]) begin end\
         endmodule",
    );
    let unit = new_compilation_unit(&db, vec![pkg_file, mod_file]);
    assert_eq!(
        get_type(&db, mod_file, unit, "i"),
        SymbolType::Value(Ty::int())
    );
}

// Foreach var slot out of range -> Error
#[test]
fn foreach_var_slot_out_of_range() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\
         int arr [4];\
         initial foreach (arr[i,j]) begin end\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    // i maps to slot 0 (dim [4]) -> int
    assert_eq!(get_type(&db, file, unit, "i"), SymbolType::Value(Ty::int()));
    // j maps to slot 1 -> no dim -> Error
    assert_eq!(get_type(&db, file, unit, "j"), SymbolType::Value(Ty::Error));
}
