use super::*;

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
