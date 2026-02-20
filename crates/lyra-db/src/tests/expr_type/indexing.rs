use super::*;

#[test]
fn expr_type_concat() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic [7:0] a; logic [3:0] b; parameter P = {a, b}; endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u4(12));
}

#[test]
fn expr_type_concat_non_bit() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; parameter P = {\"a\", 8'h1}; endmodule");
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::ConcatNonBitOperand)
    );
}

#[test]
fn expr_type_replic() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic [7:0] a; parameter P = {3{a}}; endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u4(24));
}

#[test]
fn expr_type_replic_error_kind() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic [7:0] x; logic [7:0] W; parameter P = {W{x}}; endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::ReplicationConstEvalFailed(
            ConstEvalError::NonConstant
        ))
    );
}

#[test]
fn expr_type_unpacked_index() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic x [3:0]; parameter P = x[0]; endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u4(1));
}

#[test]
fn expr_type_packed_bitselect() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic [7:0] y; parameter P = y[0]; endmodule",
    );
    let unit = single_file_unit(&db, file);
    // Bit-select on 4-state type returns 4-state 1-bit unsigned
    assert_eq!(expr_type_of_first_param(&db, file, unit), one_bit_4());
}

#[test]
fn expr_type_part_select_fixed() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic [7:0] y; parameter P = y[3:0]; endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u4(4));
}

#[test]
fn expr_type_part_select_indexed_plus() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic [7:0] a; parameter P = a[0+:4]; endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u4(4));
}

#[test]
fn expr_type_part_select_indexed_minus() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic [7:0] a; parameter P = a[7-:4]; endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u4(4));
}

#[test]
fn expr_type_part_select_2state() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; bit [7:0] a; parameter P = a[3:0]; endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u(4));
}

#[test]
fn expr_type_part_select_non_integral() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; string s; parameter P = s[3:0]; endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::PartSelectNonIntegral)
    );
}

#[test]
fn expr_type_multidim_unpacked_chained() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic a [3:0][1:0]; parameter P = a[0][0]; endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u4(1));
}

#[test]
fn expr_type_packed_array_dim_peel() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic [3:0][7:0] x; parameter P = x[2]; endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u4(8));
}

#[test]
fn expr_type_bitselect_int() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; int x; parameter P = x[3]; endmodule");
    let unit = single_file_unit(&db, file);
    // int is 2-state, bit-select returns 2-state 1-bit
    assert_eq!(expr_type_of_first_param(&db, file, unit), one_bit());
}

#[test]
fn expr_type_index_error_propagates() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic [7:0] a; parameter P = a[unknown_name]; endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::Unresolved)
    );
}

#[test]
fn expr_type_nameref_preserves_array() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic [7:0] a [3:0]; parameter P = a[0]; endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u4(8));
}

#[test]
fn expr_type_field_error_non_composite() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; logic x; parameter P = x.y; endmodule");
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::MemberAccessOnNonComposite)
    );
}

#[test]
fn expr_type_system_call_clog2() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; parameter P = $clog2(8); endmodule");
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_s(32));
}

#[test]
fn expr_type_system_call_unsupported() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic [7:0] x; parameter P = $bits(x); endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::UnsupportedSystemCall)
    );
}
