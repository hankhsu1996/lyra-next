use super::*;

#[test]
fn expr_type_prefix_neg() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic [7:0] x; parameter P = -x; endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u4(8));
}

#[test]
fn expr_type_prefix_logical_not() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic [7:0] x; parameter P = !x; endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), one_bit());
}

#[test]
fn expr_type_prefix_bitwise_not() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic [7:0] x; parameter P = ~x; endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u4(8));
}

#[test]
fn expr_type_prefix_reduction_and() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic [7:0] x; parameter P = &x; endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), one_bit());
}

#[test]
fn expr_type_bin_add() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic [7:0] a; logic [3:0] b; parameter P = a + b; endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u4(8));
}

#[test]
fn expr_type_bin_add_signed() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; int a; int b; parameter P = a + b; endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_s(32));
}

#[test]
fn expr_type_bin_mixed_sign() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; int a; logic [7:0] b; parameter P = a + b; endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u4(32));
}

#[test]
fn expr_type_shift() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic [7:0] a; logic [3:0] b; parameter P = a << b; endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u4(8));
}

#[test]
fn expr_type_relational() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic [7:0] a; logic [7:0] b; parameter P = a < b; endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), one_bit_4());
}

#[test]
fn expr_type_equality() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic [7:0] a; logic [7:0] b; parameter P = a == b; endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), one_bit_4());
}

#[test]
fn expr_type_case_equality_is_two_state() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic [7:0] a, b; parameter P = a === b; endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), one_bit());
}

#[test]
fn expr_type_logical_and() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic a; logic b; parameter P = a && b; endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), one_bit_4());
}

#[test]
fn expr_type_power() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic [7:0] a; logic [7:0] b; parameter P = a ** b; endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u4(8));
}

#[test]
fn expr_type_cond() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic c; logic [7:0] a; logic [3:0] b; parameter P = c ? a : b; endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u4(8));
}

#[test]
fn expr_type_cond_mismatch() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; parameter P = 1 ? 1 : \"hello\"; endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::CondBranchTypeMismatch)
    );
}
