use super::*;

#[test]
fn expr_type_int_literal() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; parameter P = 42; endmodule");
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_s(32));
}

#[test]
fn expr_type_sized_literal() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; parameter P = 8'hFF; endmodule");
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u(8));
}

#[test]
fn expr_type_unsized_based() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; parameter P = 'hFF; endmodule");
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u(32));
}

#[test]
fn expr_type_unbased_unsized() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; parameter P = '1; endmodule");
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_context());
}

#[test]
fn expr_type_unbased_unsized_in_binop() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic [7:0] a; parameter P = a + '1; endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u4(8));
}

#[test]
fn expr_type_string_literal() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; parameter string P = \"hello\"; endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::from_ty(&Ty::String)
    );
}

#[test]
fn expr_type_real_literal() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; parameter real P = 3.14; endmodule");
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::from_ty(&Ty::Real(RealKw::Real))
    );
}

#[test]
fn expr_type_literal_with_xz_is_four_state() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; parameter P = 8'hxF; endmodule");
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u4(8));
}

#[test]
fn expr_type_literal_without_xz_is_two_state() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; parameter P = 8'hFF; endmodule");
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u(8));
}

#[test]
fn expr_type_unbased_unsized_xz_is_four_state() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; parameter P = 'x; endmodule");
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::bitvec(BitVecType {
            width: BitWidth::ContextDependent,
            signed: Signedness::Unsigned,
            four_state: true,
        })
    );
}

#[test]
fn expr_type_name_ref() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic [7:0] x; parameter P = x; endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u4(8));
}

#[test]
fn expr_type_paren() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic [7:0] x; parameter P = (x); endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u4(8));
}

#[test]
fn expr_type_typedef_name_in_expr() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef logic [7:0] byte_t; parameter P = byte_t; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let result = expr_type_of_first_param(&db, file, unit);
    assert!(
        matches!(
            result.view,
            ExprView::Error(
                ExprTypeErrorKind::Unresolved | ExprTypeErrorKind::NameRefIsTypeNotValue
            )
        ),
        "expected Unresolved or NameRefIsTypeNotValue, got {result:?}"
    );
}

#[test]
fn expr_type_param_expr() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; parameter int W = 8; logic [W-1:0] x; parameter P = x; endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_named_param(&db, file, unit, "P"), bv_u4(8));
}

#[test]
fn expr_type_cross_file_name() {
    let db = LyraDatabase::default();
    let file_a = new_file(&db, 0, "package pkg; parameter int DATA_W = 16; endpackage");
    let file_b = new_file(
        &db,
        1,
        "module m; import pkg::*; logic [DATA_W-1:0] x; parameter P = x; endmodule",
    );
    let unit = new_compilation_unit(&db, vec![file_a, file_b]);
    assert_eq!(expr_type_of_named_param(&db, file_b, unit, "P"), bv_u4(16));
}
