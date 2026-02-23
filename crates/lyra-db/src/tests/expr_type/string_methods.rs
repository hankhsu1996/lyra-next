use super::*;

#[test]
fn expr_type_string_len() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         string s;\n\
         parameter P = s.len();\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::from_ty(&Ty::int()),
    );
}

#[test]
fn expr_type_string_getc() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         string s;\n\
         parameter P = s.getc(0);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::from_ty(&Ty::byte()),
    );
}

#[test]
fn expr_type_string_toupper() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         string s;\n\
         parameter P = s.toupper();\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::from_ty(&Ty::String),
    );
}

#[test]
fn expr_type_string_compare() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         string s, t;\n\
         parameter P = s.compare(t);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::from_ty(&Ty::int()),
    );
}

#[test]
fn expr_type_string_atoi() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         string s;\n\
         parameter P = s.atoi();\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::from_ty(&Ty::integer()),
    );
}

#[test]
fn expr_type_string_atoreal() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         string s;\n\
         parameter P = s.atoreal();\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::from_ty(&Ty::Real(RealKw::Real)),
    );
}

#[test]
fn expr_type_string_substr() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         string s;\n\
         parameter P = s.substr(0, 3);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::from_ty(&Ty::String),
    );
}

#[test]
fn expr_type_string_putc_void() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         string s;\n\
         parameter P = s.putc(0, 65);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::VoidUsedAsExpr),
    );
}

#[test]
fn expr_type_string_itoa_void() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         string s;\n\
         integer i;\n\
         parameter P = s.itoa(i);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::VoidUsedAsExpr),
    );
}

#[test]
fn expr_type_string_realtoa_void() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         string s;\n\
         real r;\n\
         parameter P = s.realtoa(r);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::VoidUsedAsExpr),
    );
}

#[test]
fn expr_type_string_unknown_method() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         string s;\n\
         parameter P = s.foo();\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::UnknownMember),
    );
}

#[test]
fn expr_type_string_bare_method_ref() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         string s;\n\
         parameter P = s.len;\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::MethodRequiresCall),
    );
}

#[test]
fn expr_type_string_len_arity() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         string s;\n\
         parameter P = s.len(1);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::MethodArityMismatch),
    );
}

#[test]
fn expr_type_string_putc_arity() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         string s;\n\
         parameter P = s.putc(0);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::MethodArityMismatch),
    );
}
