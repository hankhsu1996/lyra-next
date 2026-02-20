use super::*;

#[test]
fn expr_type_struct_field_access_int() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         typedef struct { int a; logic [7:0] b; } foo_t;\n\
         foo_t x;\n\
         parameter P = x.a;\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::from_ty(&Ty::int()),
    );
}

#[test]
fn expr_type_struct_field_access_logic() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         typedef struct { int a; logic [7:0] b; } foo_t;\n\
         foo_t x;\n\
         parameter P = x.b;\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u4(8));
}

#[test]
fn expr_type_struct_unknown_field() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         typedef struct { int a; } foo_t;\n\
         foo_t x;\n\
         parameter P = x.nonexistent;\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::UnknownMember),
    );
}

#[test]
fn expr_type_struct_nested_access() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         typedef struct { int x; } inner_t;\n\
         typedef struct { inner_t sub; } outer_t;\n\
         outer_t s;\n\
         parameter P = s.sub.x;\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::from_ty(&Ty::int()),
    );
}

#[test]
fn expr_type_struct_field_from_package() {
    let db = LyraDatabase::default();
    let file_pkg = new_file(
        &db,
        0,
        "package P;\n\
         typedef int T;\n\
         endpackage",
    );
    let file_mod = new_file(
        &db,
        1,
        "module m;\n\
         import P::*;\n\
         typedef struct { T val; } wrap_t;\n\
         wrap_t w;\n\
         parameter P1 = w.val;\n\
         endmodule",
    );
    let unit = new_compilation_unit(&db, vec![file_pkg, file_mod]);
    assert_eq!(
        expr_type_of_named_param(&db, file_mod, unit, "P1"),
        ExprType::from_ty(&Ty::int()),
    );
}

#[test]
fn expr_type_union_member_access() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         typedef union { int i; logic [7:0] b; } num_t;\n\
         num_t u;\n\
         parameter P = u.i;\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::from_ty(&Ty::int()),
    );
}

#[test]
fn expr_type_packed_union_member_access() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         typedef union packed { logic [7:0] a; logic [7:0] b; } u_t;\n\
         u_t u;\n\
         parameter P = u.a;\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u4(8));
}

#[test]
fn expr_type_union_nested_struct_member() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         typedef struct { int x; } inner_t;\n\
         typedef union { inner_t s; int i; } outer_t;\n\
         outer_t u;\n\
         parameter P = u.s.x;\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::from_ty(&Ty::int()),
    );
}

#[test]
fn expr_type_union_unknown_member() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         typedef union { int i; } u_t;\n\
         u_t u;\n\
         parameter P = u.nonexistent;\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::UnknownMember),
    );
}

#[test]
fn expr_type_enum_member_ref() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         typedef enum { IDLE, RUNNING } state_t;\n\
         parameter P = IDLE;\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    let result = expr_type_of_first_param(&db, file, unit);
    assert!(
        matches!(result.view, ExprView::Plain),
        "enum member ref should be Plain, got {result:?}"
    );
}

#[test]
fn expr_type_enum_member_in_assign() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         typedef enum { IDLE, RUNNING } state_t;\n\
         state_t s;\n\
         assign s = RUNNING;\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    let diags = file_diagnostics(&db, file, unit);
    let has_unresolved = diags
        .iter()
        .any(|d| d.render_message().contains("undeclared"));
    assert!(
        !has_unresolved,
        "RUNNING should resolve without errors: {diags:?}"
    );
}
