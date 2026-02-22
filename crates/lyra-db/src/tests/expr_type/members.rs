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
fn expr_type_enum_method_first() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         typedef enum { A, B, C } e_t;\n\
         e_t e;\n\
         parameter P = e.first();\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    let result = expr_type_of_first_param(&db, file, unit);
    assert!(
        matches!(result.ty, Ty::Enum(_)),
        "first() should return enum type, got {result:?}"
    );
}

#[test]
fn expr_type_enum_method_last() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         typedef enum { A, B, C } e_t;\n\
         e_t e;\n\
         parameter P = e.last();\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    let result = expr_type_of_first_param(&db, file, unit);
    assert!(
        matches!(result.ty, Ty::Enum(_)),
        "last() should return enum type, got {result:?}"
    );
}

#[test]
fn expr_type_enum_method_next() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         typedef enum { A, B, C } e_t;\n\
         e_t e;\n\
         parameter P = e.next();\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    let result = expr_type_of_first_param(&db, file, unit);
    assert!(
        matches!(result.ty, Ty::Enum(_)),
        "next() should return enum type, got {result:?}"
    );
}

#[test]
fn expr_type_enum_method_prev() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         typedef enum { A, B, C } e_t;\n\
         e_t e;\n\
         parameter P = e.prev();\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    let result = expr_type_of_first_param(&db, file, unit);
    assert!(
        matches!(result.ty, Ty::Enum(_)),
        "prev() should return enum type, got {result:?}"
    );
}

#[test]
fn expr_type_enum_method_num() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         typedef enum { A, B, C } e_t;\n\
         e_t e;\n\
         parameter P = e.num();\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::from_ty(&Ty::int()),
    );
}

#[test]
fn expr_type_enum_method_name() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         typedef enum { A, B, C } e_t;\n\
         e_t e;\n\
         parameter P = e.name();\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::from_ty(&Ty::String),
    );
}

#[test]
fn expr_type_enum_method_next_with_arg() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         typedef enum { A, B, C } e_t;\n\
         e_t e;\n\
         parameter P = e.next(2);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    let result = expr_type_of_first_param(&db, file, unit);
    assert!(
        matches!(result.ty, Ty::Enum(_)),
        "next(2) should return enum type, got {result:?}"
    );
}

#[test]
fn expr_type_enum_method_prev_with_arg() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         typedef enum { A, B, C } e_t;\n\
         e_t e;\n\
         parameter P = e.prev(1);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    let result = expr_type_of_first_param(&db, file, unit);
    assert!(
        matches!(result.ty, Ty::Enum(_)),
        "prev(1) should return enum type, got {result:?}"
    );
}

#[test]
fn expr_type_enum_method_bare_ref() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         typedef enum { A, B, C } e_t;\n\
         e_t e;\n\
         parameter P = e.first;\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::MethodRequiresCall),
    );
}

#[test]
fn expr_type_enum_method_first_arity() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         typedef enum { A, B, C } e_t;\n\
         e_t e;\n\
         parameter P = e.first(1);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::MethodArityMismatch),
    );
}

#[test]
fn expr_type_enum_method_next_arity() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         typedef enum { A, B, C } e_t;\n\
         e_t e;\n\
         parameter P = e.next(1, 2);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::MethodArityMismatch),
    );
}

#[test]
fn expr_type_enum_method_name_arity() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         typedef enum { A, B, C } e_t;\n\
         e_t e;\n\
         parameter P = e.name(1);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::MethodArityMismatch),
    );
}

#[test]
fn expr_type_enum_unknown_method() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         typedef enum { A, B, C } e_t;\n\
         e_t e;\n\
         parameter P = e.foo();\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::UnknownMember),
    );
}

#[test]
fn expr_type_enum_method_next_bad_arg() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         typedef enum { A, B, C } e_t;\n\
         e_t e;\n\
         parameter P = e.next(\"x\");\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::MethodArgNotIntegral),
    );
}

#[test]
fn expr_type_enum_method_next_unresolved_arg() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         typedef enum { A, B, C } e_t;\n\
         e_t e;\n\
         parameter P = e.next(undefined_name);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::Unresolved),
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

// Array method tests (LRM 7.5, 7.9, 7.10)

#[test]
fn expr_type_dynamic_array_size() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int d[];\n\
         parameter P = d.size();\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::from_ty(&Ty::int()),
    );
}

#[test]
fn expr_type_dynamic_array_delete_void() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int d[];\n\
         parameter P = d.delete();\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::VoidUsedAsExpr),
    );
}

#[test]
fn expr_type_dynamic_array_push_back_wrong_kind() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int d[];\n\
         parameter P = d.push_back(1);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::MethodNotValidOnReceiver(
            MethodInvalidReason::WrongArrayKind
        )),
    );
}

#[test]
fn expr_type_dynamic_array_num_wrong_kind() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int d[];\n\
         parameter P = d.num();\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::MethodNotValidOnReceiver(
            MethodInvalidReason::WrongArrayKind
        )),
    );
}

#[test]
fn expr_type_queue_size() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int q[$];\n\
         parameter P = q.size();\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::from_ty(&Ty::int()),
    );
}

#[test]
fn expr_type_queue_pop_front() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int q[$];\n\
         parameter P = q.pop_front();\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::from_ty(&Ty::int()),
    );
}

#[test]
fn expr_type_queue_pop_back() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int q[$];\n\
         parameter P = q.pop_back();\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::from_ty(&Ty::int()),
    );
}

#[test]
fn expr_type_queue_push_back_void() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int q[$];\n\
         int v;\n\
         parameter P = q.push_back(v);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::VoidUsedAsExpr),
    );
}

#[test]
fn expr_type_queue_exists_wrong_kind() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int q[$];\n\
         parameter P = q.exists(0);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::MethodNotValidOnReceiver(
            MethodInvalidReason::WrongArrayKind
        )),
    );
}

#[test]
fn expr_type_assoc_typed_size() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int aa[string];\n\
         parameter P = aa.size();\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::from_ty(&Ty::int()),
    );
}

#[test]
fn expr_type_assoc_typed_num() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int aa[string];\n\
         parameter P = aa.num();\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::from_ty(&Ty::int()),
    );
}

#[test]
fn expr_type_assoc_typed_exists() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int aa[string];\n\
         parameter P = aa.exists(\"k\");\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::from_ty(&Ty::int()),
    );
}

#[test]
fn expr_type_assoc_typed_first() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int aa[string];\n\
         string s;\n\
         parameter P = aa.first(s);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::from_ty(&Ty::int()),
    );
}

#[test]
fn expr_type_assoc_push_back_wrong_kind() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int aa[string];\n\
         parameter P = aa.push_back(1);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::MethodNotValidOnReceiver(
            MethodInvalidReason::WrongArrayKind
        )),
    );
}

#[test]
fn expr_type_assoc_wildcard_size() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int ww[*];\n\
         parameter P = ww.size();\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::from_ty(&Ty::int()),
    );
}

#[test]
fn expr_type_assoc_wildcard_num() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int ww[*];\n\
         parameter P = ww.num();\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::from_ty(&Ty::int()),
    );
}

#[test]
fn expr_type_assoc_wildcard_first_rejected() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int ww[*];\n\
         int k;\n\
         parameter P = ww.first(k);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::MethodNotValidOnReceiver(
            MethodInvalidReason::AssocKeyWildcard
        )),
    );
}

#[test]
fn expr_type_assoc_wildcard_exists_rejected() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int ww[*];\n\
         parameter P = ww.exists(0);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::MethodNotValidOnReceiver(
            MethodInvalidReason::AssocKeyWildcard
        )),
    );
}

#[test]
fn expr_type_fixed_array_size_rejected() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int fa[10];\n\
         parameter P = fa.size();\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::MethodNotValidOnReceiver(
            MethodInvalidReason::WrongArrayKind
        )),
    );
}

#[test]
fn expr_type_queue_size_arity_mismatch() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int q[$];\n\
         parameter P = q.size(1);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::MethodArityMismatch),
    );
}

#[test]
fn expr_type_queue_insert_arity_mismatch() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int q[$];\n\
         parameter P = q.insert(0);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::MethodArityMismatch),
    );
}

#[test]
fn expr_type_assoc_first_not_lvalue() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int aa[string];\n\
         parameter P = aa.first(\"literal\");\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::MethodArgNotLvalue),
    );
}

#[test]
fn expr_type_array_unknown_method() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int d[];\n\
         parameter P = d.foo();\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::UnknownMember),
    );
}

#[test]
fn expr_type_array_method_bare_ref() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int d[];\n\
         parameter P = d.size;\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::MethodRequiresCall),
    );
}

#[test]
fn expr_type_queue_push_back_bad_arg_type() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int q[$];\n\
         string s;\n\
         parameter P = q.push_back(s);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::MethodArgTypeMismatch),
    );
}
