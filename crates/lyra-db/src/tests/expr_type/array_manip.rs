use super::*;

// 7.12.1 locator methods

#[test]
fn find_returns_queue_of_elem() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int d[];\n\
         parameter P = d.find() with (item > 0);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    let result = expr_type_of_first_param(&db, file, unit);
    assert_eq!(
        result,
        ExprType::from_ty(&Ty::Array {
            elem: Box::new(Ty::int()),
            dim: UnpackedDim::Queue { bound: None },
        }),
    );
}

#[test]
fn find_index_returns_queue_of_int() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         logic [7:0] d[];\n\
         parameter P = d.find_index() with (item > 0);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    let result = expr_type_of_first_param(&db, file, unit);
    assert_eq!(
        result,
        ExprType::from_ty(&Ty::Array {
            elem: Box::new(Ty::int()),
            dim: UnpackedDim::Queue { bound: None },
        }),
    );
}

#[test]
fn min_returns_queue_of_elem() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int q[$];\n\
         parameter P = q.min();\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    let result = expr_type_of_first_param(&db, file, unit);
    assert_eq!(
        result,
        ExprType::from_ty(&Ty::Array {
            elem: Box::new(Ty::int()),
            dim: UnpackedDim::Queue { bound: None },
        }),
    );
}

#[test]
fn unique_index_returns_queue_of_int() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int d[];\n\
         parameter P = d.unique_index();\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    let result = expr_type_of_first_param(&db, file, unit);
    assert_eq!(
        result,
        ExprType::from_ty(&Ty::Array {
            elem: Box::new(Ty::int()),
            dim: UnpackedDim::Queue { bound: None },
        }),
    );
}

// 7.12.2 ordering methods (void)

#[test]
fn sort_is_void() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int d[];\n\
         parameter P = d.sort();\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::VoidUsedAsExpr),
    );
}

#[test]
fn reverse_is_void() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int d[];\n\
         parameter P = d.reverse();\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::VoidUsedAsExpr),
    );
}

// 7.12.3 reduction methods

#[test]
fn sum_returns_elem_type() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int d[];\n\
         parameter P = d.sum();\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::from_ty(&Ty::int()),
    );
}

#[test]
fn product_returns_elem_type() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         logic [7:0] q[$];\n\
         parameter P = q.product();\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u4(8),);
}

// 7.12 keyword method names

#[test]
fn keyword_and_method() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         logic [3:0] d[];\n\
         parameter P = d.and();\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u4(4),);
}

#[test]
fn keyword_or_method() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         logic [3:0] d[];\n\
         parameter P = d.or();\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u4(4),);
}

#[test]
fn keyword_xor_method() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         logic [3:0] d[];\n\
         parameter P = d.xor();\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u4(4),);
}

#[test]
fn keyword_unique_method() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int d[];\n\
         parameter P = d.unique();\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::from_ty(&Ty::Array {
            elem: Box::new(Ty::int()),
            dim: UnpackedDim::Queue { bound: None },
        }),
    );
}

// With clause: reduction override

#[test]
fn sum_with_clause_overrides_return_type() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         typedef struct { int x; logic [7:0] y; } item_t;\n\
         item_t d[];\n\
         parameter P = d.sum() with (item.x);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::from_ty(&Ty::int()),
    );
}

// Reject on assoc arrays

#[test]
fn find_rejected_on_assoc() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int aa[string];\n\
         parameter P = aa.find() with (item > 0);\n\
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

// Locator on fixed array

#[test]
fn find_on_fixed_array() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int fa[10];\n\
         parameter P = fa.find() with (item > 5);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::from_ty(&Ty::Array {
            elem: Box::new(Ty::int()),
            dim: UnpackedDim::Queue { bound: None },
        }),
    );
}

// 7.12.4 iterator index

#[test]
fn item_index_returns_int() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int d[];\n\
         parameter P = d.find() with (item.index() > 0);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::from_ty(&Ty::Array {
            elem: Box::new(Ty::int()),
            dim: UnpackedDim::Queue { bound: None },
        }),
    );
}

#[test]
fn item_index_with_dim_arg() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int d[];\n\
         parameter P = d.find() with (item.index(1) > 0);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::from_ty(&Ty::Array {
            elem: Box::new(Ty::int()),
            dim: UnpackedDim::Queue { bound: None },
        }),
    );
}

#[test]
fn item_index_not_on_non_iter_var() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int d[];\n\
         int other;\n\
         parameter P = d.find() with (other.index());\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    let result = expr_type_of_first_param(&db, file, unit);
    assert!(
        matches!(result.view, ExprView::Error(_)),
        "non-iter variable should not get .index(): {result:?}",
    );
}

// 7.12.5 map method

#[test]
fn map_returns_dynamic_array_of_with_type() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int d[];\n\
         parameter P = d.map() with (item);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::from_ty(&Ty::Array {
            elem: Box::new(Ty::int()),
            dim: UnpackedDim::Unsized,
        }),
    );
}

#[test]
fn map_struct_field_returns_field_type_array() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         typedef struct { int x; logic [7:0] y; } item_t;\n\
         item_t d[];\n\
         parameter P = d.map() with (item.x);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::from_ty(&Ty::Array {
            elem: Box::new(Ty::int()),
            dim: UnpackedDim::Unsized,
        }),
    );
}

// Missing with-clause diagnostics

#[test]
fn find_without_with_clause_errors() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int d[];\n\
         parameter P = d.find();\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::WithClauseRequired),
    );
}

#[test]
fn map_without_with_clause_errors() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int d[];\n\
         parameter P = d.map();\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::WithClauseRequired),
    );
}

// Custom iterator variable names (LRM 7.12)

#[test]
fn custom_iter_name_find() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int d[];\n\
         parameter P = d.find(x) with (x > 0);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::from_ty(&Ty::Array {
            elem: Box::new(Ty::int()),
            dim: UnpackedDim::Queue { bound: None },
        }),
    );
}

#[test]
fn custom_iter_name_index() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int d[];\n\
         parameter P = d.find(x) with (x.index() > 0);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::from_ty(&Ty::Array {
            elem: Box::new(Ty::int()),
            dim: UnpackedDim::Queue { bound: None },
        }),
    );
}

#[test]
fn custom_iter_name_map() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         typedef struct { int x; logic [7:0] y; } item_t;\n\
         item_t d[];\n\
         parameter P = d.map(el) with (el.x);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::from_ty(&Ty::Array {
            elem: Box::new(Ty::int()),
            dim: UnpackedDim::Unsized,
        }),
    );
}

// With clause on non-accepting methods

#[test]
fn reverse_with_clause_errors() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int d[];\n\
         parameter P = d.reverse() with (item);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::WithClauseNotAccepted),
    );
}

#[test]
fn shuffle_with_clause_errors() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int d[];\n\
         parameter P = d.shuffle() with (item);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::WithClauseNotAccepted),
    );
}

#[test]
fn sum_without_with_clause_ok() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         int d[];\n\
         parameter P = d.sum();\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::from_ty(&Ty::int()),
    );
}
