use super::*;

#[test]
fn system_call_clog2() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; parameter P = $clog2(8); endmodule");
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_s(32));
}

#[test]
fn system_call_unknown_returns_unsupported() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; parameter P = $random(0); endmodule");
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::error(ExprTypeErrorKind::UnsupportedSystemCall)
    );
}

#[test]
fn tyref_structural_interning() {
    let db = LyraDatabase::default();
    let ty1 = Ty::int();
    let ty2 = Ty::int();
    let ref1 = crate::type_queries::TyRef::new(&db, ty1);
    let ref2 = crate::type_queries::TyRef::new(&db, ty2);
    assert_eq!(ref1, ref2);
}
