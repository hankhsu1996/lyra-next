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
fn signed_logic_8() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic [7:0] x; parameter P = $signed(x); endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        bv(8, Signedness::Signed, true)
    );
}

#[test]
fn unsigned_int() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; int x; parameter P = $unsigned(x); endmodule",
    );
    let unit = single_file_unit(&db, file);
    let result = expr_type_of_first_param(&db, file, unit);
    // int keyword preserved, signedness flipped to unsigned
    assert_eq!(
        result,
        ExprType::from_ty(&Ty::Integral(lyra_semantic::types::Integral {
            keyword: lyra_semantic::types::IntegralKw::Int,
            signed: false,
            packed: lyra_semantic::types::PackedDims::empty(),
        }))
    );
}

#[test]
fn unsigned_integer() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; integer x; parameter P = $unsigned(x); endmodule",
    );
    let unit = single_file_unit(&db, file);
    let result = expr_type_of_first_param(&db, file, unit);
    // integer keyword preserved, signedness flipped to unsigned
    assert_eq!(
        result,
        ExprType::from_ty(&Ty::Integral(lyra_semantic::types::Integral {
            keyword: lyra_semantic::types::IntegralKw::Integer,
            signed: false,
            packed: lyra_semantic::types::PackedDims::empty(),
        }))
    );
}

#[test]
fn signed_bit_vec() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; bit [3:0] x; parameter P = $signed(x); endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        bv(4, Signedness::Signed, false)
    );
}

#[test]
fn signed_preserves_width() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic [15:0] x; parameter P = $signed(x); endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        bv(16, Signedness::Signed, true)
    );
}

#[test]
fn signed_non_integral_noop() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; string s; parameter P = $signed(s); endmodule",
    );
    let unit = single_file_unit(&db, file);
    let result = expr_type_of_first_param(&db, file, unit);
    assert_eq!(result.view, ExprView::Plain);
    assert_eq!(result.ty, Ty::String);
}

#[test]
fn bits_expr() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic [7:0] x; parameter P = $bits(x); endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::from_ty(&Ty::int())
    );
}

#[test]
fn bits_type_int() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; parameter P = $bits(int); endmodule");
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::from_ty(&Ty::int())
    );
}

#[test]
fn bits_type_logic_dim() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; parameter P = $bits(logic [15:0]); endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::from_ty(&Ty::int())
    );
}

#[test]
fn bits_typedef() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef logic [7:0] byte_t; parameter P = $bits(byte_t); endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::from_ty(&Ty::int())
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
