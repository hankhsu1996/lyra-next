use lyra_semantic::type_infer::{BitVecType, BitWidth, ExprType, ExprTypeErrorKind, Signedness};
use lyra_semantic::types::{ConstEvalError, RealKw, Ty};

use super::*;

/// Find the first parameter's init expression AstId and return its ExprType.
fn expr_type_of_first_param(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
) -> ExprType {
    let def = def_index_file(db, file);
    let (sym_id, _) = def
        .symbols
        .iter()
        .find(|(_, s)| s.kind == lyra_semantic::symbols::SymbolKind::Parameter)
        .expect("should have a parameter");
    let decl_ast_id = def.symbol_to_decl[sym_id.index()].expect("param should have decl");
    let init_ast_id = def
        .decl_to_init_expr
        .get(&decl_ast_id)
        .expect("param should be tracked")
        .expect("param should have init");
    let expr_ref = ExprRef::new(db, unit, init_ast_id);
    type_of_expr(db, expr_ref)
}

/// Find a named parameter's init expression and return its ExprType.
fn expr_type_of_named_param(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
    name: &str,
) -> ExprType {
    let def = def_index_file(db, file);
    let (sym_id, _) = def
        .symbols
        .iter()
        .find(|(_, s)| {
            s.kind == lyra_semantic::symbols::SymbolKind::Parameter && s.name.as_str() == name
        })
        .unwrap_or_else(|| panic!("parameter '{name}' not found"));
    let decl_ast_id = def.symbol_to_decl[sym_id.index()].expect("param should have decl");
    let init_ast_id = def
        .decl_to_init_expr
        .get(&decl_ast_id)
        .expect("param should be tracked")
        .expect("param should have init");
    let expr_ref = ExprRef::new(db, unit, init_ast_id);
    type_of_expr(db, expr_ref)
}

fn bv(width: u32, signed: Signedness) -> ExprType {
    ExprType::BitVec(BitVecType {
        width: BitWidth::Known(width),
        signed,
    })
}

fn bv_u(width: u32) -> ExprType {
    bv(width, Signedness::Unsigned)
}

fn bv_s(width: u32) -> ExprType {
    bv(width, Signedness::Signed)
}

fn one_bit() -> ExprType {
    bv_u(1)
}

// Literal tests

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
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u(1));
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
        ExprType::NonBit(Ty::String)
    );
}

#[test]
fn expr_type_real_literal() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; parameter real P = 3.14; endmodule");
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::NonBit(Ty::Real(RealKw::Real))
    );
}

// Name reference tests

#[test]
fn expr_type_name_ref() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic [7:0] x; parameter P = x; endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u(8));
}

// Parenthesized expression

#[test]
fn expr_type_paren() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic [7:0] x; parameter P = (x); endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u(8));
}

// Prefix expression tests

#[test]
fn expr_type_prefix_neg() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic [7:0] x; parameter P = -x; endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u(8));
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
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u(8));
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

// Binary expression tests

#[test]
fn expr_type_bin_add() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic [7:0] a; logic [3:0] b; parameter P = a + b; endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u(8));
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
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u(32));
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
    // Shift: width = lhs width, sign = lhs sign
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u(8));
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
    assert_eq!(expr_type_of_first_param(&db, file, unit), one_bit());
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
    assert_eq!(expr_type_of_first_param(&db, file, unit), one_bit());
}

#[test]
fn expr_type_power_unsupported() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic [7:0] a; logic [7:0] b; parameter P = a ** b; endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::Error(ExprTypeErrorKind::UnsupportedBinaryOp)
    );
}

// Conditional expression tests

#[test]
fn expr_type_cond() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic c; logic [7:0] a; logic [3:0] b; parameter P = c ? a : b; endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u(8));
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
        ExprType::Error(ExprTypeErrorKind::CondBranchTypeMismatch)
    );
}

// Concatenation tests

#[test]
fn expr_type_concat() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic [7:0] a; logic [3:0] b; parameter P = {a, b}; endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u(12));
}

#[test]
fn expr_type_concat_non_bit() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; parameter P = {\"a\", 8'h1}; endmodule");
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::Error(ExprTypeErrorKind::ConcatNonBitOperand)
    );
}

// Replication tests

#[test]
fn expr_type_replic() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic [7:0] a; parameter P = {3{a}}; endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u(24));
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
        ExprType::Error(ExprTypeErrorKind::ReplicationConstEvalFailed(
            ConstEvalError::NonConstant
        ))
    );
}

// Index expression tests

#[test]
fn expr_type_unpacked_index() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic x [3:0]; parameter P = x[0]; endmodule",
    );
    let unit = single_file_unit(&db, file);
    // Unpacked index peels off the first unpacked dim, result is scalar logic
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u(1));
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
    assert_eq!(expr_type_of_first_param(&db, file, unit), one_bit());
}

// Range expression (unsupported for M4)

#[test]
fn expr_type_range_unsupported() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic [7:0] y; parameter P = y[3:0]; endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::Error(ExprTypeErrorKind::RangeUnsupported)
    );
}

// Field expression (unsupported for M4)

#[test]
fn expr_type_field_error() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; logic x; parameter P = x.y; endmodule");
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::Error(ExprTypeErrorKind::FieldAccessUnsupported)
    );
}

// System call tests

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
        ExprType::Error(ExprTypeErrorKind::UnsupportedSystemCall)
    );
}

// Typedef name in expression context

#[test]
fn expr_type_typedef_name_in_expr() {
    let db = LyraDatabase::default();
    // Typedef is in the type namespace. When used as a value expression,
    // the resolver either finds it (resolves to TypeAlias -> NameRefIsTypeNotValue)
    // or doesn't resolve it at all (Unresolved). Both are valid error paths.
    // In the current implementation, typedef names used as values are unresolved
    // because the resolver only looks in the value namespace for expression contexts.
    let file = new_file(
        &db,
        0,
        "module m; typedef logic [7:0] byte_t; parameter P = byte_t; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let result = expr_type_of_first_param(&db, file, unit);
    assert!(
        matches!(
            result,
            ExprType::Error(ExprTypeErrorKind::Unresolved)
                | ExprType::Error(ExprTypeErrorKind::NameRefIsTypeNotValue)
        ),
        "expected Unresolved or NameRefIsTypeNotValue, got {result:?}"
    );
}

// Parameter expression (const-eval resolves width)

#[test]
fn expr_type_param_expr() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; parameter int W = 8; logic [W-1:0] x; parameter P = x; endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_named_param(&db, file, unit, "P"), bv_u(8));
}

// Cross-file name resolution

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
    assert_eq!(expr_type_of_named_param(&db, file_b, unit, "P"), bv_u(16));
}
