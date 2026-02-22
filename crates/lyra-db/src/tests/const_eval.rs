use lyra_semantic::types::ConstInt;

use super::*;

/// Helper: find the first parameter's init expression `AstId` and evaluate it.
pub(crate) fn eval_first_param(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
) -> ConstInt {
    let def = def_index_file(db, file);
    // Find the first parameter symbol
    let (sym_id, _sym) = def
        .symbols
        .iter()
        .find(|(_, s)| s.kind == lyra_semantic::symbols::SymbolKind::Parameter)
        .expect("should have a parameter");
    let decl_ast_id = def.symbol_to_decl[sym_id.index()].expect("parameter should have decl");
    let init_ast_id = def
        .decl_to_init_expr
        .get(&decl_ast_id)
        .expect("parameter should be tracked")
        .expect("parameter should have init");
    let expr_ref = ConstExprRef::new(db, unit, init_ast_id);
    eval_const_int(db, expr_ref)
}

/// Helper: find a named parameter's init expression `AstId` and evaluate it.
pub(crate) fn eval_named_param(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
    name: &str,
) -> ConstInt {
    let def = def_index_file(db, file);
    let (sym_id, _sym) = def
        .symbols
        .iter()
        .find(|(_, s)| {
            s.kind == lyra_semantic::symbols::SymbolKind::Parameter && s.name.as_str() == name
        })
        .expect("should have named parameter");
    let decl_ast_id = def.symbol_to_decl[sym_id.index()].expect("parameter should have decl");
    let init_ast_id = def
        .decl_to_init_expr
        .get(&decl_ast_id)
        .expect("parameter should be tracked")
        .expect("parameter should have init");
    let expr_ref = ConstExprRef::new(db, unit, init_ast_id);
    eval_const_int(db, expr_ref)
}

#[test]
fn const_eval_simple_literal() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; parameter P = 42; endmodule");
    let unit = single_file_unit(&db, file);
    assert_eq!(eval_first_param(&db, file, unit), ConstInt::Known(42));
}

#[test]
fn const_eval_arithmetic() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; parameter P = 3 + 4; endmodule");
    let unit = single_file_unit(&db, file);
    assert_eq!(eval_first_param(&db, file, unit), ConstInt::Known(7));
}

#[test]
fn const_eval_param_chain() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; parameter A = 8; parameter B = A - 1; endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(eval_named_param(&db, file, unit, "B"), ConstInt::Known(7));
}

#[test]
fn const_eval_qualified_param() {
    let db = LyraDatabase::default();
    let file_a = new_file(&db, 0, "package pkg; parameter W = 8; endpackage");
    let file_b = new_file(&db, 1, "module m; parameter P = pkg::W; endmodule");
    let unit = new_compilation_unit(&db, vec![file_a, file_b]);
    assert_eq!(eval_first_param(&db, file_b, unit), ConstInt::Known(8));
}

#[test]
fn const_eval_cycle() {
    use lyra_semantic::types::ConstEvalError;
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; parameter A = B + 1; parameter B = A + 1; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let result = eval_named_param(&db, file, unit, "A");
    assert_eq!(result, ConstInt::Error(ConstEvalError::Cycle));
}

#[test]
fn const_eval_clog2() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; parameter P = $clog2(256); endmodule");
    let unit = single_file_unit(&db, file);
    assert_eq!(eval_first_param(&db, file, unit), ConstInt::Known(8));
}

// $bits type-form: keyword types

#[test]
fn const_eval_bits_int() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; parameter P = $bits(int); endmodule");
    let unit = single_file_unit(&db, file);
    assert_eq!(eval_first_param(&db, file, unit), ConstInt::Known(32));
}

#[test]
fn const_eval_bits_logic() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; parameter P = $bits(logic); endmodule");
    let unit = single_file_unit(&db, file);
    assert_eq!(eval_first_param(&db, file, unit), ConstInt::Known(1));
}

#[test]
fn const_eval_bits_logic_vec() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; parameter P = $bits(logic [7:0]); endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(eval_first_param(&db, file, unit), ConstInt::Known(8));
}

#[test]
fn const_eval_bits_packed_2d() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; parameter P = $bits(logic [3:0][7:0]); endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(eval_first_param(&db, file, unit), ConstInt::Known(32));
}

#[test]
fn const_eval_bits_real() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; parameter P = $bits(real); endmodule");
    let unit = single_file_unit(&db, file);
    assert_eq!(eval_first_param(&db, file, unit), ConstInt::Known(64));
}

#[test]
fn const_eval_bits_shortreal() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; parameter P = $bits(shortreal); endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(eval_first_param(&db, file, unit), ConstInt::Known(32));
}

#[test]
fn const_eval_bits_byte() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; parameter P = $bits(byte); endmodule");
    let unit = single_file_unit(&db, file);
    assert_eq!(eval_first_param(&db, file, unit), ConstInt::Known(8));
}

// $bits type-form: user-defined types

#[test]
fn const_eval_bits_typedef() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef logic [15:0] word_t; parameter P = $bits(word_t); endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(eval_first_param(&db, file, unit), ConstInt::Known(16));
}

#[test]
fn const_eval_bits_enum_default() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef enum {A, B} e_t; parameter P = $bits(e_t); endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(eval_first_param(&db, file, unit), ConstInt::Known(32));
}

#[test]
fn const_eval_bits_enum_explicit() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef enum logic [7:0] {A, B} e_t; parameter P = $bits(e_t); endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(eval_first_param(&db, file, unit), ConstInt::Known(8));
}

#[test]
fn const_eval_bits_struct() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef struct packed { logic [7:0] a; logic [3:0] b; } s_t; parameter P = $bits(s_t); endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(eval_first_param(&db, file, unit), ConstInt::Known(12));
}

#[test]
fn const_eval_bits_union() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef union packed { logic [7:0] a; logic [3:0] b; } u_t; parameter P = $bits(u_t); endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(eval_first_param(&db, file, unit), ConstInt::Known(8));
}

// $bits expr-form: simple names

#[test]
fn const_eval_bits_variable() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic [7:0] x; parameter P = $bits(x); endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(eval_first_param(&db, file, unit), ConstInt::Known(8));
}

#[test]
fn const_eval_bits_param_typed() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; parameter int A = 0; parameter P = $bits(A); endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(eval_named_param(&db, file, unit, "P"), ConstInt::Known(32));
}

// $bits expr-form: non-name expressions

#[test]
fn const_eval_bits_addition() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic [7:0] x; parameter P = $bits(x + 1); endmodule",
    );
    let unit = single_file_unit(&db, file);
    // Per LRM: x is 8-bit, unsized `1` is 32-bit (LRM 5.7.1),
    // addition result width = max(8, 32) = 32
    assert_eq!(eval_first_param(&db, file, unit), ConstInt::Known(32));
}

#[test]
fn const_eval_bits_struct_field() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef struct packed { logic [7:0] a; logic [3:0] b; } s_t; s_t s; parameter P = $bits(s.a); endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(eval_first_param(&db, file, unit), ConstInt::Known(8));
}

#[test]
fn const_eval_bits_bit_select() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic [7:0] x; parameter P = $bits(x[3]); endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(eval_first_param(&db, file, unit), ConstInt::Known(1));
}

// $bits composition

#[test]
fn const_eval_bits_in_expr() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; parameter P = $bits(int) + 1; endmodule");
    let unit = single_file_unit(&db, file);
    assert_eq!(eval_first_param(&db, file, unit), ConstInt::Known(33));
}

#[test]
fn const_eval_bits_cross_package() {
    let db = LyraDatabase::default();
    let file_pkg = new_file(&db, 0, "package P; typedef logic [15:0] word_t; endpackage");
    let file_mod = new_file(
        &db,
        1,
        "module m; import P::*; parameter W = $bits(word_t); endmodule",
    );
    let unit = new_compilation_unit(&db, vec![file_pkg, file_mod]);
    assert_eq!(eval_first_param(&db, file_mod, unit), ConstInt::Known(16),);
}

#[test]
fn const_eval_bits_param_dim() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; parameter W = 8; logic [W-1:0] x; parameter P = $bits(x); endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(eval_named_param(&db, file, unit, "P"), ConstInt::Known(8));
}

// $bits negative cases

#[test]
fn const_eval_bits_string() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; parameter P = $bits(string); endmodule");
    let unit = single_file_unit(&db, file);
    assert!(matches!(
        eval_first_param(&db, file, unit),
        ConstInt::Error(_),
    ));
}

#[test]
fn const_eval_bits_extra_args() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; parameter P = $bits(int, 1); endmodule");
    let unit = single_file_unit(&db, file);
    assert!(matches!(
        eval_first_param(&db, file, unit),
        ConstInt::Error(_),
    ));
}

#[test]
fn const_eval_no_initializer() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; parameter P; endmodule");
    let unit = single_file_unit(&db, file);
    let def = def_index_file(&db, file);
    let (sym_id, _) = def
        .symbols
        .iter()
        .find(|(_, s)| s.kind == lyra_semantic::symbols::SymbolKind::Parameter)
        .expect("should have parameter");
    let decl_ast_id = def.symbol_to_decl[sym_id.index()].expect("parameter should have decl");
    let init_opt = def.decl_to_init_expr.get(&decl_ast_id);
    // Parameter with no initializer should have None value
    match init_opt {
        Some(None) | None => {} // No initializer
        Some(Some(init_id)) => {
            // If it has an init_id, evaluating should give Unresolved
            let expr_ref = ConstExprRef::new(&db, unit, *init_id);
            let result = eval_const_int(&db, expr_ref);
            assert!(
                matches!(result, ConstInt::Error(_)),
                "no-init parameter should error: {result:?}"
            );
        }
    }
}
