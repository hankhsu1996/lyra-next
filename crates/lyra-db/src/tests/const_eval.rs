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
