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

// Function call tests

#[test]
fn expr_type_function_call_returns_type() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         function logic [7:0] add(input logic [7:0] a, logic [7:0] b);\n\
           add = a + b;\n\
         endfunction\n\
         parameter P = add(8'h01, 8'h02);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u(8));
}

#[test]
fn expr_type_function_call_int_return() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         function int get_val();\n\
           get_val = 42;\n\
         endfunction\n\
         parameter P = get_val();\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_s(32));
}

#[test]
fn expr_type_function_call_void_return() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         function void do_nothing();\n\
         endfunction\n\
         parameter P = do_nothing();\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::NonBit(Ty::Void)
    );
}

#[test]
fn expr_type_task_in_expr_context() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         task my_task(input logic a);\n\
         endtask\n\
         parameter P = my_task(1);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::Error(ExprTypeErrorKind::TaskInExprContext)
    );
}

#[test]
fn expr_type_function_call_unresolved() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         parameter P = unknown_func(1);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::Error(ExprTypeErrorKind::UnresolvedCall)
    );
}

#[test]
fn expr_type_not_a_callable() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         logic [7:0] not_func;\n\
         parameter P = not_func(1);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::Error(ExprTypeErrorKind::NotACallable(
            lyra_semantic::symbols::SymbolKind::Variable
        ))
    );
}

#[test]
fn expr_type_method_call_unsupported() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         logic [7:0] obj;\n\
         parameter P = obj.method(1);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::Error(ExprTypeErrorKind::UnsupportedCalleeForm(
            lyra_semantic::type_infer::CalleeFormKind::MethodCall
        ))
    );
}

#[test]
fn expr_type_package_function_call() {
    let db = LyraDatabase::default();
    let file_pkg = new_file(
        &db,
        0,
        "package pkg;\n\
         function int compute();\n\
           compute = 42;\n\
         endfunction\n\
         endpackage",
    );
    let file_mod = new_file(
        &db,
        1,
        "module m;\n\
         import pkg::*;\n\
         parameter P = compute();\n\
         endmodule",
    );
    let unit = new_compilation_unit(&db, vec![file_pkg, file_mod]);
    assert_eq!(expr_type_of_named_param(&db, file_mod, unit, "P"), bv_s(32));
}

#[test]
fn expr_type_system_call_still_works() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; parameter P = $clog2(16); endmodule");
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_s(32));
}

#[test]
fn callable_sig_projection_matches_db_sig() {
    // Smoke test: the CallableSigRef projection from DbInferCtx must agree
    // with the DB-level CallableSig on port count and return type.
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         function logic [15:0] add(input logic [15:0] a, logic [15:0] b);\n\
           add = a + b;\n\
         endfunction\n\
         parameter P = add(16'h1, 16'h2);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);

    // Via expression inference (exercises the projection path)
    let result = expr_type_of_first_param(&db, file, unit);
    assert_eq!(result, bv_u(16));

    // Via DB-level callable_signature directly
    let def = def_index_file(&db, file);
    let (sym_id, _) = def
        .symbols
        .iter()
        .find(|(_, s)| s.kind == lyra_semantic::symbols::SymbolKind::Function)
        .expect("should have a function symbol");
    let global_id = lyra_semantic::symbols::GlobalSymbolId {
        file: file.file_id(&db),
        local: sym_id,
    };
    let callable_ref = crate::callable_queries::CallableRef::new(&db, unit, global_id);
    let sig = crate::callable_queries::callable_signature(&db, callable_ref);
    assert_eq!(sig.ports.len(), 2, "DB sig should have 2 ports");
    assert_eq!(
        sig.kind,
        crate::module_sig::CallableKind::Function,
        "should be a function"
    );
}

#[test]
fn expr_type_function_implicit_return_type() {
    // P1: function with no explicit return type -> implicit logic (1 bit)
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         function foo();\n\
         endfunction\n\
         parameter P = foo();\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    // Implicit return type is logic (1-bit unsigned)
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u(1));
}

#[test]
fn expr_type_function_typedef_return() {
    // P2: function returning a user-defined type
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         typedef logic [15:0] word_t;\n\
         function word_t get_word();\n\
           get_word = 16'h1234;\n\
         endfunction\n\
         parameter P = get_word();\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u(16));
}

#[test]
fn expr_type_function_typedef_port() {
    // P2: function port with user-defined type
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         typedef logic [7:0] byte_t;\n\
         function byte_t identity(input byte_t x);\n\
           identity = x;\n\
         endfunction\n\
         parameter P = identity(8'hAB);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u(8));
}

#[test]
fn expr_type_function_implicit_port_type() {
    // TF port with no TypeSpec -> implicit logic (1-bit unsigned), LRM 13.3
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         function logic [7:0] f(input a);\n\
           f = {7'b0, a};\n\
         endfunction\n\
         parameter P = f(1'b1);\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    // Return type is explicit logic [7:0]
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u(8));
}
