use super::*;

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
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u4(8));
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
    assert_eq!(
        expr_type_of_first_param(&db, file, unit),
        ExprType::from_ty(&Ty::int()),
    );
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
        ExprType::from_ty(&Ty::Void)
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
        ExprType::error(ExprTypeErrorKind::TaskInExprContext)
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
        ExprType::error(ExprTypeErrorKind::UnresolvedCall)
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
        ExprType::error(ExprTypeErrorKind::NotACallable(
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
        ExprType::error(ExprTypeErrorKind::UnsupportedCalleeForm(
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
    assert_eq!(
        expr_type_of_named_param(&db, file_mod, unit, "P"),
        ExprType::from_ty(&Ty::int()),
    );
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

    let result = expr_type_of_first_param(&db, file, unit);
    assert_eq!(result, bv_u4(16));

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
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u4(1));
}

#[test]
fn expr_type_function_typedef_return() {
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
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u4(16));
}

#[test]
fn expr_type_function_typedef_port() {
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
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u4(8));
}

#[test]
fn expr_type_function_implicit_port_type() {
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
    assert_eq!(expr_type_of_first_param(&db, file, unit), bv_u4(8));
}

#[test]
fn callable_sig_modport_qualified_port() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "interface my_bus; logic data; modport master(input data); endinterface\n\
         module m;\n\
         function void setup(my_bus.master b);\n\
         endfunction\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);

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
    assert_eq!(sig.ports.len(), 1, "should have 1 port");
    match &sig.ports[0].ty {
        lyra_semantic::types::Ty::Interface(it) => {
            assert!(
                it.modport.is_some(),
                "port type should have modport: {it:?}"
            );
        }
        other => panic!("expected Interface type, got {other:?}"),
    }
}
