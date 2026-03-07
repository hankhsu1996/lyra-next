use super::*;
use lyra_semantic::symbols::Lifetime;

fn find_symbol_lifetime(db: &LyraDatabase, file: SourceFile, name: &str) -> Lifetime {
    let def = def_index_file(db, file);
    let (_id, sym) = def
        .symbols
        .iter()
        .find(|(_, s)| s.name == name)
        .unwrap_or_else(|| panic!("symbol '{name}' not found"));
    sym.lifetime
}

fn all_symbol_lifetimes(db: &LyraDatabase, file: SourceFile, name: &str) -> Vec<Lifetime> {
    let def = def_index_file(db, file);
    def.symbols
        .iter()
        .filter(|(_, s)| s.name == name)
        .map(|(_, s)| s.lifetime)
        .collect()
}

#[test]
fn function_explicit_automatic() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; function automatic void f; endfunction endmodule",
    );
    assert_eq!(find_symbol_lifetime(&db, file, "f"), Lifetime::Automatic);
}

#[test]
fn function_explicit_static() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; function static void f; endfunction endmodule",
    );
    assert_eq!(find_symbol_lifetime(&db, file, "f"), Lifetime::Static);
}

#[test]
fn function_default_is_static() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; function void f; endfunction endmodule");
    assert_eq!(find_symbol_lifetime(&db, file, "f"), Lifetime::Static);
}

#[test]
fn task_explicit_automatic() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; task automatic t; endtask endmodule");
    assert_eq!(find_symbol_lifetime(&db, file, "t"), Lifetime::Automatic);
}

#[test]
fn task_explicit_static() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; task static t; endtask endmodule");
    assert_eq!(find_symbol_lifetime(&db, file, "t"), Lifetime::Static);
}

#[test]
fn task_default_is_static() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; task t; endtask endmodule");
    assert_eq!(find_symbol_lifetime(&db, file, "t"), Lifetime::Static);
}

// Callable-local lifetime inheritance (LRM 6.21)

#[test]
fn local_var_in_automatic_function_defaults_to_automatic() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; function automatic void f; int x; endfunction endmodule",
    );
    assert_eq!(find_symbol_lifetime(&db, file, "x"), Lifetime::Automatic);
}

#[test]
fn local_var_in_static_function_defaults_to_static() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; function static void f; int x; endfunction endmodule",
    );
    assert_eq!(find_symbol_lifetime(&db, file, "x"), Lifetime::Static);
}

#[test]
fn explicit_static_local_in_automatic_callable() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; function automatic void f; static int x; endfunction endmodule",
    );
    assert_eq!(find_symbol_lifetime(&db, file, "x"), Lifetime::Static);
}

#[test]
fn nested_block_inherits_automatic_from_callable() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; function automatic void f; begin int y; end endfunction endmodule",
    );
    assert_eq!(find_symbol_lifetime(&db, file, "y"), Lifetime::Automatic);
}

// Container callable-default inheritance

#[test]
fn module_automatic_makes_unqualified_callable_automatic() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module automatic m; function void f; endfunction endmodule",
    );
    assert_eq!(find_symbol_lifetime(&db, file, "f"), Lifetime::Automatic);
}

#[test]
fn package_automatic_makes_unqualified_callable_automatic() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "package automatic p; function void f; endfunction endpackage",
    );
    assert_eq!(find_symbol_lifetime(&db, file, "f"), Lifetime::Automatic);
}

#[test]
fn interface_automatic_makes_unqualified_callable_automatic() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "interface automatic ifc; function void f; endfunction endinterface",
    );
    assert_eq!(find_symbol_lifetime(&db, file, "f"), Lifetime::Automatic);
}

#[test]
fn program_automatic_makes_unqualified_callable_automatic() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "program automatic p; function void f; endfunction endprogram",
    );
    assert_eq!(find_symbol_lifetime(&db, file, "f"), Lifetime::Automatic);
}

#[test]
fn container_automatic_local_inherits_through_callable() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module automatic m; function void f; int x; endfunction endmodule",
    );
    assert_eq!(find_symbol_lifetime(&db, file, "f"), Lifetime::Automatic);
    assert_eq!(find_symbol_lifetime(&db, file, "x"), Lifetime::Automatic);
}

#[test]
fn explicit_static_callable_overrides_container_automatic() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module automatic m; function static void f; int x; endfunction endmodule",
    );
    assert_eq!(find_symbol_lifetime(&db, file, "f"), Lifetime::Static);
    assert_eq!(find_symbol_lifetime(&db, file, "x"), Lifetime::Static);
}

// Container-body variables are NOT affected by container automatic

#[test]
fn container_body_var_not_automatic() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module automatic m; int x; endmodule");
    assert_eq!(find_symbol_lifetime(&db, file, "x"), Lifetime::Static);
}

#[test]
fn package_body_var_not_automatic() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "package automatic p; int x; endpackage");
    assert_eq!(find_symbol_lifetime(&db, file, "x"), Lifetime::Static);
}

// Loop variable lifetimes (LRM 6.21)

#[test]
fn foreach_iteration_var_is_automatic() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; int arr[3]; initial foreach (arr[i]) begin end endmodule",
    );
    assert_eq!(find_symbol_lifetime(&db, file, "i"), Lifetime::Automatic);
}

#[test]
fn for_init_var_is_automatic() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; initial for (int i = 0; i < 10; i = i + 1) begin end endmodule",
    );
    assert_eq!(find_symbol_lifetime(&db, file, "i"), Lifetime::Automatic);
}

#[test]
fn for_init_var_automatic_even_in_static_context() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; function static void f; for (int j = 0; j < 5; j = j + 1) begin end endfunction endmodule",
    );
    assert_eq!(find_symbol_lifetime(&db, file, "j"), Lifetime::Automatic);
}

// Scope-anchored tests: same name in different scopes

#[test]
fn same_name_container_vs_callable_local() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module automatic m; int x; function void f; int x; endfunction endmodule",
    );
    let lifetimes = all_symbol_lifetimes(&db, file, "x");
    assert_eq!(lifetimes.len(), 2);
    // Container-body x is static, callable-local x is automatic
    assert!(lifetimes.contains(&Lifetime::Static));
    assert!(lifetimes.contains(&Lifetime::Automatic));
}

#[test]
fn nested_block_same_name_both_automatic() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; function automatic void f; int x; begin int x; end endfunction endmodule",
    );
    let lifetimes = all_symbol_lifetimes(&db, file, "x");
    assert_eq!(lifetimes.len(), 2);
    assert!(lifetimes.iter().all(|lt| *lt == Lifetime::Automatic));
}
