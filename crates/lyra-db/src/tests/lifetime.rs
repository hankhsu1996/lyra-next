use super::*;
use lyra_semantic::scopes::ScopeKind;
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

/// Find the scope owned by a named declaration. For symbol-namespace owners
/// (functions, tasks), finds the symbol's `decl_site` and looks up the scope
/// owned by that site. For definition-namespace owners (modules, packages),
/// finds the def entry's `decl_site`. Panics if no match found.
fn find_owner_scope(
    db: &LyraDatabase,
    file: SourceFile,
    name: &str,
) -> lyra_semantic::scopes::ScopeId {
    let def = def_index_file(db, file);
    if let Some((_id, sym)) = def.symbols.iter().find(|(_, s)| s.name == name)
        && let Some(sid) = def.find_scope_by_owner(sym.decl_site)
    {
        return sid;
    }
    if let Some(entry) = def.def_entries.iter().find(|e| e.name == name)
        && let Some(sid) = def.find_scope_by_owner(entry.decl_site)
    {
        return sid;
    }
    panic!("no scope owned by declaration '{name}'");
}

/// Find the lifetime of a symbol named `name` declared in exactly `scope`.
/// Searches only that scope's bindings, not descendants. Panics if zero or
/// more than one match.
fn find_symbol_lifetime_in_scope(
    db: &LyraDatabase,
    file: SourceFile,
    scope: lyra_semantic::scopes::ScopeId,
    name: &str,
) -> Lifetime {
    let def = def_index_file(db, file);
    let mut matches: Vec<Lifetime> = Vec::new();
    for (_id, sym) in def.symbols.iter() {
        if sym.scope == scope && sym.name == name {
            matches.push(sym.lifetime);
        }
    }
    match matches.len() {
        0 => panic!("no symbol '{name}' in scope {scope:?}"),
        1 => matches[0],
        n => panic!("expected exactly 1 symbol '{name}' in scope {scope:?}, found {n}"),
    }
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

// Owner-anchored tests: same name in different scopes, verified by exact owner

#[test]
fn same_name_container_vs_callable_local() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module automatic m; int x; function void f; int x; endfunction endmodule",
    );
    let module_scope = find_owner_scope(&db, file, "m");
    let fn_scope = find_owner_scope(&db, file, "f");
    assert_eq!(
        find_symbol_lifetime_in_scope(&db, file, module_scope, "x"),
        Lifetime::Static,
    );
    assert_eq!(
        find_symbol_lifetime_in_scope(&db, file, fn_scope, "x"),
        Lifetime::Automatic,
    );
}

#[test]
fn nested_block_same_name_both_automatic() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; function automatic void f; int x; begin int x; end endfunction endmodule",
    );
    let def = def_index_file(&db, file);
    let fn_scope = find_owner_scope(&db, file, "f");
    let block_scopes = def.scopes.children_of_kind(fn_scope, ScopeKind::Block);
    assert_eq!(
        block_scopes.len(),
        1,
        "function f should have exactly one block child"
    );
    let block_scope = block_scopes[0];
    assert_eq!(
        find_symbol_lifetime_in_scope(&db, file, fn_scope, "x"),
        Lifetime::Automatic,
    );
    assert_eq!(
        find_symbol_lifetime_in_scope(&db, file, block_scope, "x"),
        Lifetime::Automatic,
    );
}
