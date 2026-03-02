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
