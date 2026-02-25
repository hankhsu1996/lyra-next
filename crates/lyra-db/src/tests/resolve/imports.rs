use super::*;

#[test]
fn import_explicit_resolves_cross_file() {
    let db = LyraDatabase::default();
    let file_a = new_file(&db, 0, "package pkg; logic x; endpackage");
    let file_b = new_file(&db, 1, "module m; import pkg::x; assign y = x; endmodule");
    let unit = new_compilation_unit(&db, vec![file_a, file_b]);
    let text = file_b.text(&db);
    let x_pos = text.rfind("x;").expect("should find 'x;'");
    let result = resolve_at(&db, file_b, unit, lyra_source::TextSize::new(x_pos as u32));
    assert!(result.is_some(), "'x' should resolve via explicit import");
    let sym_id = result.expect("checked above");
    assert_eq!(
        sym_id.file,
        lyra_source::FileId(0),
        "should resolve to pkg file"
    );
    let sym = symbol_global(&db, unit, sym_id).expect("symbol should exist");
    assert_eq!(sym.name.as_str(), "x");
}

#[test]
fn qualified_name_resolves_cross_file() {
    let db = LyraDatabase::default();
    let file_a = new_file(&db, 0, "package pkg; logic val; endpackage");
    let file_b = new_file(&db, 1, "module m; assign y = pkg::val; endmodule");
    let unit = new_compilation_unit(&db, vec![file_a, file_b]);
    // Cursor on 'val' in 'pkg::val'
    let text = file_b.text(&db);
    let val_pos = text.find("val").expect("should find 'val'");
    let result = resolve_at(
        &db,
        file_b,
        unit,
        lyra_source::TextSize::new(val_pos as u32),
    );
    assert!(result.is_some(), "'val' in pkg::val should resolve");
    let sym_id = result.expect("checked above");
    assert_eq!(sym_id.file, lyra_source::FileId(0));
    let sym = symbol_global(&db, unit, sym_id).expect("symbol should exist");
    assert_eq!(sym.name.as_str(), "val");
}

#[test]
fn wildcard_import_resolves() {
    let db = LyraDatabase::default();
    let file_a = new_file(&db, 0, "package pkg; logic x; endpackage");
    let file_b = new_file(&db, 1, "module m; import pkg::*; assign y = x; endmodule");
    let unit = new_compilation_unit(&db, vec![file_a, file_b]);
    let text = file_b.text(&db);
    let x_pos = text.rfind("x;").expect("should find 'x;'");
    let result = resolve_at(&db, file_b, unit, lyra_source::TextSize::new(x_pos as u32));
    assert!(result.is_some(), "'x' should resolve via wildcard import");
    let sym_id = result.expect("checked above");
    assert_eq!(sym_id.file, lyra_source::FileId(0));
}

#[test]
fn local_shadows_import() {
    let db = LyraDatabase::default();
    let file_a = new_file(&db, 0, "package pkg; logic x; endpackage");
    let file_b = new_file(
        &db,
        1,
        "module m; import pkg::*; logic x; assign y = x; endmodule",
    );
    let unit = new_compilation_unit(&db, vec![file_a, file_b]);
    let text = file_b.text(&db);
    let x_pos = text.rfind("x;").expect("should find 'x;'");
    let result = resolve_at(&db, file_b, unit, lyra_source::TextSize::new(x_pos as u32));
    assert!(result.is_some(), "'x' should resolve to local declaration");
    let sym_id = result.expect("checked above");
    // Local declaration is in file_b
    assert_eq!(
        sym_id.file,
        lyra_source::FileId(1),
        "local should shadow import"
    );
}

#[test]
fn explicit_import_shadows_wildcard() {
    let db = LyraDatabase::default();
    let file_a = new_file(&db, 0, "package p1; logic x; endpackage");
    let file_b = new_file(&db, 1, "package p2; logic x; endpackage");
    let file_c = new_file(
        &db,
        2,
        "module m; import p1::*; import p2::x; assign y = x; endmodule",
    );
    let unit = new_compilation_unit(&db, vec![file_a, file_b, file_c]);
    let text = file_c.text(&db);
    let x_pos = text.rfind("x;").expect("should find 'x;'");
    let result = resolve_at(&db, file_c, unit, lyra_source::TextSize::new(x_pos as u32));
    assert!(result.is_some(), "'x' should resolve via explicit import");
    let sym_id = result.expect("checked above");
    assert_eq!(
        sym_id.file,
        lyra_source::FileId(1),
        "explicit import from p2 should shadow wildcard from p1"
    );
}

#[test]
fn unresolved_import_package_not_found() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; import nonexistent::x; endmodule");
    let unit = single_file_unit(&db, file);
    let diags = file_diagnostics(&db, file, unit);
    assert!(
        diags.iter().any(|d| {
            let msg = d.render_message();
            msg.contains("nonexistent") && msg.contains("not found")
        }),
        "should have package not found diagnostic: {diags:?}"
    );
}

#[test]
fn import_member_not_found() {
    let db = LyraDatabase::default();
    let file_a = new_file(&db, 0, "package pkg; logic x; endpackage");
    let file_b = new_file(&db, 1, "module m; import pkg::z; endmodule");
    let unit = new_compilation_unit(&db, vec![file_a, file_b]);
    let diags = file_diagnostics(&db, file_b, unit);
    assert!(
        diags.iter().any(|d| {
            let msg = d.render_message();
            msg.contains('z') && msg.contains("not found")
        }),
        "should have member not found diagnostic: {diags:?}"
    );
}

#[test]
fn wildcard_ambiguity() {
    let db = LyraDatabase::default();
    let file_a = new_file(&db, 0, "package p1; logic x; endpackage");
    let file_b = new_file(&db, 1, "package p2; logic x; endpackage");
    let file_c = new_file(
        &db,
        2,
        "module m; import p1::*; import p2::*; assign y = x; endmodule",
    );
    let unit = new_compilation_unit(&db, vec![file_a, file_b, file_c]);
    let diags = file_diagnostics(&db, file_c, unit);
    assert!(
        diags.iter().any(|d| {
            let msg = d.render_message();
            msg.contains("ambiguous") || msg.contains('x')
        }),
        "should have ambiguous import diagnostic: {diags:?}"
    );
}

#[test]
fn module_package_name_collision() {
    let db = LyraDatabase::default();
    let file_a = new_file(&db, 0, "module foo; endmodule");
    let file_b = new_file(&db, 1, "package foo; endpackage");
    let unit = new_compilation_unit(&db, vec![file_a, file_b]);
    let diags = unit_diagnostics(&db, unit);
    assert!(
        diags
            .iter()
            .any(|d| d.render_message().contains("duplicate definition")),
        "module and package 'foo' should collide: {diags:?}"
    );
}

#[test]
fn qualified_name_cursor_on_package() {
    let db = LyraDatabase::default();
    let file_a = new_file(&db, 0, "package pkg; logic val; endpackage");
    let file_b = new_file(&db, 1, "module m; assign y = pkg::val; endmodule");
    let unit = new_compilation_unit(&db, vec![file_a, file_b]);
    // Packages are def-namespace entries, not symbols, so resolve_at returns None
    // for cursor on the package name. Verify the package exists in the global index.
    let global = global_def_index(&db, unit);
    let pkg = global.resolve_package("pkg");
    assert!(pkg.is_some(), "package 'pkg' should be in global index");
    // Cursor on 'val' should still resolve to the member symbol
    let text = file_b.text(&db);
    let val_pos = text.find("val").expect("should find 'val'");
    let result = resolve_at(
        &db,
        file_b,
        unit,
        lyra_source::TextSize::new(val_pos as u32),
    );
    assert!(result.is_some(), "cursor on 'val' should resolve");
}

#[test]
fn package_symbols_in_exports() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "package my_pkg; logic x; endpackage");
    let def = def_index_file(&db, file);
    let has_pkg = def
        .defs_by_name
        .iter()
        .filter_map(|id| def.def_entry(*id))
        .any(|e| e.kind == lyra_semantic::global_index::DefinitionKind::Package);
    assert!(has_pkg, "package should be in def_entries");
}

#[test]
fn qualified_name_resolves_parameter() {
    let db = LyraDatabase::default();
    let file_a = new_file(&db, 0, "package pkg; parameter WIDTH = 8; endpackage");
    let file_b = new_file(&db, 1, "module m; assign y = pkg::WIDTH; endmodule");
    let unit = new_compilation_unit(&db, vec![file_a, file_b]);
    let text = file_b.text(&db);
    let pos = text.find("WIDTH").expect("should find 'WIDTH'");
    let result = resolve_at(&db, file_b, unit, lyra_source::TextSize::new(pos as u32));
    assert!(result.is_some(), "'WIDTH' in pkg::WIDTH should resolve");
    let sym_id = result.expect("checked above");
    assert_eq!(sym_id.file, lyra_source::FileId(0));
    let sym = symbol_global(&db, unit, sym_id).expect("symbol should exist");
    assert_eq!(sym.name.as_str(), "WIDTH");
}
