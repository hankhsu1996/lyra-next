use super::*;

#[test]
fn typedef_resolves_locally() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef logic [7:0] byte_t; byte_t x; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let diags = file_diagnostics(&db, file, unit);
    let unresolved: Vec<_> = diags
        .iter()
        .filter(|d| d.render_message().contains("unresolved"))
        .collect();
    assert!(
        unresolved.is_empty(),
        "byte_t should resolve locally: {diags:?}"
    );
}

#[test]
fn typedef_value_type_coexist() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; logic x; typedef int x; endmodule");
    let unit = single_file_unit(&db, file);
    let diags = file_diagnostics(&db, file, unit);
    let dup_diags: Vec<_> = diags
        .iter()
        .filter(|d| d.render_message().contains("duplicate"))
        .collect();
    assert!(
        dup_diags.is_empty(),
        "value 'x' and type 'x' should not conflict: {diags:?}"
    );
}

#[test]
fn typedef_same_namespace_duplicate() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef int t; typedef logic t; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let diags = file_diagnostics(&db, file, unit);
    let dup_diags: Vec<_> = diags
        .iter()
        .filter(|d| d.render_message().contains("duplicate"))
        .collect();
    assert!(
        !dup_diags.is_empty(),
        "two typedefs with same name should be duplicate: {diags:?}"
    );
}

#[test]
fn package_typedef_via_qualified_name() {
    let db = LyraDatabase::default();
    let file_a = new_file(
        &db,
        0,
        "package pkg; typedef logic [7:0] my_type; endpackage",
    );
    let file_b = new_file(&db, 1, "module m; typedef pkg::my_type local_t; endmodule");
    let unit = new_compilation_unit(&db, vec![file_a, file_b]);
    let diags = file_diagnostics(&db, file_b, unit);
    let unresolved: Vec<_> = diags
        .iter()
        .filter(|d| d.render_message().contains("unresolved"))
        .collect();
    assert!(
        unresolved.is_empty(),
        "pkg::my_type should resolve: {diags:?}"
    );
}

#[test]
fn package_typedef_via_explicit_import() {
    let db = LyraDatabase::default();
    let file_a = new_file(
        &db,
        0,
        "package pkg; typedef logic [7:0] my_type; endpackage",
    );
    let file_b = new_file(
        &db,
        1,
        "module m; import pkg::my_type; my_type x; endmodule",
    );
    let unit = new_compilation_unit(&db, vec![file_a, file_b]);
    let diags = file_diagnostics(&db, file_b, unit);
    let unresolved: Vec<_> = diags
        .iter()
        .filter(|d| d.render_message().contains("unresolved"))
        .collect();
    assert!(
        unresolved.is_empty(),
        "my_type via explicit import should resolve: {diags:?}"
    );
}

#[test]
fn package_typedef_via_wildcard_import() {
    let db = LyraDatabase::default();
    let file_a = new_file(
        &db,
        0,
        "package pkg; typedef logic [7:0] my_type; endpackage",
    );
    let file_b = new_file(&db, 1, "module m; import pkg::*; my_type x; endmodule");
    let unit = new_compilation_unit(&db, vec![file_a, file_b]);
    let diags = file_diagnostics(&db, file_b, unit);
    let unresolved: Vec<_> = diags
        .iter()
        .filter(|d| d.render_message().contains("unresolved"))
        .collect();
    assert!(
        unresolved.is_empty(),
        "my_type via wildcard import should resolve: {diags:?}"
    );
}

#[test]
fn local_typedef_shadows_imported() {
    let db = LyraDatabase::default();
    let file_a = new_file(
        &db,
        0,
        "package pkg; typedef logic [7:0] my_type; endpackage",
    );
    let file_b = new_file(
        &db,
        1,
        "module m; import pkg::*; typedef int my_type; my_type x; endmodule",
    );
    let unit = new_compilation_unit(&db, vec![file_a, file_b]);
    // 'my_type' in 'my_type x' should resolve to file_b's local typedef
    let text = file_b.text(&db);
    let pos = text.rfind("my_type x").expect("should find 'my_type x'");
    let result = resolve_at(&db, file_b, unit, lyra_source::TextSize::new(pos as u32));
    assert!(result.is_some(), "my_type should resolve");
    let sym_id = result.expect("checked");
    assert_eq!(
        sym_id.file,
        lyra_source::FileId(1),
        "local typedef should shadow import"
    );
}
