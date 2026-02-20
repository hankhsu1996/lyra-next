use super::*;

#[test]
fn resolve_at_port() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m(input logic a); assign x = a; endmodule");
    let unit = single_file_unit(&db, file);
    // 'a' in 'assign x = a' is at offset 36
    let result = resolve_at(&db, file, unit, lyra_source::TextSize::new(36));
    assert!(result.is_some(), "port 'a' should resolve");
    let sym =
        symbol_global(&db, unit, result.expect("checked above")).expect("symbol should exist");
    assert_eq!(sym.name.as_str(), "a");
    assert_eq!(sym.kind, lyra_semantic::symbols::SymbolKind::Port);
}

#[test]
fn resolve_at_net() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; wire w; assign w = 1; endmodule");
    let unit = single_file_unit(&db, file);
    // 'w' in 'assign w = 1' -- find offset of the second 'w'
    let text = file.text(&db);
    let w_pos = text.rfind("w =").expect("should find 'w ='");
    let result = resolve_at(&db, file, unit, lyra_source::TextSize::new(w_pos as u32));
    assert!(result.is_some(), "net 'w' should resolve");
    let sym =
        symbol_global(&db, unit, result.expect("checked above")).expect("symbol should exist");
    assert_eq!(sym.name.as_str(), "w");
    assert_eq!(sym.kind, lyra_semantic::symbols::SymbolKind::Net);
}

#[test]
fn resolve_at_var() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; logic x; assign x = 0; endmodule");
    let unit = single_file_unit(&db, file);
    let text = file.text(&db);
    let x_pos = text.rfind("x =").expect("should find 'x ='");
    let result = resolve_at(&db, file, unit, lyra_source::TextSize::new(x_pos as u32));
    assert!(result.is_some(), "var 'x' should resolve");
    let sym =
        symbol_global(&db, unit, result.expect("checked above")).expect("symbol should exist");
    assert_eq!(sym.name.as_str(), "x");
    assert_eq!(sym.kind, lyra_semantic::symbols::SymbolKind::Variable);
}

#[test]
fn unresolved_diag() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; assign y = unknown_name; endmodule");
    let unit = single_file_unit(&db, file);
    let diags = file_diagnostics(&db, file, unit);
    let semantic_diags: Vec<_> = diags
        .iter()
        .filter(|d| d.render_message().contains("unresolved"))
        .collect();
    assert!(
        !semantic_diags.is_empty(),
        "should have unresolved name diagnostic"
    );
    assert!(
        semantic_diags
            .iter()
            .any(|d| d.render_message().contains("unknown_name")),
        "diagnostic should mention 'unknown_name': {semantic_diags:?}"
    );
}

#[test]
fn duplicate_diag() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; logic x; logic x; endmodule");
    let unit = single_file_unit(&db, file);
    let diags = file_diagnostics(&db, file, unit);
    let dup_diags: Vec<_> = diags
        .iter()
        .filter(|d| d.render_message().contains("duplicate"))
        .collect();
    assert!(
        !dup_diags.is_empty(),
        "should have duplicate definition diagnostic"
    );
}

#[test]
fn block_scope() {
    let db = LyraDatabase::default();
    // Variable declared inside begin/end should not be visible outside
    let file = new_file(
        &db,
        0,
        "module m; always_comb begin logic inner; inner = 1; end assign y = inner; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let diags = file_diagnostics(&db, file, unit);
    let unresolved: Vec<_> = diags
        .iter()
        .filter(|d| {
            let msg = d.render_message();
            msg.contains("unresolved") && msg.contains("inner")
        })
        .collect();
    assert!(
        !unresolved.is_empty(),
        "inner should not be visible outside the block: {diags:?}"
    );
}

#[test]
fn shadowing() {
    let db = LyraDatabase::default();
    let src = "module m(input logic a); always_comb begin logic a; a = 1; end endmodule";
    let file = new_file(&db, 0, src);
    let unit = single_file_unit(&db, file);
    // The 'a' inside the block should resolve to the block-local declaration
    let text = file.text(&db);
    let a_in_block = text.find("a = 1").expect("should find 'a = 1'");
    let result = resolve_at(
        &db,
        file,
        unit,
        lyra_source::TextSize::new(a_in_block as u32),
    );
    assert!(result.is_some(), "inner 'a' should resolve");
    let sym =
        symbol_global(&db, unit, result.expect("checked above")).expect("symbol should exist");
    assert_eq!(sym.name.as_str(), "a");
    assert_eq!(sym.kind, lyra_semantic::symbols::SymbolKind::Variable);
    // Inner 'a' is a Variable, not a Port
}

#[test]
fn multi_declarator() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; wire a, b; assign a = b; endmodule");
    let unit = single_file_unit(&db, file);
    let text = file.text(&db);
    // Resolve 'a' in assign
    let a_pos = text.find("a = b").expect("should find 'a = b'");
    let result_a = resolve_at(&db, file, unit, lyra_source::TextSize::new(a_pos as u32));
    assert!(result_a.is_some(), "'a' should resolve");
    // Resolve 'b' in assign
    let b_pos = text.rfind('b').expect("should find 'b'");
    let result_b = resolve_at(&db, file, unit, lyra_source::TextSize::new(b_pos as u32));
    assert!(result_b.is_some(), "'b' should resolve");
}

#[test]
fn module_not_in_lexical_scope() {
    let db = LyraDatabase::default();
    // Module name should be in exports, not resolvable as a lexical name
    let file = new_file(&db, 0, "module m; assign x = m; endmodule");
    let unit = single_file_unit(&db, file);
    let diags = file_diagnostics(&db, file, unit);
    let unresolved: Vec<_> = diags
        .iter()
        .filter(|d| {
            let msg = d.render_message();
            msg.contains("unresolved") && msg.contains("`m`")
        })
        .collect();
    assert!(
        !unresolved.is_empty(),
        "module name 'm' should not resolve as a lexical name: {diags:?}"
    );
    // But exports should contain it
    let def = def_index_file(&db, file);
    assert!(
        !def.exports.definitions.is_empty(),
        "module 'm' should be in exports"
    );
}

// Structured diagnostic tests

#[test]
fn duplicate_diag_has_secondary_label() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; logic x; logic x; endmodule");
    let unit = single_file_unit(&db, file);
    let diags = file_diagnostics(&db, file, unit);
    let dup = diags
        .iter()
        .find(|d| d.code == lyra_diag::DiagnosticCode::DUPLICATE_DEFINITION)
        .expect("should have duplicate diagnostic");
    assert_eq!(dup.labels.len(), 2, "primary + secondary labels");
    assert_eq!(dup.labels[0].kind, lyra_diag::LabelKind::Primary);
    assert_eq!(dup.labels[1].kind, lyra_diag::LabelKind::Secondary);
}

#[test]
fn unresolved_diag_has_code() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; assign y = unknown_name; endmodule");
    let unit = single_file_unit(&db, file);
    let diags = file_diagnostics(&db, file, unit);
    let unresolved = diags
        .iter()
        .find(|d| {
            d.code == lyra_diag::DiagnosticCode::UNRESOLVED_NAME
                && d.render_message().contains("unknown_name")
        })
        .expect("should have unresolved name diagnostic for unknown_name");
    assert_eq!(unresolved.severity, lyra_diag::Severity::Error);
}

#[test]
fn parse_error_has_code() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module top;");
    let unit = single_file_unit(&db, file);
    let diags = file_diagnostics(&db, file, unit);
    assert!(
        diags
            .iter()
            .any(|d| d.code == lyra_diag::DiagnosticCode::PARSE_ERROR),
        "should have parse error diagnostic with PARSE_ERROR code"
    );
}

// Cross-file resolution tests

#[test]
fn cross_file_module_instantiation() {
    let db = LyraDatabase::default();
    let file_a = new_file(
        &db,
        0,
        "module adder(input logic a, input logic b, output logic sum); assign sum = a + b; endmodule",
    );
    let file_b = new_file(
        &db,
        1,
        "module top; logic x, y, s; adder u1(.a(x), .b(y), .sum(s)); endmodule",
    );
    let unit = new_compilation_unit(&db, vec![file_a, file_b]);

    // Cursor on 'adder' in file_b's instantiation
    let text_b = file_b.text(&db);
    let adder_pos = text_b.find("adder").expect("should find 'adder'");
    let result = resolve_at(
        &db,
        file_b,
        unit,
        lyra_source::TextSize::new(adder_pos as u32),
    );
    assert!(result.is_some(), "'adder' should resolve cross-file");
    let sym_id = result.expect("checked above");
    assert_eq!(
        sym_id.file,
        lyra_source::FileId(0),
        "should resolve to file_a"
    );
    let sym = symbol_global(&db, unit, sym_id).expect("symbol should exist");
    assert_eq!(sym.name.as_str(), "adder");
    assert_eq!(sym.kind, lyra_semantic::symbols::SymbolKind::Module);
}

#[test]
fn unresolved_module_instantiation() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module top; logic x; nonexistent u1(.a(x)); endmodule",
    );
    let unit = single_file_unit(&db, file);
    let diags = file_diagnostics(&db, file, unit);
    let unresolved: Vec<_> = diags
        .iter()
        .filter(|d| {
            d.render_message().contains("unresolved") && d.render_message().contains("nonexistent")
        })
        .collect();
    assert!(
        !unresolved.is_empty(),
        "should have unresolved name diagnostic for 'nonexistent': {diags:?}"
    );
}

#[test]
fn no_lexical_fallback_for_definition_ns() {
    let db = LyraDatabase::default();
    // Local variable named 'adder' should NOT shadow a module instantiation
    let file = new_file(&db, 0, "module top; logic adder; adder u1(); endmodule");
    let unit = single_file_unit(&db, file);
    let diags = file_diagnostics(&db, file, unit);
    // The instantiation 'adder u1()' should be unresolved (no module 'adder' in unit)
    let unresolved: Vec<_> = diags
        .iter()
        .filter(|d| {
            let msg = d.render_message();
            msg.contains("unresolved") && msg.contains("adder")
        })
        .collect();
    assert!(
        !unresolved.is_empty(),
        "module instantiation should not fall back to lexical scope: {diags:?}"
    );
}

#[test]
fn duplicate_module_definition() {
    let db = LyraDatabase::default();
    let file_a = new_file(&db, 0, "module foo; endmodule");
    let file_b = new_file(&db, 1, "module foo; endmodule");
    let unit = new_compilation_unit(&db, vec![file_a, file_b]);
    let diags = unit_diagnostics(&db, unit);
    let dup_diags: Vec<_> = diags
        .iter()
        .filter(|d| d.render_message().contains("duplicate definition"))
        .collect();
    assert!(
        !dup_diags.is_empty(),
        "should have duplicate definition diagnostic: {diags:?}"
    );
}

#[test]
fn file_addition_triggers_resolution() {
    let db = LyraDatabase::default();
    // Initially, file_b has unresolved 'adder'
    let file_b = new_file(&db, 1, "module top; logic x; adder u1(.a(x)); endmodule");
    let unit = new_compilation_unit(&db, vec![file_b]);
    let diags = file_diagnostics(&db, file_b, unit);
    assert!(
        diags
            .iter()
            .any(|d| d.render_message().contains("unresolved")),
        "adder should be unresolved initially"
    );

    // Add file_a with module adder
    let file_a = new_file(&db, 0, "module adder(input logic a); endmodule");
    let unit2 = new_compilation_unit(&db, vec![file_a, file_b]);
    let diags2 = file_diagnostics(&db, file_b, unit2);
    let unresolved: Vec<_> = diags2
        .iter()
        .filter(|d| {
            let msg = d.render_message();
            msg.contains("unresolved") && msg.contains("adder")
        })
        .collect();
    assert!(
        unresolved.is_empty(),
        "adder should resolve after adding file_a: {diags2:?}"
    );
}

// Package resolution tests

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
    // Cursor on 'pkg' in 'pkg::val'
    let text = file_b.text(&db);
    let pkg_pos = text.find("pkg::").expect("should find 'pkg::'");
    let result = resolve_at(
        &db,
        file_b,
        unit,
        lyra_source::TextSize::new(pkg_pos as u32),
    );
    assert!(
        result.is_some(),
        "cursor on 'pkg' should resolve to package"
    );
    let sym_id = result.expect("checked above");
    assert_eq!(sym_id.file, lyra_source::FileId(0));
    let sym = symbol_global(&db, unit, sym_id).expect("symbol should exist");
    assert_eq!(sym.name.as_str(), "pkg");
    assert_eq!(sym.kind, lyra_semantic::symbols::SymbolKind::Package);
}

#[test]
fn package_symbols_in_exports() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "package my_pkg; logic x; endpackage");
    let def = def_index_file(&db, file);
    assert!(
        !def.exports.packages.is_empty(),
        "package should be in exports"
    );
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

// Typedef tests

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

// Undeclared type diagnostics

#[test]
fn undeclared_type_diag() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; unknown_t x; endmodule");
    let unit = single_file_unit(&db, file);
    let diags = file_diagnostics(&db, file, unit);
    let diag = diags
        .iter()
        .find(|d| d.code == lyra_diag::DiagnosticCode::UNDECLARED_TYPE)
        .expect("should have undeclared type diagnostic");
    assert_eq!(diag.severity, lyra_diag::Severity::Error);
    assert!(
        diag.render_message()
            .contains("undeclared type `unknown_t`"),
        "message should mention undeclared type: {}",
        diag.render_message()
    );
}

#[test]
fn undeclared_type_not_value() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic [7:0] a; assign a = unknown_name; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let diags = file_diagnostics(&db, file, unit);
    let unresolved = diags
        .iter()
        .find(|d| d.render_message().contains("unknown_name"))
        .expect("should have diagnostic for unknown_name");
    assert_eq!(
        unresolved.code,
        lyra_diag::DiagnosticCode::UNRESOLVED_NAME,
        "value-position name should use UNRESOLVED_NAME, not UNDECLARED_TYPE"
    );
}

#[test]
fn not_a_type_diag() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; int my_var; my_var x; endmodule");
    let unit = single_file_unit(&db, file);
    let diags = file_diagnostics(&db, file, unit);
    let diag = diags
        .iter()
        .find(|d| d.code == lyra_diag::DiagnosticCode::NOT_A_TYPE)
        .expect("should have not-a-type diagnostic");
    assert_eq!(diag.severity, lyra_diag::Severity::Error);
    assert!(
        diag.render_message().contains("`my_var` is not a type"),
        "message: {}",
        diag.render_message()
    );
}

#[test]
fn typedef_resolves_ok() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef logic [7:0] my_type; my_type x; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let diags = file_diagnostics(&db, file, unit);
    let type_diags: Vec<_> = diags
        .iter()
        .filter(|d| {
            d.code == lyra_diag::DiagnosticCode::UNDECLARED_TYPE
                || d.code == lyra_diag::DiagnosticCode::NOT_A_TYPE
        })
        .collect();
    assert!(
        type_diags.is_empty(),
        "typedef should resolve without type errors: {type_diags:?}"
    );
}

#[test]
fn qualified_pkg_not_found() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; assign y = no_pkg::t; endmodule");
    let unit = single_file_unit(&db, file);
    let diags = file_diagnostics(&db, file, unit);
    let diag = diags
        .iter()
        .find(|d| d.render_message().contains("no_pkg"))
        .expect("should have diagnostic mentioning no_pkg");
    assert_eq!(
        diag.code,
        lyra_diag::DiagnosticCode::PACKAGE_NOT_FOUND,
        "should be PACKAGE_NOT_FOUND, not UNDECLARED_TYPE"
    );
}

#[test]
fn qualified_member_not_found() {
    let db = LyraDatabase::default();
    let file_a = new_file(&db, 0, "package pkg; typedef int my_type; endpackage");
    let file_b = new_file(
        &db,
        1,
        "module m; typedef pkg::unknown_t local_t; endmodule",
    );
    let unit = new_compilation_unit(&db, vec![file_a, file_b]);
    let diags = file_diagnostics(&db, file_b, unit);
    let diag = diags
        .iter()
        .find(|d| d.render_message().contains("unknown_t"))
        .expect("should have diagnostic mentioning unknown_t");
    assert_eq!(
        diag.code,
        lyra_diag::DiagnosticCode::MEMBER_NOT_FOUND,
        "should be MEMBER_NOT_FOUND, not UNDECLARED_TYPE"
    );
}
