use super::*;

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
