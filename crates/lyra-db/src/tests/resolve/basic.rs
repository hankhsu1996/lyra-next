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
    assert_eq!(sym.kind, lyra_semantic::symbols::SymbolKind::PortAnsi);
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

#[test]
fn interface_instance_resolves_in_port_connection() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "interface my_bus; endinterface \
         module sink(my_bus bus); endmodule \
         module top; my_bus sb(); sink u(.bus(sb)); endmodule",
    );
    let unit = single_file_unit(&db, file);
    let diags = file_diagnostics(&db, file, unit);
    let unresolved: Vec<_> = diags
        .iter()
        .filter(|d| {
            let msg = d.render_message();
            msg.contains("unresolved") && msg.contains("`sb`")
        })
        .collect();
    assert!(
        unresolved.is_empty(),
        "interface instance 'sb' should resolve in port connection: {unresolved:?}"
    );
}

#[test]
fn module_instance_not_resolved_as_value() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module child; endmodule \
         module sink(input logic x); endmodule \
         module top; child u(); sink s(.x(u)); endmodule",
    );
    let unit = single_file_unit(&db, file);
    let diags = file_diagnostics(&db, file, unit);
    let unresolved: Vec<_> = diags
        .iter()
        .filter(|d| {
            let msg = d.render_message();
            msg.contains("unresolved") && msg.contains("`u`")
        })
        .collect();
    assert!(
        !unresolved.is_empty(),
        "module instance 'u' should NOT resolve as a value: {diags:?}"
    );
}

#[test]
fn interface_instance_shadows_import() {
    let db = LyraDatabase::default();
    let pkg = new_file(&db, 0, "package pkg; logic sb; endpackage");
    let top = new_file(
        &db,
        1,
        "interface my_bus; endinterface \
         module sink(my_bus bus); endmodule \
         module top; import pkg::*; my_bus sb(); sink u(.bus(sb)); endmodule",
    );
    let unit = new_compilation_unit(&db, vec![pkg, top]);
    let diags = file_diagnostics(&db, top, unit);
    let unresolved: Vec<_> = diags
        .iter()
        .filter(|d| {
            let msg = d.render_message();
            msg.contains("unresolved") && msg.contains("`sb`")
        })
        .collect();
    assert!(
        unresolved.is_empty(),
        "local interface instance 'sb' should shadow imported 'sb': {unresolved:?}"
    );
}
