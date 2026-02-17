use super::*;

#[test]
fn interface_in_exports() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "interface my_if; endinterface");
    let def = def_index_file(&db, file);
    assert_eq!(def.exports.definitions.len(), 1);
    let sym = def.symbols.get(def.exports.definitions[0]);
    assert_eq!(sym.name.as_str(), "my_if");
    assert_eq!(sym.kind, lyra_semantic::symbols::SymbolKind::Interface);
}

#[test]
fn program_in_exports() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "program my_prog; endprogram");
    let def = def_index_file(&db, file);
    assert_eq!(def.exports.definitions.len(), 1);
    let sym = def.symbols.get(def.exports.definitions[0]);
    assert_eq!(sym.name.as_str(), "my_prog");
    assert_eq!(sym.kind, lyra_semantic::symbols::SymbolKind::Program);
}

#[test]
fn primitive_in_exports() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "primitive my_udp(output y, input a); table 0 : 0; endtable endprimitive",
    );
    let def = def_index_file(&db, file);
    assert_eq!(def.exports.definitions.len(), 1);
    let sym = def.symbols.get(def.exports.definitions[0]);
    assert_eq!(sym.name.as_str(), "my_udp");
    assert_eq!(sym.kind, lyra_semantic::symbols::SymbolKind::Primitive);
}

#[test]
fn config_in_exports() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "config my_cfg; design top; endconfig");
    let def = def_index_file(&db, file);
    assert_eq!(def.exports.definitions.len(), 1);
    let sym = def.symbols.get(def.exports.definitions[0]);
    assert_eq!(sym.name.as_str(), "my_cfg");
    assert_eq!(sym.kind, lyra_semantic::symbols::SymbolKind::Config);
}

#[test]
fn cross_file_interface_instantiation() {
    let db = LyraDatabase::default();
    let file_a = new_file(&db, 0, "interface my_bus; endinterface");
    let file_b = new_file(&db, 1, "module top; my_bus u_bus(); endmodule");
    let unit = new_compilation_unit(&db, vec![file_a, file_b]);

    let text = file_b.text(&db);
    let pos = text.find("my_bus").expect("should find 'my_bus'");
    let result = resolve_at(&db, file_b, unit, lyra_source::TextSize::new(pos as u32));
    assert!(result.is_some(), "'my_bus' should resolve cross-file");
    let sym_id = result.expect("checked");
    assert_eq!(sym_id.file, lyra_source::FileId(0));
    let sym = symbol_global(&db, unit, sym_id).expect("symbol should exist");
    assert_eq!(sym.name.as_str(), "my_bus");
    assert_eq!(sym.kind, lyra_semantic::symbols::SymbolKind::Interface);
}

#[test]
fn cross_file_program_instantiation() {
    let db = LyraDatabase::default();
    let file_a = new_file(&db, 0, "program my_prog; endprogram");
    let file_b = new_file(&db, 1, "module top; my_prog u_prog(); endmodule");
    let unit = new_compilation_unit(&db, vec![file_a, file_b]);

    let text = file_b.text(&db);
    let pos = text.find("my_prog").expect("should find 'my_prog'");
    let result = resolve_at(&db, file_b, unit, lyra_source::TextSize::new(pos as u32));
    assert!(result.is_some(), "'my_prog' should resolve cross-file");
    let sym_id = result.expect("checked");
    assert_eq!(sym_id.file, lyra_source::FileId(0));
    let sym = symbol_global(&db, unit, sym_id).expect("symbol should exist");
    assert_eq!(sym.name.as_str(), "my_prog");
    assert_eq!(sym.kind, lyra_semantic::symbols::SymbolKind::Program);
}

#[test]
fn duplicate_module_interface() {
    let db = LyraDatabase::default();
    let file_a = new_file(&db, 0, "module foo; endmodule");
    let file_b = new_file(&db, 1, "interface foo; endinterface");
    let unit = new_compilation_unit(&db, vec![file_a, file_b]);
    let diags = unit_diagnostics(&db, unit);
    assert!(
        diags
            .iter()
            .any(|d| d.render_message().contains("duplicate definition")),
        "module and interface 'foo' should collide: {diags:?}"
    );
}

#[test]
fn duplicate_interface() {
    let db = LyraDatabase::default();
    let file_a = new_file(&db, 0, "interface bar; endinterface");
    let file_b = new_file(&db, 1, "interface bar; endinterface");
    let unit = new_compilation_unit(&db, vec![file_a, file_b]);
    let diags = unit_diagnostics(&db, unit);
    assert!(
        diags
            .iter()
            .any(|d| d.render_message().contains("duplicate definition")),
        "two interfaces 'bar' should collide: {diags:?}"
    );
}

#[test]
fn all_def_types_in_global_index() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; endmodule\n\
         interface i; endinterface\n\
         program p; endprogram\n\
         primitive u(output y, input a); table 0 : 0; endtable endprimitive\n\
         config c; design m; endconfig",
    );
    let unit = single_file_unit(&db, file);
    let global = global_def_index(&db, unit);
    assert!(global.resolve_definition("m").is_some());
    assert!(global.resolve_definition("i").is_some());
    assert!(global.resolve_definition("p").is_some());
    assert!(global.resolve_definition("u").is_some());
    assert!(global.resolve_definition("c").is_some());
}

#[test]
fn mixed_file_roundtrip() {
    let db = LyraDatabase::default();
    let src = "module m; endmodule\ninterface i; endinterface\nprogram p; endprogram\n";
    let file = new_file(&db, 0, src);
    let parse = parse_file(&db, file);
    assert!(
        parse.errors.is_empty(),
        "no parse errors: {:?}",
        parse.errors
    );
    assert_eq!(parse.syntax().text().to_string(), src);
}
