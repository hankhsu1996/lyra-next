use super::*;

#[test]
fn interface_in_def_entries() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "interface my_if; endinterface");
    let def = def_index_file(&db, file);
    assert_eq!(def.defs_by_name.len(), 1);
    let entry = def.def_entry(def.defs_by_name[0]).expect("def entry");
    assert_eq!(entry.name.as_str(), "my_if");
    assert_eq!(
        entry.kind,
        lyra_semantic::global_index::DefinitionKind::Interface
    );
}

#[test]
fn program_in_def_entries() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "program my_prog; endprogram");
    let def = def_index_file(&db, file);
    assert_eq!(def.defs_by_name.len(), 1);
    let entry = def.def_entry(def.defs_by_name[0]).expect("def entry");
    assert_eq!(entry.name.as_str(), "my_prog");
    assert_eq!(
        entry.kind,
        lyra_semantic::global_index::DefinitionKind::Program
    );
}

#[test]
fn primitive_in_def_entries() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "primitive my_udp(output y, input a); table 0 : 0; endtable endprimitive",
    );
    let def = def_index_file(&db, file);
    assert_eq!(def.defs_by_name.len(), 1);
    let entry = def.def_entry(def.defs_by_name[0]).expect("def entry");
    assert_eq!(entry.name.as_str(), "my_udp");
    assert_eq!(
        entry.kind,
        lyra_semantic::global_index::DefinitionKind::Primitive
    );
}

#[test]
fn config_in_def_entries() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "config my_cfg; design top; endconfig");
    let def = def_index_file(&db, file);
    assert_eq!(def.defs_by_name.len(), 1);
    let entry = def.def_entry(def.defs_by_name[0]).expect("def entry");
    assert_eq!(entry.name.as_str(), "my_cfg");
    assert_eq!(
        entry.kind,
        lyra_semantic::global_index::DefinitionKind::Config
    );
}

#[test]
fn cross_file_interface_instantiation() {
    let db = LyraDatabase::default();
    let file_a = new_file(&db, 0, "interface my_bus; endinterface");
    let file_b = new_file(&db, 1, "module top; my_bus u_bus(); endmodule");
    let unit = new_compilation_unit(&db, vec![file_a, file_b]);

    // Verify the interface appears in the global def index
    let global = global_def_index(&db, unit);
    let def_id = global
        .resolve_definition("my_bus")
        .expect("my_bus should appear in global def index");
    assert_eq!(def_id.0.ast_id().file(), lyra_source::FileId(0));
}

#[test]
fn cross_file_program_instantiation() {
    let db = LyraDatabase::default();
    let file_a = new_file(&db, 0, "program my_prog; endprogram");
    let file_b = new_file(&db, 1, "module top; my_prog u_prog(); endmodule");
    let unit = new_compilation_unit(&db, vec![file_a, file_b]);

    // Verify the program appears in the global def index
    let global = global_def_index(&db, unit);
    let def_id = global
        .resolve_definition("my_prog")
        .expect("my_prog should appear in global def index");
    assert_eq!(def_id.0.ast_id().file(), lyra_source::FileId(0));
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
