use lyra_semantic::record::PortDirection;
use lyra_semantic::symbols::Namespace;

use super::*;

fn setup_modport_test(
    db: &LyraDatabase,
    src: &str,
) -> (
    SourceFile,
    CompilationUnit,
    lyra_semantic::record::ModportDefId,
) {
    let file = new_file(db, 0, src);
    let unit = single_file_unit(db, file);

    let def = def_index_file(db, file);
    let global = global_def_index(db, unit);

    // Find the interface GlobalDefId
    let iface_def_id = global
        .definitions()
        .iter()
        .find(|(name, _, _)| name.as_str() != "std")
        .map(|(_, def_id, _)| *def_id)
        .expect("should have an interface definition");

    // Find the first modport
    let modport_id = def
        .modport_name_map
        .values()
        .copied()
        .find(|id| id.owner == iface_def_id)
        .expect("should have a modport");

    (file, unit, modport_id)
}

#[test]
fn modport_sem_resolves_members() {
    let db = LyraDatabase::default();
    let src =
        "interface bus; logic req; logic gnt; modport master(input req, output gnt); endinterface";
    let (file, unit, modport_id) = setup_modport_test(&db, src);

    let mref = ModportRef::new(&db, unit, modport_id);
    let sem = modport_sem(&db, mref);

    assert!(
        sem.diags.is_empty(),
        "no diagnostics expected: {:?}",
        sem.diags
    );

    // Resolve req and gnt in the interface scope to get their SymbolIds
    let def = def_index_file(&db, file);
    let gsym = def_symbol(&db, unit, modport_id.owner).expect("interface symbol");
    let iface_scope = def.symbols.get(gsym.local).scope;

    let req_sym = def
        .scopes
        .resolve(&def.symbols, iface_scope, Namespace::Value, "req")
        .expect("req should resolve");
    let gnt_sym = def
        .scopes
        .resolve(&def.symbols, iface_scope, Namespace::Value, "gnt")
        .expect("gnt should resolve");

    assert_eq!(
        sem.view.direction_of(req_sym),
        Some(PortDirection::Input),
        "req should be input"
    );
    assert_eq!(
        sem.view.direction_of(gnt_sym),
        Some(PortDirection::Output),
        "gnt should be output"
    );
}

#[test]
fn modport_sem_unknown_member() {
    let db = LyraDatabase::default();
    let src = "interface bus; logic req; modport master(input nonexistent); endinterface";
    let (_file, unit, modport_id) = setup_modport_test(&db, src);

    let mref = ModportRef::new(&db, unit, modport_id);
    let sem = modport_sem(&db, mref);

    assert_eq!(sem.diags.len(), 1, "should have one diagnostic");
    let diag = &sem.diags[0];
    assert!(
        diag.format().contains("nonexistent"),
        "diagnostic should mention 'nonexistent': {}",
        diag.format()
    );
}

#[test]
fn modport_sem_duplicate_member() {
    let db = LyraDatabase::default();
    let src = "interface bus; logic req; modport master(input req, input req); endinterface";
    let (_file, unit, modport_id) = setup_modport_test(&db, src);

    let mref = ModportRef::new(&db, unit, modport_id);
    let sem = modport_sem(&db, mref);

    assert_eq!(sem.diags.len(), 1, "should have one duplicate diagnostic");
    let diag = &sem.diags[0];
    assert!(
        diag.format().contains("req"),
        "diagnostic should mention 'req': {}",
        diag.format()
    );
    // The `original` span should point to the first occurrence, not the duplicate
    if let lyra_semantic::diagnostic::SemanticDiagKind::DuplicateDefinition { original, .. } =
        &diag.kind
    {
        assert_ne!(
            *original, diag.range,
            "original span should differ from duplicate span"
        );
    } else {
        panic!("expected DuplicateDefinition diagnostic");
    }
}

#[test]
fn def_symbol_resolves_interface() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "interface bus; endinterface");
    let unit = single_file_unit(&db, file);

    let global = global_def_index(&db, unit);
    let iface_def_id = global
        .definitions()
        .iter()
        .find(|(name, _, _)| name == "bus")
        .map(|(_, def_id, _)| *def_id)
        .expect("should find bus");

    let gsym = def_symbol(&db, unit, iface_def_id).expect("should resolve");
    let def = def_index_file(&db, file);
    let sym = def.symbols.get(gsym.local);
    assert_eq!(sym.name.as_str(), "bus");
    assert_eq!(sym.kind, lyra_semantic::symbols::SymbolKind::Interface);
}
