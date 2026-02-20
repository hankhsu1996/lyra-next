use super::*;

#[test]
fn interface_typed_port() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "interface my_bus; logic data; endinterface module m(my_bus b); endmodule",
    );
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "b");
    match ty {
        SymbolType::Value(Ty::Interface(_)) => {}
        other => panic!("expected Value(Interface), got {other:?}"),
    }
}

#[test]
fn interface_typed_variable() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "interface my_bus; logic data; endinterface module m; my_bus b; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "b");
    match ty {
        SymbolType::Value(Ty::Interface(_)) => {}
        other => panic!("expected Value(Interface), got {other:?}"),
    }
}

#[test]
fn interface_identity_resolves_to_interface_kind() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "interface unique_bus; endinterface module m; unique_bus v; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "v");
    let iface_def = match ty {
        SymbolType::Value(Ty::Interface(ref it)) => it.iface,
        other => panic!("expected Value(Interface), got {other:?}"),
    };
    let global = crate::semantic::global_def_index(&db, unit);
    let actual_kind = global.def_kind(iface_def.global_def());
    assert_eq!(
        actual_kind,
        Some(lyra_semantic::global_index::DefinitionKind::Interface),
        "InterfaceDefId should resolve to DefinitionKind::Interface"
    );
}
