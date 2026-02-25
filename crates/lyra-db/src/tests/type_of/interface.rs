use lyra_semantic::types::InterfaceType;

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

#[test]
fn modport_qualified_port() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "interface my_bus; logic data; modport master(input data); endinterface \
         module m(my_bus.master b); endmodule",
    );
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "b");
    let iface_ty = match ty {
        SymbolType::Value(Ty::Interface(ref it)) => it,
        other => panic!("expected Value(Interface), got {other:?}"),
    };
    assert!(
        iface_ty.modport.is_some(),
        "modport should be Some for my_bus.master"
    );
}

#[test]
fn modport_qualified_variable() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "interface my_bus; logic data; modport master(input data); endinterface \
         module m; my_bus.master v; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "v");
    let iface_ty = match ty {
        SymbolType::Value(Ty::Interface(ref it)) => it,
        other => panic!("expected Value(Interface), got {other:?}"),
    };
    assert!(
        iface_ty.modport.is_some(),
        "modport should be Some for my_bus.master"
    );
}

#[test]
fn modport_qualified_preserves_iface_identity() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "interface my_bus; logic data; modport master(input data); endinterface \
         module m(my_bus b_plain, my_bus.master b_mp); endmodule",
    );
    let unit = single_file_unit(&db, file);
    let plain = get_type(&db, file, unit, "b_plain");
    let mp = get_type(&db, file, unit, "b_mp");
    let plain_iface = match plain {
        SymbolType::Value(Ty::Interface(InterfaceType { iface, modport })) => {
            assert!(modport.is_none(), "plain should have no modport");
            iface
        }
        other => panic!("expected Value(Interface), got {other:?}"),
    };
    let mp_iface = match mp {
        SymbolType::Value(Ty::Interface(InterfaceType { iface, modport })) => {
            assert!(modport.is_some(), "qualified should have modport");
            iface
        }
        other => panic!("expected Value(Interface), got {other:?}"),
    };
    assert_eq!(
        plain_iface, mp_iface,
        "both should reference the same InterfaceDefId"
    );
}

#[test]
fn unknown_modport_error() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "interface my_bus; logic data; endinterface \
         module m; my_bus.nonexistent v; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "v");
    assert_eq!(
        ty,
        SymbolType::Error(SymbolTypeError::UnknownModport),
        "should get UnknownModport for nonexistent modport"
    );
}

#[test]
fn modport_on_non_interface_error() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef int foo_t; foo_t.bar v; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "v");
    assert_eq!(
        ty,
        SymbolType::Error(SymbolTypeError::ModportOnNonInterface),
        "dotted name on non-interface should give ModportOnNonInterface"
    );
}

#[test]
fn cross_file_modport_qualified() {
    let db = LyraDatabase::default();
    let file_a = new_file(
        &db,
        0,
        "interface my_bus; logic data; modport master(input data); endinterface",
    );
    let file_b = new_file(&db, 1, "module m; my_bus.master v; endmodule");
    let unit = new_compilation_unit(&db, vec![file_a, file_b]);
    let ty = get_type(&db, file_b, unit, "v");
    let iface_ty = match ty {
        SymbolType::Value(Ty::Interface(ref it)) => it,
        other => panic!("expected Value(Interface), got {other:?}"),
    };
    assert!(
        iface_ty.modport.is_some(),
        "cross-file modport should resolve"
    );
}

#[test]
fn interface_instance_type() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "interface my_bus; endinterface module m; my_bus sb(); endmodule",
    );
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "sb");
    match ty {
        SymbolType::Value(Ty::Interface(_)) => {}
        other => panic!("expected Value(Interface), got {other:?}"),
    }
}

#[test]
fn module_instance_not_interface_type() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module child; endmodule module top; child u(); endmodule",
    );
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "u");
    match ty {
        SymbolType::Error(SymbolTypeError::UnsupportedSymbolKind) => {}
        other => panic!("expected Error(UnsupportedSymbolKind), got {other:?}"),
    }
}

#[test]
fn interface_variable_with_unpacked_dims() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "interface intf; endinterface module m; intf a[2]; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "a");
    match ty {
        SymbolType::Value(Ty::Array { ref elem, .. }) => match elem.as_ref() {
            Ty::Interface(_) => {}
            other => panic!("expected Array(Interface), got Array({other:?})"),
        },
        other => panic!("expected Value(Array{{Interface, ..}}), got {other:?}"),
    }
}

#[test]
fn port_with_keyword_type_still_works() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m(input logic a); endmodule");
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "a");
    assert_eq!(
        ty,
        SymbolType::Value(Ty::simple_logic()),
        "keyword type port should still work"
    );
}
