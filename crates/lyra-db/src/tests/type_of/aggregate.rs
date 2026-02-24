use super::*;

#[test]
fn type_of_typedef_enum() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef enum { A, B, C } abc_t; abc_t x; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let td = get_type(&db, file, unit, "abc_t");
    assert!(
        matches!(td, SymbolType::TypeAlias(Ty::Enum(_))),
        "typedef enum should be TypeAlias(Enum), got {td:?}"
    );
    let var = get_type(&db, file, unit, "x");
    assert!(
        matches!(var, SymbolType::Value(Ty::Enum(_))),
        "variable of typedef enum should be Value(Enum), got {var:?}"
    );
}

#[test]
fn type_of_typedef_struct_packed() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef struct packed { logic [7:0] data; } pkt_t; pkt_t y; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let td = get_type(&db, file, unit, "pkt_t");
    assert!(
        matches!(td, SymbolType::TypeAlias(Ty::Record(_))),
        "typedef struct should be TypeAlias(Record), got {td:?}"
    );
    let var = get_type(&db, file, unit, "y");
    assert!(
        matches!(var, SymbolType::Value(Ty::Record(_))),
        "variable of typedef struct should be Value(Record), got {var:?}"
    );
}

#[test]
fn type_of_inline_enum() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; enum { P, Q } pq; endmodule");
    let unit = single_file_unit(&db, file);
    let var = get_type(&db, file, unit, "pq");
    assert!(
        matches!(var, SymbolType::Value(Ty::Enum(_))),
        "inline enum variable should be Value(Enum), got {var:?}"
    );
}

#[test]
fn type_of_inline_struct() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; struct { int a; int b; } point; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let var = get_type(&db, file, unit, "point");
    assert!(
        matches!(var, SymbolType::Value(Ty::Record(_))),
        "inline struct variable should be Value(Record), got {var:?}"
    );
}

#[test]
fn type_of_enum_array() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef enum { A, B } ab_t; ab_t x [4]; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let var = get_type(&db, file, unit, "x");
    match var {
        SymbolType::Value(Ty::Array { ref dim, .. }) => {
            assert_eq!(*dim, UnpackedDim::Size(ConstInt::Known(4)));
        }
        other => panic!("expected Value(Array), got {other:?}"),
    }
}

#[test]
fn type_of_enum_array_2d_peel() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef enum { A, B } ab_t; ab_t x [2][3]; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let var = get_type(&db, file, unit, "x");
    match var {
        SymbolType::Value(ref ty @ Ty::Array { .. }) => {
            let peeled = ty.peel_unpacked_dim();
            assert!(peeled.is_some(), "should peel outermost dim");
            match peeled {
                Some(Ty::Array { ref dim, .. }) => {
                    assert_eq!(*dim, UnpackedDim::Size(ConstInt::Known(3)));
                }
                other => panic!("after peel, expected Array with dim 3, got {other:?}"),
            }
        }
        other => panic!("expected Value(Array), got {other:?}"),
    }
}

#[test]
fn enum_def_variants_stable() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef enum { X, Y, Z } xyz_t; endmodule",
    );
    let def = def_index_file(&db, file);
    assert_eq!(def.enum_defs.len(), 1);
    let names: Vec<&str> = def.enum_defs[0]
        .members
        .iter()
        .map(|v| v.name.as_str())
        .collect();
    assert_eq!(names, vec!["X", "Y", "Z"]);
}

#[test]
fn struct_def_fields_stable() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef struct packed { logic [7:0] data; logic valid; } pkt_t; endmodule",
    );
    let def = def_index_file(&db, file);
    assert_eq!(def.record_defs.len(), 1);
    let names: Vec<&str> = def.record_defs[0]
        .fields
        .iter()
        .map(|f| f.name.as_str())
        .collect();
    assert_eq!(names, vec!["data", "valid"]);
    assert_eq!(def.record_defs[0].packing, Packing::Packed);
    assert_eq!(def.record_defs[0].kind, RecordKind::Struct);
}

#[test]
fn enum_id_churn_contained() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module a; typedef enum { X } e1; typedef enum { Y } e2; endmodule\n\
         module b; typedef enum { Z } e3; endmodule",
    );
    let def = def_index_file(&db, file);
    assert_eq!(def.enum_defs.len(), 3);
    // Each enum gets a distinct enum_type_site anchored to its EnumType node
    let id0 = def.enum_defs[0].enum_type_site;
    let id1 = def.enum_defs[1].enum_type_site;
    let id2 = def.enum_defs[2].enum_type_site;
    assert_ne!(id0, id1, "enums in same module must have distinct IDs");
    assert_ne!(
        id0, id2,
        "enums in different modules must have distinct IDs"
    );
    assert_ne!(
        id1, id2,
        "enums in different modules must have distinct IDs"
    );
}

#[test]
fn type_of_typedef_union_unpacked() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef union { int i; shortreal f; } num; num x; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let var = get_type(&db, file, unit, "x");
    match var {
        SymbolType::Value(Ty::Record(ref id)) => {
            let def = def_index_file(&db, file);
            let rec = &def.record_defs[0];
            assert_eq!(rec.kind, RecordKind::Union);
            assert_eq!(rec.packing, Packing::Unpacked);
            assert_eq!(id.file(), file.file_id(&db));
        }
        other => panic!("expected Value(Record) for union, got {other:?}"),
    }
}

#[test]
fn type_of_typedef_union_packed() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef union packed { logic [7:0] a; logic [7:0] b; } u_t; u_t x; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let var = get_type(&db, file, unit, "x");
    match var {
        SymbolType::Value(Ty::Record(ref id)) => {
            let def = def_index_file(&db, file);
            let rec = &def.record_defs[0];
            assert_eq!(rec.kind, RecordKind::Union);
            assert_eq!(rec.packing, Packing::Packed);
            assert_eq!(id.file(), file.file_id(&db));
        }
        other => panic!("expected Value(Record) for packed union, got {other:?}"),
    }
}

#[test]
fn type_of_inline_union() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; union { int a; logic [31:0] b; } u; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let var = get_type(&db, file, unit, "u");
    assert!(
        matches!(var, SymbolType::Value(Ty::Record(_))),
        "inline union should be Value(Record), got {var:?}"
    );
}

#[test]
fn union_def_fields_stable() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef union packed { logic [7:0] data; logic [7:0] alt; } u_t; endmodule",
    );
    let def = def_index_file(&db, file);
    assert_eq!(def.record_defs.len(), 1);
    let names: Vec<&str> = def.record_defs[0]
        .fields
        .iter()
        .map(|f| f.name.as_str())
        .collect();
    assert_eq!(names, vec!["data", "alt"]);
    assert_eq!(def.record_defs[0].kind, RecordKind::Union);
    assert_eq!(def.record_defs[0].packing, Packing::Packed);
}

#[test]
fn tagged_union_rejected() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef union tagged { int a; int b; } instr_t; instr_t x; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let var = get_type(&db, file, unit, "x");
    assert_eq!(
        var,
        SymbolType::Value(Ty::Error),
        "tagged union typedef should resolve to Ty::Error"
    );
    let diags = file_diagnostics(&db, file, unit);
    let has_tagged = diags.iter().any(|d| {
        d.render_message()
            .contains("tagged unions are not yet supported")
    });
    assert!(has_tagged, "should emit tagged union diagnostic: {diags:?}");
}

#[test]
fn type_of_enum_member() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef enum { IDLE, RUNNING } state_t; state_t s; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let idle_ty = get_type(&db, file, unit, "IDLE");
    let s_ty = get_type(&db, file, unit, "s");
    match (&idle_ty, &s_ty) {
        (SymbolType::Value(Ty::Enum(id1)), SymbolType::Value(Ty::Enum(id2))) => {
            assert_eq!(id1, id2, "IDLE and s should share the same EnumId");
        }
        _ => panic!("expected Value(Enum) for both, got IDLE={idle_ty:?}, s={s_ty:?}"),
    }
}

#[test]
fn type_of_inline_enum_member() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; enum { A, B } x; endmodule");
    let unit = single_file_unit(&db, file);
    let a_ty = get_type(&db, file, unit, "A");
    assert!(
        matches!(a_ty, SymbolType::Value(Ty::Enum(_))),
        "inline enum member A should be Value(Enum), got {a_ty:?}"
    );
}

#[test]
fn type_of_enum_member_variant_id() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef enum { IDLE, RUNNING } state_t; endmodule",
    );
    let def = def_index_file(&db, file);
    let idle = def
        .symbols
        .iter()
        .find(|(_, s)| s.name == "IDLE")
        .expect("IDLE symbol");
    let running = def
        .symbols
        .iter()
        .find(|(_, s)| s.name == "RUNNING")
        .expect("RUNNING symbol");
    assert!(
        matches!(
            idle.1.origin,
            lyra_semantic::record::SymbolOrigin::EnumVariant(..)
        ),
        "expected EnumVariant origin for IDLE, got {:?}",
        idle.1.origin
    );
    assert!(
        matches!(
            running.1.origin,
            lyra_semantic::record::SymbolOrigin::EnumVariant(..)
        ),
        "expected EnumVariant origin for RUNNING, got {:?}",
        running.1.origin
    );
}

#[test]
fn enum_id_anchored_to_ast_node() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef enum { A, B } e1; typedef enum { C } e2; endmodule",
    );
    let def = def_index_file(&db, file);
    let id_map = ast_id_map(&db, file);
    assert_eq!(def.enum_defs.len(), 2);
    // Each EnumId's ErasedAstId matches the AstIdMap entry for the EnumType node
    let root = parse_file(&db, file).syntax();
    let enum_types: Vec<_> = root
        .descendants()
        .filter(|n| n.kind() == lyra_lexer::SyntaxKind::EnumType)
        .collect();
    assert_eq!(enum_types.len(), 2);
    for (i, node) in enum_types.iter().enumerate() {
        let expected = id_map.erased_ast_id(node).unwrap();
        assert_eq!(
            def.enum_defs[i].enum_type_site, expected,
            "EnumDef[{i}].enum_type_site must match the EnumType node's ErasedAstId"
        );
    }
}

#[test]
fn record_id_anchored_to_ast_node() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef struct packed { logic a; } s1; typedef struct packed { logic b; } s2; endmodule",
    );
    let def = def_index_file(&db, file);
    let id_map = ast_id_map(&db, file);
    assert_eq!(def.record_defs.len(), 2);
    let root = parse_file(&db, file).syntax();
    let struct_types: Vec<_> = root
        .descendants()
        .filter(|n| n.kind() == lyra_lexer::SyntaxKind::StructType)
        .collect();
    assert_eq!(struct_types.len(), 2);
    for (i, node) in struct_types.iter().enumerate() {
        let expected = id_map.erased_ast_id(node).unwrap();
        assert_eq!(
            def.record_defs[i].record_type_site, expected,
            "RecordDef[{i}].record_type_site must match the StructType node's ErasedAstId"
        );
    }
    // Distinct records get distinct IDs
    assert_ne!(
        def.record_defs[0].record_type_site,
        def.record_defs[1].record_type_site
    );
}

#[test]
fn enum_member_name_collision() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m;\n\
         typedef enum { IDLE, RUN } a_t;\n\
         typedef enum { IDLE, STOP } b_t;\n\
         endmodule",
    );
    let unit = single_file_unit(&db, file);
    let diags = file_diagnostics(&db, file, unit);
    let dup_diags: Vec<_> = diags
        .iter()
        .filter(|d| d.render_message().contains("duplicate"))
        .collect();
    assert!(
        !dup_diags.is_empty(),
        "should diagnose duplicate IDLE in value namespace: {diags:?}"
    );
}
