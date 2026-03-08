use lyra_semantic::resolve_index::{ImplicitNetId, ResolvedTarget};

use super::*;

/// Two same-scope `y` references on continuous-assign LHS resolve to the
/// same `ImplicitNetId`, and only one implicit net entity is created.
#[test]
fn same_scope_dedup() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module top; wire a; assign y = a; assign y = a; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let resolve = resolve_index_file(&db, file, unit);

    assert_eq!(
        resolve.implicit_nets.len(),
        1,
        "one implicit net for two same-scope references"
    );

    let net = resolve
        .implicit_nets
        .get(ImplicitNetId(0))
        .expect("net 0 exists");
    assert_eq!(net.name.as_str(), "y");
    assert_eq!(net.net_kind, lyra_semantic::types::NetKind::Wire);

    // Both use-sites of `y` resolve to the same ImplicitNetId.
    let implicit_targets: Vec<_> = resolve
        .resolutions
        .values()
        .filter_map(|r| match &r.target {
            ResolvedTarget::ImplicitNet(id) => Some(*id),
            _ => None,
        })
        .collect();
    assert_eq!(
        implicit_targets.len(),
        2,
        "two use-sites resolve to implicit net"
    );
    assert!(
        implicit_targets.iter().all(|id| *id == ImplicitNetId(0)),
        "both resolve to the same ImplicitNetId"
    );
}

/// When `y` is explicitly declared, it resolves to a Symbol, not an implicit net.
#[test]
fn declared_name_wins() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module top; wire a, y; assign y = a; endmodule");
    let unit = single_file_unit(&db, file);
    let resolve = resolve_index_file(&db, file, unit);

    assert!(
        resolve.implicit_nets.is_empty(),
        "no implicit nets when name is declared"
    );

    // `y` in assign should resolve to a Symbol
    let text = file.text(&db);
    let y_pos = text.find("y = a").expect("find y in assign");
    let result = resolve_at(&db, file, unit, lyra_source::TextSize::new(y_pos as u32));
    assert!(result.is_some(), "declared 'y' should resolve");
    let sym = symbol_global(&db, unit, result.expect("checked")).expect("symbol exists");
    assert_eq!(sym.name.as_str(), "y");
    assert_eq!(sym.kind, lyra_semantic::symbols::SymbolKind::Net);
}

/// Same name in different modules produces distinct implicit net entities.
#[test]
fn distinct_scopes_distinct_nets() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module a; wire x; assign y = x; endmodule \
         module b; wire x; assign y = x; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let resolve = resolve_index_file(&db, file, unit);

    assert_eq!(
        resolve.implicit_nets.len(),
        2,
        "two implicit nets for two different scopes"
    );

    let net0 = resolve.implicit_nets.get(ImplicitNetId(0)).expect("net 0");
    let net1 = resolve.implicit_nets.get(ImplicitNetId(1)).expect("net 1");
    assert_eq!(net0.name.as_str(), "y");
    assert_eq!(net1.name.as_str(), "y");
    assert_ne!(
        net0.owner_scope, net1.owner_scope,
        "different scopes must produce different owner_scope"
    );
}

/// `default_nettype none` produces no implicit nets and emits forbidden diagnostic.
#[test]
fn none_policy_no_implicit_net() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "`default_nettype none\nmodule top; wire a; assign y = a; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let resolve = resolve_index_file(&db, file, unit);

    assert!(
        resolve.implicit_nets.is_empty(),
        "no implicit nets under default_nettype none"
    );

    let has_forbidden = resolve.diagnostics.iter().any(|d| {
        matches!(
            &d.kind,
            lyra_semantic::diagnostic::SemanticDiagKind::ImplicitNetForbidden { .. }
        )
    });
    assert!(has_forbidden, "should emit ImplicitNetForbidden diagnostic");
}
