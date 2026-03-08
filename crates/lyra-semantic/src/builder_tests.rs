use lyra_lexer::SyntaxKind;
use lyra_source::FileId;
use smol_str::SmolStr;

use crate::builder::build_def_index;
use crate::def_index::{ExpectedNs, ImportName, NamePath, QualifiedRoot, ScopeOwner};
use crate::diagnostic::SemanticDiagKind;
use crate::name_graph::NameGraph;
use crate::scopes::ScopeId;
use crate::symbols::{GlobalDefId, SymbolKind};
use crate::test_support::ScopeOwnerRole;
use crate::time_scale::TimeUnitsDecl;

fn parse_source(src: &str) -> (lyra_parser::Parse, lyra_ast::AstIdMap) {
    let tokens = lyra_lexer::lex(src);
    let pp = lyra_preprocess::preprocess_identity(FileId(0), &tokens, src);
    let parse = lyra_parser::parse(&pp.tokens, &pp.expanded_text);
    let map = lyra_ast::AstIdMap::from_root(FileId(0), &parse.syntax());
    (parse, map)
}

#[test]
fn use_site_ordering_stable_across_whitespace() {
    let src_a = "module m; logic x; assign x = 0; endmodule";
    let src_b = "module  m;  logic  x;  assign  x  =  0;  endmodule";

    let (parse_a, map_a) = parse_source(src_a);
    let def_a = build_def_index(FileId(0), &parse_a, &map_a);
    let graph_a = NameGraph::from_def_index(&def_a);

    let (parse_b, map_b) = parse_source(src_b);
    let def_b = build_def_index(FileId(0), &parse_b, &map_b);
    let graph_b = NameGraph::from_def_index(&def_b);

    assert_eq!(
        graph_a, graph_b,
        "NameGraph should be equal across whitespace edits"
    );

    assert_ne!(def_a, def_b, "DefIndex should differ (offsets changed)");
}

#[test]
fn package_symbols_collected() {
    let src = "package pkg; logic x; parameter P = 1; endpackage";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);

    assert_eq!(def.defs_by_name.len(), 1);
    let pkg_entry = def.def_entry(def.defs_by_name[0]).expect("def entry");
    assert_eq!(pkg_entry.name.as_str(), "pkg");
    assert_eq!(pkg_entry.kind, crate::global_index::DefinitionKind::Package);

    let has_x = def
        .symbols
        .iter()
        .any(|(_, s)| s.name.as_str() == "x" && s.kind == SymbolKind::Variable);
    assert!(has_x, "variable 'x' should be collected in package");
}

#[test]
fn import_recorded_in_def_index() {
    let src = "module m; import pkg::x; import pkg2::*; endmodule";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);

    assert_eq!(def.imports.len(), 2);
    assert_eq!(def.imports[0].package.as_str(), "pkg");
    assert_eq!(def.imports[0].name, ImportName::Explicit(SmolStr::new("x")));
    assert_eq!(def.imports[1].package.as_str(), "pkg2");
    assert_eq!(def.imports[1].name, ImportName::Wildcard);

    let import_use_sites: Vec<_> = def
        .use_sites
        .iter()
        .filter(|u| {
            u.path
                .as_simple()
                .is_some_and(|s| s == "pkg" || s == "pkg2")
        })
        .collect();
    assert!(
        import_use_sites.is_empty(),
        "imports should not be recorded as use-sites"
    );
}

#[test]
fn qualified_name_use_site() {
    let src = "module m; assign y = pkg::x; endmodule";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);

    let qualified_sites: Vec<_> = def
        .use_sites
        .iter()
        .filter(|u| matches!(u.path, NamePath::Qualified(_)))
        .collect();
    assert_eq!(qualified_sites.len(), 1);
    match &qualified_sites[0].path {
        NamePath::Qualified(qp) => {
            assert!(
                matches!(&qp.root, QualifiedRoot::Package(pkg) if pkg == "pkg"),
                "expected Package root"
            );
            assert_eq!(qp.member.as_str(), "x");
        }
        other @ NamePath::Simple(_) => panic!("expected Package-qualified path, got {other:?}"),
    }
}

#[test]
fn name_graph_includes_imports() {
    let src = "module m; import pkg::x; endmodule";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);
    let graph = NameGraph::from_def_index(&def);

    assert_eq!(graph.imports.len(), 1);
    assert_eq!(graph.imports[0].package.as_str(), "pkg");
}

#[test]
fn typedef_collected_as_type_ns() {
    let src = "module m; typedef logic [7:0] byte_t; endmodule";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);

    let typedef_sym = def
        .symbols
        .iter()
        .find(|(_, s)| s.name.as_str() == "byte_t")
        .map(|(_, s)| s);
    assert!(
        typedef_sym.is_some(),
        "typedef 'byte_t' should be collected"
    );
    assert_eq!(typedef_sym.expect("checked").kind, SymbolKind::Typedef);
}

#[test]
fn typedef_use_site_type_then_value() {
    let src = "module m; typedef my_t other_t; endmodule";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);

    let my_t_use = def
        .use_sites
        .iter()
        .find(|u| u.path.as_simple() == Some("my_t"));
    assert!(my_t_use.is_some(), "'my_t' should be a use-site");
    assert_eq!(
        my_t_use.expect("checked").expected_ns,
        ExpectedNs::TypeThenValue,
        "type-position NameRef should have TypeThenValue"
    );

    let other_sym = def
        .symbols
        .iter()
        .find(|(_, s)| s.name.as_str() == "other_t");
    assert!(other_sym.is_some(), "'other_t' should be a typedef symbol");
}

#[test]
fn same_name_different_namespace_no_duplicate() {
    let src = "module m; logic x; typedef int x; endmodule";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);

    assert!(
        def.diagnostics.is_empty(),
        "value 'x' and type 'x' should not conflict: {:?}",
        def.diagnostics
    );
}

#[test]
fn same_namespace_typedef_duplicate() {
    let src = "module m; typedef int t; typedef logic t; endmodule";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);

    let dup_diags: Vec<_> = def
        .diagnostics
        .iter()
        .filter(|d| matches!(d.kind, SemanticDiagKind::DuplicateDefinition { .. }))
        .collect();
    assert!(
        !dup_diags.is_empty(),
        "two typedefs with same name should produce duplicate diagnostic"
    );
}

#[test]
fn decl_site_points_to_expected_node_kind() {
    let src = "module m; logic x; endmodule";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);

    let module_entry = def
        .def_entries
        .iter()
        .find(|e| e.name.as_str() == "m" && e.kind == crate::global_index::DefinitionKind::Module);
    assert!(module_entry.is_some());
    assert_eq!(
        module_entry.expect("checked").decl_site.kind(),
        SyntaxKind::ModuleDecl,
    );

    let var_sym = def
        .symbols
        .iter()
        .find(|(_, s)| s.name.as_str() == "x" && s.kind == SymbolKind::Variable)
        .map(|(_, s)| s);
    assert!(var_sym.is_some());
    assert_eq!(
        var_sym.expect("checked").decl_site.kind(),
        SyntaxKind::VarDecl,
    );
}

#[test]
fn decl_site_port_points_to_port_node() {
    let src = "module m(input logic a, input logic b); endmodule";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);

    let ports: Vec<_> = def
        .symbols
        .iter()
        .filter(|(_, s)| s.kind == SymbolKind::PortAnsi)
        .collect();
    assert_eq!(ports.len(), 2);

    // Each ANSI port has its own Port node, so decl_site should differ
    assert_eq!(ports[0].1.decl_site.kind(), SyntaxKind::Port);
    assert_eq!(ports[1].1.decl_site.kind(), SyntaxKind::Port);
    assert_ne!(
        ports[0].1.decl_site, ports[1].1.decl_site,
        "separate Port nodes should have different decl_site"
    );
}

#[test]
fn decl_site_shared_for_multi_declarator() {
    let src = "module m; logic x, y; endmodule";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);

    let x_sym = def
        .symbols
        .iter()
        .find(|(_, s)| s.name.as_str() == "x" && s.kind == SymbolKind::Variable)
        .map(|(_, s)| s)
        .expect("x");
    let y_sym = def
        .symbols
        .iter()
        .find(|(_, s)| s.name.as_str() == "y" && s.kind == SymbolKind::Variable)
        .map(|(_, s)| s)
        .expect("y");

    assert_eq!(
        x_sym.decl_site, y_sym.decl_site,
        "multi-declarator symbols should share the same decl_site"
    );
    assert_eq!(x_sym.decl_site.kind(), SyntaxKind::VarDecl);
}

#[test]
fn decl_site_shared_for_multi_instance() {
    let src = "module top; m u1(), u2(); endmodule";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);

    let instances: Vec<_> = def
        .symbols
        .iter()
        .filter(|(_, s)| s.kind == SymbolKind::Instance)
        .collect();
    assert_eq!(instances.len(), 2);
    assert_eq!(
        instances[0].1.decl_site, instances[1].1.decl_site,
        "multi-instance symbols should share the same decl_site"
    );
    assert_eq!(
        instances[0].1.decl_site.kind(),
        SyntaxKind::ModuleInstantiation,
    );
}

#[test]
fn no_internal_errors_for_valid_input() {
    let src = "module m(input logic a); logic x, y; typedef int t; m u1(), u2(); endmodule";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);
    assert!(
        def.internal_errors.is_empty(),
        "valid input should produce no internal errors: {:?}",
        def.internal_errors
    );
}

#[test]
fn name_site_unique_for_multi_declarator() {
    let src = "module m; logic x, y; endmodule";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);

    let x = def
        .symbols
        .iter()
        .find(|(_, s)| s.name == "x")
        .map(|(_, s)| s)
        .expect("x");
    let y = def
        .symbols
        .iter()
        .find(|(_, s)| s.name == "y")
        .map(|(_, s)| s)
        .expect("y");

    assert_eq!(x.decl_site, y.decl_site, "should share decl_site (VarDecl)");
    assert_ne!(
        x.name_site, y.name_site,
        "should have distinct name_site (Declarator)"
    );
    assert_eq!(x.name_site.kind(), SyntaxKind::Declarator);
    assert_eq!(y.name_site.kind(), SyntaxKind::Declarator);
}

#[test]
fn name_site_unique_for_multi_instance() {
    let src = "module top; m u1(), u2(); endmodule";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);

    let insts: Vec<_> = def
        .symbols
        .iter()
        .filter(|(_, s)| s.kind == SymbolKind::Instance)
        .collect();
    assert_eq!(insts.len(), 2);
    assert_eq!(
        insts[0].1.decl_site, insts[1].1.decl_site,
        "should share decl_site"
    );
    assert_ne!(
        insts[0].1.name_site, insts[1].1.name_site,
        "should have distinct name_site (HierarchicalInstance)"
    );
    assert_eq!(
        insts[0].1.name_site.kind(),
        SyntaxKind::HierarchicalInstance
    );
    assert_eq!(
        insts[1].1.name_site.kind(),
        SyntaxKind::HierarchicalInstance
    );
}

#[test]
fn name_site_equals_decl_site_for_single_name() {
    let src = "module m(input logic a); typedef int t; endmodule";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);

    let module = def
        .def_entries
        .iter()
        .find(|e| e.kind == crate::global_index::DefinitionKind::Module);
    assert_eq!(
        module.expect("module").name_site,
        module.expect("module").decl_site
    );

    let port = def
        .symbols
        .iter()
        .find(|(_, s)| s.kind == SymbolKind::PortAnsi)
        .map(|(_, s)| s);
    assert_eq!(port.expect("port").name_site, port.expect("port").decl_site);

    let td = def
        .symbols
        .iter()
        .find(|(_, s)| s.kind == SymbolKind::Typedef)
        .map(|(_, s)| s);
    assert_eq!(
        td.expect("typedef").name_site,
        td.expect("typedef").decl_site
    );
}

#[test]
fn type_site_present_for_typed_declarations() {
    let src = "module m(input logic a); logic x; wire [7:0] w; typedef int t; endmodule";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);

    let var = def
        .symbols
        .iter()
        .find(|(_, s)| s.name == "x")
        .map(|(_, s)| s)
        .expect("x");
    assert!(var.type_site.is_some(), "Variable should have type_site");
    assert_eq!(var.type_site.expect("checked").kind(), SyntaxKind::TypeSpec);

    let net = def
        .symbols
        .iter()
        .find(|(_, s)| s.name == "w")
        .map(|(_, s)| s)
        .expect("w");
    assert!(net.type_site.is_some(), "Net should have type_site");
    assert_eq!(net.type_site.expect("checked").kind(), SyntaxKind::TypeSpec);

    let port = def
        .symbols
        .iter()
        .find(|(_, s)| s.kind == SymbolKind::PortAnsi)
        .map(|(_, s)| s);
    assert!(
        port.expect("port").type_site.is_some(),
        "PortAnsi with explicit type should have type_site"
    );

    let td = def
        .symbols
        .iter()
        .find(|(_, s)| s.kind == SymbolKind::Typedef)
        .map(|(_, s)| s);
    assert!(
        td.expect("typedef").type_site.is_some(),
        "Typedef should have type_site"
    );
}

#[test]
fn type_site_none_for_untyped() {
    let src = "module m; m u1(); endmodule";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);

    assert!(
        def.def_entries
            .iter()
            .any(|e| e.kind == crate::global_index::DefinitionKind::Module),
        "Module should be a def entry (not a symbol)"
    );

    let inst = def
        .symbols
        .iter()
        .find(|(_, s)| s.kind == SymbolKind::Instance)
        .map(|(_, s)| s);
    assert!(
        inst.expect("instance").type_site.is_none(),
        "Instance should have no type_site"
    );
}

#[test]
fn type_site_present_for_net() {
    let src = "module m; wire [7:0] w; endmodule";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);

    let net = def
        .symbols
        .iter()
        .find(|(_, s)| s.name == "w")
        .map(|(_, s)| s)
        .expect("w");
    assert!(
        net.type_site.is_some(),
        "net should have type_site after A2 TypeSpec wrapping"
    );
    assert_eq!(net.type_site.expect("checked").kind(), SyntaxKind::TypeSpec);
}

#[test]
fn type_site_for_signed_net_with_packed_dims() {
    let src = "module m; wire signed [15:0] s; endmodule";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);

    let net = def
        .symbols
        .iter()
        .find(|(_, s)| s.name == "s")
        .map(|(_, s)| s)
        .expect("s");
    let ts_id = net.type_site.expect("signed net should have type_site");
    assert_eq!(ts_id.kind(), SyntaxKind::TypeSpec);
}

#[test]
fn type_site_for_function_is_return_type() {
    let src = "module m; function logic [3:0] f(); endfunction endmodule";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);

    let func = def
        .symbols
        .iter()
        .find(|(_, s)| s.kind == SymbolKind::Function)
        .map(|(_, s)| s);
    assert!(func.is_some(), "should find function symbol");
    let func = func.expect("checked");
    assert!(
        func.type_site.is_some(),
        "Function should have return type_site"
    );
    assert_eq!(
        func.type_site.expect("checked").kind(),
        SyntaxKind::TypeSpec
    );
    assert_eq!(func.name_site, func.decl_site);
}

#[test]
fn type_site_none_for_task() {
    let src = "module m; task t(); endtask endmodule";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);

    let task = def
        .symbols
        .iter()
        .find(|(_, s)| s.kind == SymbolKind::Task)
        .map(|(_, s)| s);
    assert!(
        task.expect("task").type_site.is_none(),
        "Task should have no type_site"
    );
    assert_eq!(task.expect("task").name_site, task.expect("task").decl_site);
}

/// Find all scopes owned by declarations matching a given name and role.
///
/// Uses the production `scope_of_def` / `scope_of_symbol` APIs.
fn find_owned_scopes(
    def: &crate::def_index::DefIndex,
    name: &str,
    role: ScopeOwnerRole,
) -> Vec<ScopeId> {
    let mut results = Vec::new();
    for (def_id, entry) in def.iter_defs() {
        if entry.name == name
            && ScopeOwnerRole::from_definition_kind(entry.kind) == role
            && let Some(scope_id) = def.scope_of_def(def_id)
        {
            results.push(scope_id);
        }
    }
    for (sym_id, sym) in def.symbols.iter() {
        if sym.name == name
            && ScopeOwnerRole::from_symbol_kind(sym.kind) == Some(role)
            && let Some(scope_id) = def.scope_of_symbol(sym_id)
        {
            results.push(scope_id);
        }
    }
    results.sort();
    results.dedup();
    results
}

/// Find the unique scope owned by a declaration with the given name and role.
///
/// Asserts exactly one match. Panics on zero or multiple matches so tests
/// fail loudly when uniqueness assumptions break.
fn find_owner_scope(def: &crate::def_index::DefIndex, name: &str, role: ScopeOwnerRole) -> ScopeId {
    let scopes = find_owned_scopes(def, name, role);
    assert_eq!(
        scopes.len(),
        1,
        "expected exactly 1 scope owned by {role:?} '{name}', found {}",
        scopes.len()
    );
    scopes[0]
}

#[test]
fn timeunit_collected_in_module() {
    let src = "module m; timeunit 100ps; endmodule";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);

    let scope_id = find_owner_scope(&def, "m", ScopeOwnerRole::Module);
    let tu = &def.scope_time_units[&scope_id];
    assert_eq!(tu.decls.len(), 1);
    assert!(
        matches!(&tu.decls[0], TimeUnitsDecl::Timeunit { unit, precision, .. }
        if unit.raw == "100ps" && precision.is_none())
    );
}

#[test]
fn timeprecision_collected_in_module() {
    let src = "module m; timeprecision 1ns; endmodule";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);

    let scope_id = find_owner_scope(&def, "m", ScopeOwnerRole::Module);
    let tu = &def.scope_time_units[&scope_id];
    assert_eq!(tu.decls.len(), 1);
    assert!(
        matches!(&tu.decls[0], TimeUnitsDecl::Timeprecision { precision, .. }
        if precision.raw == "1ns")
    );
}

#[test]
fn timeunit_with_precision_preserves_both() {
    let src = "module m; timeunit 100ps / 10fs; endmodule";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);

    let scope_id = find_owner_scope(&def, "m", ScopeOwnerRole::Module);
    let tu = &def.scope_time_units[&scope_id];
    assert_eq!(tu.decls.len(), 1);
    assert!(
        matches!(&tu.decls[0], TimeUnitsDecl::Timeunit { unit, precision: Some(prec), .. }
        if unit.raw == "100ps" && prec.raw == "10fs")
    );
}

#[test]
fn timeunit_then_timeprecision_preserves_order() {
    let src = "module m; timeunit 100ps; timeprecision 10fs; endmodule";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);

    let scope_id = find_owner_scope(&def, "m", ScopeOwnerRole::Module);
    let tu = &def.scope_time_units[&scope_id];
    assert_eq!(tu.decls.len(), 2);
    assert!(matches!(&tu.decls[0], TimeUnitsDecl::Timeunit { unit, .. } if unit.raw == "100ps"));
    assert!(
        matches!(&tu.decls[1], TimeUnitsDecl::Timeprecision { precision, .. } if precision.raw == "10fs")
    );
}

#[test]
fn timeprecision_then_timeunit_preserves_order() {
    let src = "module m; timeprecision 10fs; timeunit 100ps; endmodule";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);

    let scope_id = find_owner_scope(&def, "m", ScopeOwnerRole::Module);
    let tu = &def.scope_time_units[&scope_id];
    assert_eq!(tu.decls.len(), 2);
    assert!(
        matches!(&tu.decls[0], TimeUnitsDecl::Timeprecision { precision, .. } if precision.raw == "10fs")
    );
    assert!(matches!(&tu.decls[1], TimeUnitsDecl::Timeunit { unit, .. } if unit.raw == "100ps"));
}

#[test]
fn timeunit_in_package() {
    let src = "package p; timeunit 1ns; endpackage";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);

    let scope_id = find_owner_scope(&def, "p", ScopeOwnerRole::Package);
    let tu = &def.scope_time_units[&scope_id];
    assert_eq!(tu.decls.len(), 1);
    assert!(matches!(&tu.decls[0], TimeUnitsDecl::Timeunit { unit, .. } if unit.raw == "1ns"));
}

#[test]
fn timeunit_in_interface() {
    let src = "interface i; timeunit 10ns; endinterface";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);

    let scope_id = find_owner_scope(&def, "i", ScopeOwnerRole::Interface);
    let tu = &def.scope_time_units[&scope_id];
    assert_eq!(tu.decls.len(), 1);
    assert!(matches!(&tu.decls[0], TimeUnitsDecl::Timeunit { unit, .. } if unit.raw == "10ns"));
}

#[test]
fn timeunit_in_program() {
    let src = "program p; timeunit 1us; endprogram";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);

    let scope_id = find_owner_scope(&def, "p", ScopeOwnerRole::Program);
    let tu = &def.scope_time_units[&scope_id];
    assert_eq!(tu.decls.len(), 1);
    assert!(matches!(&tu.decls[0], TimeUnitsDecl::Timeunit { unit, .. } if unit.raw == "1us"));
}

#[test]
fn two_modules_different_names_resolve_independently() {
    let src = "module a; logic x; endmodule module b; logic y; endmodule";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);

    let scope_a = find_owner_scope(&def, "a", ScopeOwnerRole::Module);
    let scope_b = find_owner_scope(&def, "b", ScopeOwnerRole::Module);
    assert_ne!(scope_a, scope_b);
    assert!(
        def.scopes
            .resolve(&def.symbols, scope_a, crate::symbols::Namespace::Value, "x")
            .is_some()
    );
    assert!(
        def.scopes
            .resolve(&def.symbols, scope_b, crate::symbols::Namespace::Value, "y")
            .is_some()
    );
}

#[test]
fn two_callables_different_names_resolve_independently() {
    let src = "module m; function void f(); endfunction task t(); endtask endmodule";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);

    let scope_f = find_owner_scope(&def, "f", ScopeOwnerRole::Function);
    let scope_t = find_owner_scope(&def, "t", ScopeOwnerRole::Task);
    assert_ne!(scope_f, scope_t);
}

#[test]
fn kind_qualification_disambiguates_same_name() {
    let src = "package p; endpackage program p; endprogram";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);

    let pkg_scopes = find_owned_scopes(&def, "p", ScopeOwnerRole::Package);
    let prog_scopes = find_owned_scopes(&def, "p", ScopeOwnerRole::Program);
    assert_eq!(pkg_scopes.len(), 1);
    assert_eq!(prog_scopes.len(), 1);
    assert_ne!(pkg_scopes[0], prog_scopes[0]);
}

#[test]
fn owner_to_scope_reverse_index_container() {
    let src = "module m; endmodule";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);

    let def_id = GlobalDefId::new(FileId(0), 0);
    let scope = def.scope_of_def(def_id);
    assert!(scope.is_some());
    let owner = def.scope_owners.get(&scope.expect("checked"));
    assert_eq!(owner, Some(&ScopeOwner::Def(def_id)));
}

#[test]
fn owner_to_scope_reverse_index_callable() {
    let src = "module m; function void f(); endfunction endmodule";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);

    let (func_sym_id, _func_sym) = def
        .symbols
        .iter()
        .find(|(_, s)| s.kind == SymbolKind::Function && s.name == "f")
        .expect("function symbol");
    let scope = def.scope_of_symbol(func_sym_id);
    assert!(scope.is_some());
    let owner = def.scope_owners.get(&scope.expect("checked"));
    assert_eq!(owner, Some(&ScopeOwner::Symbol(func_sym_id)));
}

#[test]
fn duplicate_name_resolved_by_kind_then_owner() {
    let src = "module top; function void f(); endfunction endmodule module top2; function void f(); endfunction endmodule";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);

    let fn_scopes = find_owned_scopes(&def, "f", ScopeOwnerRole::Function);
    assert_eq!(fn_scopes.len(), 2, "should find both functions named 'f'");
    assert_ne!(fn_scopes[0], fn_scopes[1]);
}

fn make_ctx() -> (lyra_parser::Parse, lyra_ast::AstIdMap) {
    let (parse, map) = parse_source("");
    (parse, map)
}

#[test]
fn register_scope_owner_rejects_conflicting_scope() {
    let (_parse, map) = make_ctx();
    let mut ctx = crate::builder::DefContext::new(FileId(0), &map);
    let scope_a = ctx.scopes.push(crate::scopes::ScopeKind::Module, None);
    let scope_b = ctx.scopes.push(crate::scopes::ScopeKind::Module, None);
    let owner = ScopeOwner::Def(GlobalDefId::new(FileId(0), 0));
    let site = lyra_ast::ErasedAstId::placeholder(FileId(0));
    ctx.register_scope_owner(scope_a, owner, site);
    ctx.register_scope_owner(scope_b, owner, site);
    assert_eq!(
        ctx.internal_errors.len(),
        1,
        "re-registering same owner to different scope should emit internal error"
    );
}

#[test]
fn register_scope_owner_rejects_conflicting_owner() {
    let (_parse, map) = make_ctx();
    let mut ctx = crate::builder::DefContext::new(FileId(0), &map);
    let scope = ctx.scopes.push(crate::scopes::ScopeKind::Module, None);
    let owner_a = ScopeOwner::Def(GlobalDefId::new(FileId(0), 0));
    let owner_b = ScopeOwner::Def(GlobalDefId::new(FileId(1), 0));
    let site = lyra_ast::ErasedAstId::placeholder(FileId(0));
    ctx.register_scope_owner(scope, owner_a, site);
    ctx.register_scope_owner(scope, owner_b, site);
    assert_eq!(
        ctx.internal_errors.len(),
        1,
        "re-registering same scope to different owner should emit internal error"
    );
}

#[test]
fn register_scope_owner_allows_idempotent() {
    let (_parse, map) = make_ctx();
    let mut ctx = crate::builder::DefContext::new(FileId(0), &map);
    let scope = ctx.scopes.push(crate::scopes::ScopeKind::Module, None);
    let owner = ScopeOwner::Def(GlobalDefId::new(FileId(0), 0));
    let site = lyra_ast::ErasedAstId::placeholder(FileId(0));
    ctx.register_scope_owner(scope, owner, site);
    ctx.register_scope_owner(scope, owner, site);
    assert!(
        ctx.internal_errors.is_empty(),
        "re-registering same pair should not emit internal error"
    );
}

#[test]
fn scope_of_def_round_trip() {
    let src = "module m; endmodule package p; endpackage";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);

    for (def_id, entry) in def.iter_defs() {
        let scope = def.scope_of_def(def_id);
        assert!(scope.is_some(), "def '{}' should own a scope", entry.name);
        let owner = def.scope_owners.get(&scope.expect("checked"));
        assert_eq!(owner, Some(&ScopeOwner::Def(def_id)));
    }
}

#[test]
fn scope_of_symbol_round_trip() {
    let src = "module m; function void f(); endfunction task t(); endtask endmodule";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);

    for (sym_id, sym) in def.symbols.iter() {
        if !matches!(sym.kind, SymbolKind::Function | SymbolKind::Task) {
            continue;
        }
        let scope = def.scope_of_symbol(sym_id);
        assert!(
            scope.is_some(),
            "callable '{}' should own a scope",
            sym.name
        );
        let owner = def.scope_owners.get(&scope.expect("checked"));
        assert_eq!(owner, Some(&ScopeOwner::Symbol(sym_id)));
    }
}
