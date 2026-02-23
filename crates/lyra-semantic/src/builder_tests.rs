use lyra_lexer::SyntaxKind;
use lyra_source::FileId;
use smol_str::SmolStr;

use crate::builder::build_def_index;
use crate::def_index::{ExpectedNs, ImportName};
use crate::diagnostic::SemanticDiagKind;
use crate::name_graph::NameGraph;
use crate::symbols::SymbolKind;

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

    assert_eq!(def.exports.packages.len(), 1);
    let pkg_sym = def.symbols.get(def.exports.packages[0]);
    assert_eq!(pkg_sym.name.as_str(), "pkg");
    assert_eq!(pkg_sym.kind, SymbolKind::Package);

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
        .filter(|u| u.path.as_qualified().is_some())
        .collect();
    assert_eq!(qualified_sites.len(), 1);
    let segs = qualified_sites[0].path.as_qualified().unwrap();
    assert_eq!(segs.len(), 2);
    assert_eq!(segs[0].as_str(), "pkg");
    assert_eq!(segs[1].as_str(), "x");
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
fn def_ast_points_to_expected_node_kind() {
    let src = "module m; logic x; endmodule";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);

    let module_sym = def
        .symbols
        .iter()
        .find(|(_, s)| s.name.as_str() == "m" && s.kind == SymbolKind::Module)
        .map(|(_, s)| s);
    assert!(module_sym.is_some());
    assert_eq!(
        module_sym.expect("checked").def_ast.kind(),
        SyntaxKind::ModuleDecl,
    );

    let var_sym = def
        .symbols
        .iter()
        .find(|(_, s)| s.name.as_str() == "x" && s.kind == SymbolKind::Variable)
        .map(|(_, s)| s);
    assert!(var_sym.is_some());
    assert_eq!(
        var_sym.expect("checked").def_ast.kind(),
        SyntaxKind::VarDecl,
    );
}

#[test]
fn def_ast_port_points_to_port_node() {
    let src = "module m(input logic a, input logic b); endmodule";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);

    let ports: Vec<_> = def
        .symbols
        .iter()
        .filter(|(_, s)| s.kind == SymbolKind::PortAnsi)
        .collect();
    assert_eq!(ports.len(), 2);

    // Each ANSI port has its own Port node, so def_ast should differ
    assert_eq!(ports[0].1.def_ast.kind(), SyntaxKind::Port);
    assert_eq!(ports[1].1.def_ast.kind(), SyntaxKind::Port);
    assert_ne!(
        ports[0].1.def_ast, ports[1].1.def_ast,
        "separate Port nodes should have different def_ast"
    );
}

#[test]
fn def_ast_shared_for_multi_declarator() {
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
        x_sym.def_ast, y_sym.def_ast,
        "multi-declarator symbols should share the same def_ast"
    );
    assert_eq!(x_sym.def_ast.kind(), SyntaxKind::VarDecl);
}

#[test]
fn def_ast_shared_for_multi_instance() {
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
        instances[0].1.def_ast, instances[1].1.def_ast,
        "multi-instance symbols should share the same def_ast"
    );
    assert_eq!(
        instances[0].1.def_ast.kind(),
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
fn name_ast_unique_for_multi_declarator() {
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

    assert_eq!(x.def_ast, y.def_ast, "should share def_ast (VarDecl)");
    assert_ne!(
        x.name_ast, y.name_ast,
        "should have distinct name_ast (Declarator)"
    );
    assert_eq!(x.name_ast.kind(), SyntaxKind::Declarator);
    assert_eq!(y.name_ast.kind(), SyntaxKind::Declarator);
}

#[test]
fn name_ast_unique_for_multi_instance() {
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
        insts[0].1.def_ast, insts[1].1.def_ast,
        "should share def_ast"
    );
    assert_ne!(
        insts[0].1.name_ast, insts[1].1.name_ast,
        "should have distinct name_ast (HierarchicalInstance)"
    );
    assert_eq!(insts[0].1.name_ast.kind(), SyntaxKind::HierarchicalInstance);
    assert_eq!(insts[1].1.name_ast.kind(), SyntaxKind::HierarchicalInstance);
}

#[test]
fn name_ast_equals_def_ast_for_single_name() {
    let src = "module m(input logic a); typedef int t; endmodule";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);

    let module = def
        .symbols
        .iter()
        .find(|(_, s)| s.kind == SymbolKind::Module)
        .map(|(_, s)| s);
    assert_eq!(
        module.expect("module").name_ast,
        module.expect("module").def_ast
    );

    let port = def
        .symbols
        .iter()
        .find(|(_, s)| s.kind == SymbolKind::PortAnsi)
        .map(|(_, s)| s);
    assert_eq!(port.expect("port").name_ast, port.expect("port").def_ast);

    let td = def
        .symbols
        .iter()
        .find(|(_, s)| s.kind == SymbolKind::Typedef)
        .map(|(_, s)| s);
    assert_eq!(td.expect("typedef").name_ast, td.expect("typedef").def_ast);
}

#[test]
fn type_ast_present_for_typed_declarations() {
    let src = "module m(input logic a); logic x; wire [7:0] w; typedef int t; endmodule";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);

    let var = def
        .symbols
        .iter()
        .find(|(_, s)| s.name == "x")
        .map(|(_, s)| s)
        .expect("x");
    assert!(var.type_ast.is_some(), "Variable should have type_ast");
    assert_eq!(var.type_ast.expect("checked").kind(), SyntaxKind::TypeSpec);

    let net = def
        .symbols
        .iter()
        .find(|(_, s)| s.name == "w")
        .map(|(_, s)| s)
        .expect("w");
    assert!(net.type_ast.is_some(), "Net should have type_ast");
    assert_eq!(net.type_ast.expect("checked").kind(), SyntaxKind::TypeSpec);

    let port = def
        .symbols
        .iter()
        .find(|(_, s)| s.kind == SymbolKind::PortAnsi)
        .map(|(_, s)| s);
    assert!(
        port.expect("port").type_ast.is_some(),
        "PortAnsi with explicit type should have type_ast"
    );

    let td = def
        .symbols
        .iter()
        .find(|(_, s)| s.kind == SymbolKind::Typedef)
        .map(|(_, s)| s);
    assert!(
        td.expect("typedef").type_ast.is_some(),
        "Typedef should have type_ast"
    );
}

#[test]
fn type_ast_none_for_untyped() {
    let src = "module m; m u1(); endmodule";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);

    let module = def
        .symbols
        .iter()
        .find(|(_, s)| s.kind == SymbolKind::Module)
        .map(|(_, s)| s);
    assert!(
        module.expect("module").type_ast.is_none(),
        "Module should have no type_ast"
    );

    let inst = def
        .symbols
        .iter()
        .find(|(_, s)| s.kind == SymbolKind::Instance)
        .map(|(_, s)| s);
    assert!(
        inst.expect("instance").type_ast.is_none(),
        "Instance should have no type_ast"
    );
}

#[test]
fn type_ast_present_for_net() {
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
        net.type_ast.is_some(),
        "net should have type_ast after A2 TypeSpec wrapping"
    );
    assert_eq!(net.type_ast.expect("checked").kind(), SyntaxKind::TypeSpec);
}

#[test]
fn type_ast_for_signed_net_with_packed_dims() {
    let src = "module m; wire signed [15:0] s; endmodule";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);

    let net = def
        .symbols
        .iter()
        .find(|(_, s)| s.name == "s")
        .map(|(_, s)| s)
        .expect("s");
    let ts_id = net.type_ast.expect("signed net should have type_ast");
    assert_eq!(ts_id.kind(), SyntaxKind::TypeSpec);
}

#[test]
fn type_ast_for_function_is_return_type() {
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
        func.type_ast.is_some(),
        "Function should have return type_ast"
    );
    assert_eq!(func.type_ast.expect("checked").kind(), SyntaxKind::TypeSpec);
    assert_eq!(func.name_ast, func.def_ast);
}

#[test]
fn type_ast_none_for_task() {
    let src = "module m; task t(); endtask endmodule";
    let (parse, map) = parse_source(src);
    let def = build_def_index(FileId(0), &parse, &map);

    let task = def
        .symbols
        .iter()
        .find(|(_, s)| s.kind == SymbolKind::Task)
        .map(|(_, s)| s);
    assert!(
        task.expect("task").type_ast.is_none(),
        "Task should have no type_ast"
    );
    assert_eq!(task.expect("task").name_ast, task.expect("task").def_ast);
}
