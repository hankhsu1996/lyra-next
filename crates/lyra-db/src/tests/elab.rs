use lyra_diag::DiagnosticCode;
use lyra_semantic::types::ConstInt;
use smol_str::SmolStr;

use super::*;

use crate::elaboration::{ElabItemKey, ElabTree, InstanceKey, ScopeKey};

fn elab_diags(files: &[&str], top: &str) -> Vec<lyra_diag::Diagnostic> {
    let db = LyraDatabase::default();
    let mut source_files = Vec::new();
    for (i, src) in files.iter().enumerate() {
        source_files.push(new_file(&db, i as u32, src));
    }
    let unit = new_compilation_unit(&db, source_files);
    let top_mod = TopModule::new(&db, unit, SmolStr::new(top));
    elab_diagnostics(&db, top_mod).to_vec()
}

fn elab_tree(files: &[&str], top: &str) -> (LyraDatabase, ElabTree) {
    let db = LyraDatabase::default();
    let mut source_files = Vec::new();
    for (i, src) in files.iter().enumerate() {
        source_files.push(new_file(&db, i as u32, src));
    }
    let unit = new_compilation_unit(&db, source_files);
    let top_mod = TopModule::new(&db, unit, SmolStr::new(top));
    let tree = elaborate_top(&db, top_mod).clone();
    (db, tree)
}

fn sig_ports(src: &str, module_name: &str) -> Vec<String> {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, src);
    let unit = single_file_unit(&db, file);
    let global = crate::semantic::global_def_index(&db, unit);
    let def_id = global
        .resolve_module(module_name)
        .expect("module not found");
    let mref = crate::elab_queries::ModuleRef::new(&db, unit, def_id);
    let sig = module_signature(&db, mref);
    sig.ports
        .iter()
        .map(|p| format!("{}: {}", p.name, p.ty.pretty()))
        .collect()
}

fn child_instance_names(tree: &ElabTree, key: &InstanceKey) -> Vec<String> {
    let node = &tree.nodes[key];
    node.children
        .iter()
        .filter_map(|ck| match ck {
            ElabItemKey::Inst(ik) => Some(tree.nodes[ik].instance_name.to_string()),
            ElabItemKey::GenScope(_) => None,
        })
        .collect()
}

fn all_instance_names_under(tree: &ElabTree, key: &InstanceKey) -> Vec<String> {
    let mut names = Vec::new();
    collect_inst_names(tree, &ElabItemKey::Inst(key.clone()), &mut names);
    names
}

fn collect_inst_names(tree: &ElabTree, item: &ElabItemKey, names: &mut Vec<String>) {
    let children: &[ElabItemKey] = match item {
        ElabItemKey::Inst(ik) => &tree.nodes[ik].children,
        ElabItemKey::GenScope(gk) => match tree.gen_scopes.get(gk) {
            Some(gs) => &gs.children,
            None => return,
        },
    };
    for child in children {
        if let ElabItemKey::Inst(cik) = child {
            names.push(tree.nodes[cik].instance_name.to_string());
        }
        collect_inst_names(tree, child, names);
    }
}

// Module signature tests

#[test]
fn sig_ansi_ports_with_directions() {
    let src =
        "module adder(input logic [7:0] a, input logic [7:0] b, output logic [8:0] sum); endmodule";
    let ports = sig_ports(src, "adder");
    assert_eq!(ports.len(), 3);
    assert!(ports[0].starts_with("a:"), "got: {}", ports[0]);
    assert!(ports[1].starts_with("b:"), "got: {}", ports[1]);
    assert!(ports[2].starts_with("sum:"), "got: {}", ports[2]);
}

#[test]
fn sig_preserves_source_order() {
    let src = "module m(input logic z, input logic a, output logic m_out); endmodule";
    let ports = sig_ports(src, "m");
    assert_eq!(ports[0].split(':').next(), Some("z"));
    assert_eq!(ports[1].split(':').next(), Some("a"));
    assert_eq!(ports[2].split(':').next(), Some("m_out"));
}

#[test]
fn sig_params() {
    let db = LyraDatabase::default();
    let src =
        "module m #(parameter int W = 8, parameter int D = 4)(input logic [W-1:0] a); endmodule";
    let file = new_file(&db, 0, src);
    let unit = single_file_unit(&db, file);
    let global = crate::semantic::global_def_index(&db, unit);
    let def_id = global.resolve_module("m").expect("module not found");
    let mref = crate::elab_queries::ModuleRef::new(&db, unit, def_id);
    let sig = module_signature(&db, mref);
    assert_eq!(sig.params.len(), 2);
    assert_eq!(sig.params[0].name, "W");
    assert_eq!(sig.params[1].name, "D");
    assert!(sig.params[0].has_default);
}

// Named port connection tests

#[test]
fn named_ports_correct() {
    let diags = elab_diags(
        &[
            "module leaf(input logic a, output logic b); endmodule",
            "module top; logic x, y; leaf u1(.a(x), .b(y)); endmodule",
        ],
        "top",
    );
    let elab_diags: Vec<_> = diags
        .iter()
        .filter(|d| d.code.namespace == "lyra.elab")
        .collect();
    assert!(
        elab_diags.is_empty(),
        "expected no elab diags, got: {elab_diags:?}"
    );
}

#[test]
fn named_port_unknown() {
    let diags = elab_diags(
        &[
            "module leaf(input logic a); endmodule",
            "module top; logic x; leaf u1(.a(x), .bogus(x)); endmodule",
        ],
        "top",
    );
    let unknown: Vec<_> = diags
        .iter()
        .filter(|d| d.code == DiagnosticCode::UNKNOWN_PORT)
        .collect();
    assert_eq!(unknown.len(), 1, "expected 1 unknown port diag");
}

#[test]
fn named_port_duplicate() {
    let diags = elab_diags(
        &[
            "module leaf(input logic a); endmodule",
            "module top; logic x; leaf u1(.a(x), .a(x)); endmodule",
        ],
        "top",
    );
    let dups: Vec<_> = diags
        .iter()
        .filter(|d| d.code == DiagnosticCode::DUPLICATE_PORT_CONN)
        .collect();
    assert_eq!(dups.len(), 1, "expected 1 duplicate port diag");
}

// Positional port connection tests

#[test]
fn positional_ports_correct() {
    let diags = elab_diags(
        &[
            "module leaf(input logic a, output logic b); endmodule",
            "module top; logic x, y; leaf u1(x, y); endmodule",
        ],
        "top",
    );
    let elab_diags: Vec<_> = diags
        .iter()
        .filter(|d| d.code.namespace == "lyra.elab")
        .collect();
    assert!(
        elab_diags.is_empty(),
        "expected no elab diags, got: {elab_diags:?}"
    );
}

#[test]
fn positional_too_many() {
    let diags = elab_diags(
        &[
            "module leaf(input logic a); endmodule",
            "module top; logic x, y; leaf u1(x, y); endmodule",
        ],
        "top",
    );
    let too_many: Vec<_> = diags
        .iter()
        .filter(|d| d.code == DiagnosticCode::TOO_MANY_POSITIONAL_PORTS)
        .collect();
    assert_eq!(too_many.len(), 1, "expected 1 too-many diag");
}

// Missing port tests

#[test]
fn missing_port_named() {
    let diags = elab_diags(
        &[
            "module leaf(input logic a, input logic b); endmodule",
            "module top; logic x; leaf u1(.a(x)); endmodule",
        ],
        "top",
    );
    let missing: Vec<_> = diags
        .iter()
        .filter(|d| d.code == DiagnosticCode::MISSING_PORT_CONN)
        .collect();
    assert_eq!(missing.len(), 1, "expected 1 missing port warning");
    assert_eq!(missing[0].severity, lyra_diag::Severity::Warning);
}

#[test]
fn missing_port_positional() {
    let diags = elab_diags(
        &[
            "module leaf(input logic a, input logic b, input logic c); endmodule",
            "module top; logic x; leaf u1(x); endmodule",
        ],
        "top",
    );
    let missing: Vec<_> = diags
        .iter()
        .filter(|d| d.code == DiagnosticCode::MISSING_PORT_CONN)
        .collect();
    assert_eq!(missing.len(), 2, "expected 2 missing port warnings (b, c)");
}

// Unresolved and not-a-module tests

#[test]
fn unresolved_module() {
    let diags = elab_diags(&["module top; nonexistent u1(); endmodule"], "top");
    let unresolved: Vec<_> = diags
        .iter()
        .filter(|d| d.code == DiagnosticCode::UNRESOLVED_MODULE_INST)
        .collect();
    assert_eq!(unresolved.len(), 1);
}

#[test]
fn not_a_module_instantiation() {
    let diags = elab_diags(
        &["package pkg; endpackage", "module top; pkg u1(); endmodule"],
        "top",
    );
    let not_mod: Vec<_> = diags
        .iter()
        .filter(|d| d.code == DiagnosticCode::NOT_A_MODULE)
        .collect();
    assert_eq!(not_mod.len(), 1);
}

// Instance tree tests

#[test]
fn instance_tree_two_levels() {
    let (_, tree) = elab_tree(
        &[
            "module leaf(input logic a); endmodule",
            "module mid(input logic x); leaf l1(.a(x)); endmodule",
            "module top; logic w; mid m1(.x(w)); endmodule",
        ],
        "top",
    );

    let top_key = tree.top.clone().expect("top should exist");
    let top_node = &tree.nodes[&top_key];
    assert_eq!(top_node.instance_name, "top");
    assert!(top_node.parent.is_none(), "top has no parent");

    // top -> m1
    let top_children = child_instance_names(&tree, &top_key);
    assert_eq!(top_children, vec!["m1"]);

    // m1 -> l1
    let m1_key = match &top_node.children[0] {
        ElabItemKey::Inst(k) => k.clone(),
        _ => panic!("expected instance"),
    };
    let m1_node = &tree.nodes[&m1_key];
    assert_eq!(m1_node.parent, Some(ScopeKey::Instance(top_key.clone())));
    let m1_children = child_instance_names(&tree, &m1_key);
    assert_eq!(m1_children, vec!["l1"]);

    // l1 is a leaf
    let l1_key = match &m1_node.children[0] {
        ElabItemKey::Inst(k) => k.clone(),
        _ => panic!("expected instance"),
    };
    let l1_node = &tree.nodes[&l1_key];
    assert_eq!(l1_node.parent, Some(ScopeKey::Instance(m1_key)));
    assert!(l1_node.children.is_empty(), "leaf has no children");

    // 3 nodes total: top, m1, l1
    assert_eq!(tree.nodes.len(), 3);
}

#[test]
fn instance_tree_diamond_legal() {
    let (_, tree) = elab_tree(
        &[
            "module leaf(input logic a); endmodule",
            "module top; logic x, y; leaf u1(.a(x)); leaf u2(.a(y)); endmodule",
        ],
        "top",
    );

    let top_key = tree.top.clone().expect("top should exist");
    let top_children = child_instance_names(&tree, &top_key);
    assert_eq!(top_children, vec!["u1", "u2"]);

    // Both children point back to top
    for child in &tree.nodes[&top_key].children {
        if let ElabItemKey::Inst(ck) = child {
            assert_eq!(
                tree.nodes[ck].parent,
                Some(ScopeKey::Instance(top_key.clone()))
            );
            assert!(tree.nodes[ck].children.is_empty());
        }
    }

    // No recursion diagnostics
    let recursion: Vec<_> = tree
        .diagnostics
        .iter()
        .filter(|d| matches!(d, crate::elaboration::ElabDiag::RecursionLimit { .. }))
        .collect();
    assert!(recursion.is_empty(), "diamond should not trigger recursion");

    // 3 nodes: top + u1 + u2
    assert_eq!(tree.nodes.len(), 3);
}

#[test]
fn instance_tree_multi_instance_statement() {
    let (_, tree) = elab_tree(
        &[
            "module leaf(input logic a); endmodule",
            "module top; logic x, y; leaf u1(.a(x)), u2(.a(y)); endmodule",
        ],
        "top",
    );

    let top_key = tree.top.clone().expect("top should exist");
    let top_children = child_instance_names(&tree, &top_key);
    assert_eq!(
        top_children,
        vec!["u1", "u2"],
        "multi-instance statement produces two distinct children"
    );
    assert_eq!(tree.nodes.len(), 3);
}

// Cycle detection test

#[test]
fn cycle_detection() {
    let (_, tree) = elab_tree(
        &["module a; b b1(); endmodule", "module b; a a1(); endmodule"],
        "a",
    );

    let recursion: Vec<_> = tree
        .diagnostics
        .iter()
        .filter(|d| matches!(d, crate::elaboration::ElabDiag::RecursionLimit { .. }))
        .collect();
    assert!(
        !recursion.is_empty(),
        "expected recursion limit diag for cycle"
    );

    let top_key = tree.top.clone().expect("top should exist");
    let top_children = child_instance_names(&tree, &top_key);
    assert_eq!(top_children, vec!["b1"], "top has one child before cycle");
}

// Multi-file test

#[test]
fn multi_file_instantiation() {
    let diags = elab_diags(
        &[
            "module leaf(input logic [7:0] data); endmodule",
            "module top; logic [7:0] d; leaf u1(.data(d)); endmodule",
        ],
        "top",
    );
    let elab_diags: Vec<_> = diags
        .iter()
        .filter(|d| d.code.namespace == "lyra.elab")
        .collect();
    assert!(
        elab_diags.is_empty(),
        "multi-file should have no elab errors, got: {elab_diags:?}"
    );
}

// Parameter tests

#[test]
fn param_default_no_override() {
    let diags = elab_diags(
        &[
            "module leaf #(parameter int W = 8)(input logic [W-1:0] a); endmodule",
            "module top; logic [7:0] d; leaf u1(.a(d)); endmodule",
        ],
        "top",
    );
    let elab_errs: Vec<_> = diags
        .iter()
        .filter(|d| d.code.namespace == "lyra.elab" && d.severity == lyra_diag::Severity::Error)
        .collect();
    assert!(
        elab_errs.is_empty(),
        "param with default should need no override, got: {elab_errs:?}"
    );
}

#[test]
fn param_positional_override() {
    let (_, tree) = elab_tree(
        &[
            "module leaf #(parameter int W = 8)(); endmodule",
            "module top; leaf #(16) u1(); endmodule",
        ],
        "top",
    );
    let top_key = tree.top.clone().expect("top should exist");
    let children = child_instance_names(&tree, &top_key);
    assert_eq!(children, vec!["u1"]);
    assert!(tree.diagnostics.is_empty(), "no diags expected");
}

#[test]
fn param_named_override() {
    let (_, tree) = elab_tree(
        &[
            "module leaf #(parameter int W = 8)(); endmodule",
            "module top; leaf #(.W(16)) u1(); endmodule",
        ],
        "top",
    );
    let top_key = tree.top.clone().expect("top should exist");
    let children = child_instance_names(&tree, &top_key);
    assert_eq!(children, vec!["u1"]);
    assert!(tree.diagnostics.is_empty(), "no diags expected");
}

#[test]
fn param_unknown_name() {
    let diags = elab_diags(
        &[
            "module leaf #(parameter int W = 8)(); endmodule",
            "module top; leaf #(.BOGUS(16)) u1(); endmodule",
        ],
        "top",
    );
    let unknown: Vec<_> = diags
        .iter()
        .filter(|d| d.code == DiagnosticCode::UNKNOWN_PARAM)
        .collect();
    assert_eq!(unknown.len(), 1, "expected 1 unknown param diag");
}

#[test]
fn param_too_many_positional() {
    let diags = elab_diags(
        &[
            "module leaf #(parameter int W = 8)(); endmodule",
            "module top; leaf #(16, 32) u1(); endmodule",
        ],
        "top",
    );
    let too_many: Vec<_> = diags
        .iter()
        .filter(|d| d.code == DiagnosticCode::TOO_MANY_POSITIONAL_PARAMS)
        .collect();
    assert_eq!(too_many.len(), 1, "expected 1 too-many params diag");
}

#[test]
fn param_duplicate_named() {
    let diags = elab_diags(
        &[
            "module leaf #(parameter int W = 8)(); endmodule",
            "module top; leaf #(.W(16), .W(32)) u1(); endmodule",
        ],
        "top",
    );
    let dups: Vec<_> = diags
        .iter()
        .filter(|d| d.code == DiagnosticCode::DUPLICATE_PARAM_OVERRIDE)
        .collect();
    assert_eq!(dups.len(), 1, "expected 1 duplicate param override diag");
}

#[test]
fn param_non_const_override() {
    let diags = elab_diags(
        &[
            "module leaf #(parameter int W = 8)(); endmodule",
            "module top; logic [3:0] x; leaf #(.W(x)) u1(); endmodule",
        ],
        "top",
    );
    let not_const: Vec<_> = diags
        .iter()
        .filter(|d| d.code == DiagnosticCode::PARAM_NOT_CONST)
        .collect();
    assert_eq!(not_const.len(), 1, "expected 1 param-not-const diag");
}

// Generate-if tests

#[test]
fn generate_if_true_branch() {
    let (_, tree) = elab_tree(
        &[
            "module leaf(); endmodule",
            "module top; if (1) begin leaf u1(); end else begin leaf u2(); end endmodule",
        ],
        "top",
    );
    let top_key = tree.top.clone().expect("top should exist");
    let all = all_instance_names_under(&tree, &top_key);
    assert!(
        all.contains(&"u1".to_string()),
        "true branch should be elaborated: {all:?}"
    );
    assert!(
        !all.contains(&"u2".to_string()),
        "false branch should not be elaborated: {all:?}"
    );
}

#[test]
fn generate_if_false_branch() {
    let (_, tree) = elab_tree(
        &[
            "module leaf(); endmodule",
            "module top; if (0) begin leaf u1(); end else begin leaf u2(); end endmodule",
        ],
        "top",
    );
    let top_key = tree.top.clone().expect("top should exist");
    let all = all_instance_names_under(&tree, &top_key);
    assert!(
        !all.contains(&"u1".to_string()),
        "true branch should not be elaborated: {all:?}"
    );
    assert!(
        all.contains(&"u2".to_string()),
        "false branch should be elaborated: {all:?}"
    );
}

// Generate-for tests

#[test]
fn generate_for_four_iterations() {
    let (_, tree) = elab_tree(
        &[
            "module leaf(); endmodule",
            "module top; for (genvar i = 0; i < 4; i = i + 1) begin leaf u(); end endmodule",
        ],
        "top",
    );
    let top_key = tree.top.clone().expect("top should exist");
    let all = all_instance_names_under(&tree, &top_key);
    assert_eq!(all.len(), 4, "expected 4 instances from for-loop: {all:?}");
}

#[test]
fn generate_for_zero_iterations() {
    let (_, tree) = elab_tree(
        &[
            "module leaf(); endmodule",
            "module top; for (genvar i = 0; i < 0; i = i + 1) begin leaf u(); end endmodule",
        ],
        "top",
    );
    let top_key = tree.top.clone().expect("top should exist");
    let all = all_instance_names_under(&tree, &top_key);
    assert!(
        all.is_empty(),
        "expected 0 instances for zero-iteration loop: {all:?}"
    );
}

// Generate-case tests

#[test]
fn generate_case_match() {
    let (_, tree) = elab_tree(
        &[
            "module leaf_a(); endmodule",
            "module leaf_b(); endmodule",
            "module top; case (1) 0: begin leaf_a u1(); end 1: begin leaf_b u2(); end endcase endmodule",
        ],
        "top",
    );
    let top_key = tree.top.clone().expect("top should exist");
    let all = all_instance_names_under(&tree, &top_key);
    assert!(
        all.contains(&"u2".to_string()),
        "case 1 should match: {all:?}"
    );
    assert!(
        !all.contains(&"u1".to_string()),
        "case 0 should not match: {all:?}"
    );
}

#[test]
fn generate_case_default() {
    let (_, tree) = elab_tree(
        &[
            "module leaf_a(); endmodule",
            "module leaf_b(); endmodule",
            "module top; case (99) 0: begin leaf_a u1(); end default: begin leaf_b u2(); end endcase endmodule",
        ],
        "top",
    );
    let top_key = tree.top.clone().expect("top should exist");
    let all = all_instance_names_under(&tree, &top_key);
    assert!(
        all.contains(&"u2".to_string()),
        "default should match: {all:?}"
    );
    assert!(
        !all.contains(&"u1".to_string()),
        "case 0 should not match: {all:?}"
    );
}

// Param-dependent generate test

#[test]
fn param_dependent_generate_if() {
    let (_, tree) = elab_tree(
        &[
            "module leaf(); endmodule",
            "module inner #(parameter int USE_A = 1)(); if (USE_A) begin leaf u_a(); end else begin leaf u_b(); end endmodule",
            "module top; inner u1(); endmodule",
        ],
        "top",
    );
    let top_key = tree.top.clone().expect("top should exist");
    let all = all_instance_names_under(&tree, &top_key);
    assert!(all.contains(&"u1".to_string()), "inner instance: {all:?}");
    assert!(
        all.contains(&"u_a".to_string()),
        "USE_A=1 default should pick true branch: {all:?}"
    );
}

// Nested generate

#[test]
fn nested_generate_for_inside_if() {
    let (_, tree) = elab_tree(
        &[
            "module leaf(); endmodule",
            "module top; if (1) begin for (genvar i = 0; i < 2; i = i + 1) begin leaf u(); end end endmodule",
        ],
        "top",
    );
    let top_key = tree.top.clone().expect("top should exist");
    let all = all_instance_names_under(&tree, &top_key);
    assert_eq!(
        all.len(),
        2,
        "expected 2 instances from nested for-in-if: {all:?}"
    );
}

// ParamEnv interning tests

fn child_param_values(tree: &ElabTree, parent: &InstanceKey, child_name: &str) -> Vec<ConstInt> {
    let node = &tree.nodes[parent];
    for ck in &node.children {
        if let ElabItemKey::Inst(ik) = ck {
            if tree.nodes[ik].instance_name == child_name {
                let env_id = tree.nodes[ik].param_env;
                return tree.envs.values(env_id).to_vec();
            }
        }
    }
    panic!("child instance '{child_name}' not found");
}

#[test]
fn param_default_sees_override_of_earlier_param() {
    let (_, tree) = elab_tree(
        &[
            "module leaf #(parameter int W = 8, parameter int DEPTH = 1 << W)(); endmodule",
            "module top; leaf #(.W(16)) u1(); endmodule",
        ],
        "top",
    );
    let top_key = tree.top.clone().expect("top should exist");
    let vals = child_param_values(&tree, &top_key, "u1");
    assert_eq!(vals.len(), 2);
    assert_eq!(vals[0], ConstInt::Known(16), "W should be overridden to 16");
    assert_eq!(
        vals[1],
        ConstInt::Known(65536),
        "DEPTH should be 1<<16 = 65536"
    );
}

#[test]
fn param_forward_reference_errors() {
    let (_, tree) = elab_tree(
        &[
            "module leaf #(parameter int A = B, parameter int B = 8)(); endmodule",
            "module top; leaf u1(); endmodule",
        ],
        "top",
    );
    let top_key = tree.top.clone().expect("top should exist");
    let vals = child_param_values(&tree, &top_key, "u1");
    assert_eq!(vals.len(), 2);
    assert!(
        matches!(vals[0], ConstInt::Error(_)),
        "A should be error (forward ref to B), got: {:?}",
        vals[0]
    );
    assert_eq!(vals[1], ConstInt::Known(8), "B should be 8");
}

#[test]
fn param_env_dedup() {
    let (_, tree) = elab_tree(
        &[
            "module leaf #(parameter int W = 8)(); endmodule",
            "module top; leaf u1(); leaf u2(); endmodule",
        ],
        "top",
    );
    let top_key = tree.top.clone().expect("top should exist");
    let node = &tree.nodes[&top_key];
    let ids: Vec<_> = node
        .children
        .iter()
        .filter_map(|ck| match ck {
            ElabItemKey::Inst(ik) => Some(tree.nodes[ik].param_env),
            _ => None,
        })
        .collect();
    assert_eq!(ids.len(), 2);
    assert_eq!(
        ids[0], ids[1],
        "identical param envs should share the same ParamEnvId"
    );
}

// Override resolves instantiator's param

#[test]
fn override_uses_instantiator_param() {
    let (_, tree) = elab_tree(
        &[
            "module inner #(parameter int W = 1)(); endmodule",
            "module outer #(parameter int X = 4)(); inner #(.W(X)) u1(); endmodule",
        ],
        "outer",
    );
    let top_key = tree.top.clone().expect("top should exist");
    let all = all_instance_names_under(&tree, &top_key);
    assert_eq!(all, vec!["u1"]);
    let vals = child_param_values(&tree, &top_key, "u1");
    assert_eq!(vals.len(), 1);
    assert_eq!(
        vals[0],
        ConstInt::Known(4),
        "W should be 4 from outer's X default"
    );
}

#[test]
fn override_uses_localparam() {
    let (_, tree) = elab_tree(
        &[
            "module inner #(parameter int W = 1)(); endmodule",
            "module outer; localparam int Y = 16; inner #(.W(Y)) u1(); endmodule",
        ],
        "outer",
    );
    let top_key = tree.top.clone().expect("top should exist");
    let vals = child_param_values(&tree, &top_key, "u1");
    assert_eq!(vals.len(), 1);
    assert_eq!(
        vals[0],
        ConstInt::Known(16),
        "W should be 16 from localparam Y"
    );
}

// For-loop with param bounds

#[test]
fn for_loop_bound_from_param() {
    let (_, tree) = elab_tree(
        &[
            "module leaf(); endmodule",
            "module top #(parameter int N = 4)(); for (genvar i = 0; i < N; i = i + 1) begin leaf u(); end endmodule",
        ],
        "top",
    );
    let top_key = tree.top.clone().expect("top should exist");
    let all = all_instance_names_under(&tree, &top_key);
    assert_eq!(
        all.len(),
        4,
        "expected 4 instances from for-loop with param N=4: {all:?}"
    );
}

#[test]
fn for_loop_bound_from_param_expression() {
    let (_, tree) = elab_tree(
        &[
            "module leaf(); endmodule",
            "module top #(parameter int N = 4)(); for (genvar i = 0; i < N - 1; i = i + 1) begin leaf u(); end endmodule",
        ],
        "top",
    );
    let top_key = tree.top.clone().expect("top should exist");
    let all = all_instance_names_under(&tree, &top_key);
    assert_eq!(
        all.len(),
        3,
        "expected 3 instances from for-loop with param N-1=3: {all:?}"
    );
}

#[test]
fn for_loop_flipped_condition() {
    let (_, tree) = elab_tree(
        &[
            "module leaf(); endmodule",
            "module top; for (genvar i = 0; 4 > i; i = i + 1) begin leaf u(); end endmodule",
        ],
        "top",
    );
    let top_key = tree.top.clone().expect("top should exist");
    let all = all_instance_names_under(&tree, &top_key);
    assert_eq!(
        all.len(),
        4,
        "expected 4 instances from flipped condition '4 > i': {all:?}"
    );
}

#[test]
fn nested_for_loops_see_both_genvars() {
    let (_, tree) = elab_tree(
        &[
            "module leaf(); endmodule",
            "module top; for (genvar i = 0; i < 2; i = i + 1) begin for (genvar j = 0; j < 3; j = j + 1) begin leaf u(); end end endmodule",
        ],
        "top",
    );
    let top_key = tree.top.clone().expect("top should exist");
    let all = all_instance_names_under(&tree, &top_key);
    assert_eq!(
        all.len(),
        6,
        "expected 2*3=6 instances from nested for-loops: {all:?}"
    );
}

#[test]
fn genvar_dependent_generate_if_inside_for() {
    // if (i) is false for i=0, true for i=1,2
    let (_, tree) = elab_tree(
        &[
            "module leaf_a(); endmodule",
            "module leaf_b(); endmodule",
            "module top; for (genvar i = 0; i < 3; i = i + 1) begin if (i) begin leaf_a u_a(); end else begin leaf_b u_b(); end end endmodule",
        ],
        "top",
    );
    let top_key = tree.top.clone().expect("top should exist");
    let all = all_instance_names_under(&tree, &top_key);
    let a_count = all.iter().filter(|n| *n == "u_a").count();
    let b_count = all.iter().filter(|n| *n == "u_b").count();
    assert_eq!(a_count, 2, "i=1,2 pick true branch: {all:?}");
    assert_eq!(b_count, 1, "i=0 picks false branch: {all:?}");
}
