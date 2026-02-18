use lyra_diag::DiagnosticCode;
use smol_str::SmolStr;

use super::*;

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
    let db = LyraDatabase::default();
    let f0 = new_file(&db, 0, "module leaf(input logic a); endmodule");
    let f1 = new_file(
        &db,
        1,
        "module mid(input logic x); leaf l1(.a(x)); endmodule",
    );
    let f2 = new_file(&db, 2, "module top; logic w; mid m1(.x(w)); endmodule");
    let unit = new_compilation_unit(&db, vec![f0, f1, f2]);
    let top = TopModule::new(&db, unit, SmolStr::new("top"));
    let tree = elaborate_top(&db, top);
    assert!(tree.top.is_some());
    // The tree should have at least the mid instance and the leaf instance
    assert!(!tree.nodes.is_empty(), "expected non-empty instance tree");
}

#[test]
fn instance_tree_diamond_legal() {
    // Same module type instantiated in two unrelated places is legal
    let db = LyraDatabase::default();
    let f0 = new_file(&db, 0, "module leaf(input logic a); endmodule");
    let f1 = new_file(
        &db,
        1,
        "module top; logic x, y; leaf u1(.a(x)); leaf u2(.a(y)); endmodule",
    );
    let unit = new_compilation_unit(&db, vec![f0, f1]);
    let top = TopModule::new(&db, unit, SmolStr::new("top"));
    let tree = elaborate_top(&db, top);
    let elab_errs: Vec<_> = tree
        .diagnostics
        .iter()
        .filter(|d| matches!(d, crate::elaboration::ElabDiag::RecursionLimit { .. }))
        .collect();
    assert!(elab_errs.is_empty(), "diamond should not trigger recursion");
}

// Cycle detection test

#[test]
fn cycle_detection() {
    let diags = elab_diags(
        &["module a; b b1(); endmodule", "module b; a a1(); endmodule"],
        "a",
    );
    let recursion: Vec<_> = diags
        .iter()
        .filter(|d| d.code == DiagnosticCode::ELAB_RECURSION_LIMIT)
        .collect();
    assert!(
        !recursion.is_empty(),
        "expected recursion limit diag for cycle"
    );
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
