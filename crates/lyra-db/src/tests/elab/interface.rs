use lyra_diag::message::render_message;

use super::*;

#[test]
fn interface_instantiation() {
    let (_, tree) = elab_tree(
        &[
            "interface my_bus; logic data; endinterface",
            "module top; my_bus u_bus(); endmodule",
        ],
        "top",
    );
    assert!(
        tree.diagnostics.is_empty(),
        "expected no diagnostics, got: {:?}",
        tree.diagnostics
    );
    let top_id = tree.top.expect("top should exist");
    let children = child_instance_names(&tree, top_id);
    assert_eq!(children, vec!["u_bus"]);
}

#[test]
fn interface_sig_with_ports() {
    let diags = elab_diags(
        &[
            "interface my_bus(input logic clk, input logic rst); endinterface",
            "module top; logic c, r; my_bus u(.clk(c), .rst(r)); endmodule",
        ],
        "top",
    );
    let elab_errs: Vec<_> = diags
        .iter()
        .filter(|d| d.code.namespace == "lyra.elab" && d.severity == lyra_diag::Severity::Error)
        .collect();
    assert!(
        elab_errs.is_empty(),
        "expected no elab errors for interface with ports, got: {elab_errs:?}"
    );
}

#[test]
fn non_instantiable_rejected() {
    let diags = elab_diags(
        &["package pkg; endpackage", "module top; pkg p(); endmodule"],
        "top",
    );
    let not_inst: Vec<_> = diags
        .iter()
        .filter(|d| d.code == DiagnosticCode::NOT_INSTANTIABLE)
        .collect();
    assert_eq!(not_inst.len(), 1);
}

#[test]
fn interface_instantiation_accepted() {
    let (_, tree) = elab_tree(
        &[
            "interface my_bus; endinterface",
            "module top; my_bus u(); endmodule",
        ],
        "top",
    );
    let top_id = tree.top.expect("top should exist");
    let children = child_instance_names(&tree, top_id);
    assert_eq!(children, vec!["u"]);
    let not_inst: Vec<_> = tree
        .diagnostics
        .iter()
        .filter(|d| matches!(d, crate::elaboration::ElabDiag::NotInstantiable { .. }))
        .collect();
    assert!(
        not_inst.is_empty(),
        "interface should not trigger NotInstantiable: {not_inst:?}"
    );
}

#[test]
fn modport_conflict_same_modport_no_error() {
    let src = "\
interface bus;
  logic req, gnt;
  modport master(output req, input gnt);
endinterface
module wants_master(bus.master p); endmodule
module top(bus.master bm);
  wants_master u1(.p(bm));
endmodule";
    let diags = elab_diags(&[src], "top");
    let conflicts: Vec<_> = diags
        .iter()
        .filter(|d| d.code == DiagnosticCode::MODPORT_CONFLICT)
        .collect();
    assert!(
        conflicts.is_empty(),
        "same modport should not conflict: {conflicts:?}"
    );
}

#[test]
fn modport_conflict_different_modport_error() {
    let src = "\
interface bus;
  logic req, gnt;
  modport master(output req, input gnt);
  modport slave(input req, output gnt);
endinterface
module wants_master(bus.master p); endmodule
module top(bus.slave bs);
  wants_master u1(.p(bs));
endmodule";
    let diags = elab_diags(&[src], "top");
    let conflicts: Vec<_> = diags
        .iter()
        .filter(|d| d.code == DiagnosticCode::MODPORT_CONFLICT)
        .collect();
    assert_eq!(
        conflicts.len(),
        1,
        "different modport should produce 1 conflict"
    );
    let msg = render_message(&conflicts[0].message);
    assert!(
        msg.contains("master"),
        "should mention formal modport: {msg}"
    );
    assert!(
        msg.contains("slave"),
        "should mention actual modport: {msg}"
    );
}

#[test]
fn modport_conflict_bare_actual_no_error() {
    let src = "\
interface bus;
  logic req, gnt;
  modport master(output req, input gnt);
endinterface
module wants_master(bus.master p); endmodule
module top(bus b_port);
  wants_master u1(.p(b_port));
endmodule";
    let diags = elab_diags(&[src], "top");
    let conflicts: Vec<_> = diags
        .iter()
        .filter(|d| d.code == DiagnosticCode::MODPORT_CONFLICT)
        .collect();
    assert!(
        conflicts.is_empty(),
        "bare interface should not conflict: {conflicts:?}"
    );
}

#[test]
fn modport_conflict_explicit_selection_error() {
    let src = "\
interface bus;
  logic req, gnt;
  modport master(output req, input gnt);
  modport slave(input req, output gnt);
endinterface
module wants_master(bus.master p); endmodule
module top(bus b_port);
  wants_master u1(.p(b_port.slave));
endmodule";
    let diags = elab_diags(&[src], "top");
    let conflicts: Vec<_> = diags
        .iter()
        .filter(|d| d.code == DiagnosticCode::MODPORT_CONFLICT)
        .collect();
    assert_eq!(
        conflicts.len(),
        1,
        "explicit conflicting modport selection: {conflicts:?}"
    );
}

#[test]
fn modport_conflict_explicit_selection_matching() {
    let src = "\
interface bus;
  logic req, gnt;
  modport master(output req, input gnt);
endinterface
module wants_master(bus.master p); endmodule
module top(bus b_port);
  wants_master u1(.p(b_port.master));
endmodule";
    let diags = elab_diags(&[src], "top");
    let conflicts: Vec<_> = diags
        .iter()
        .filter(|d| d.code == DiagnosticCode::MODPORT_CONFLICT)
        .collect();
    assert!(
        conflicts.is_empty(),
        "matching explicit modport should not conflict: {conflicts:?}"
    );
}

#[test]
fn modport_conflict_data_member_not_modport() {
    let src = "\
interface bus;
  logic req, gnt;
  logic [7:0] data;
  modport master(output req, input gnt);
  modport data_mp(input data);
endinterface
module wants_master(bus.master p); endmodule
module top(bus b_port);
  wants_master u1(.p(b_port.data));
endmodule";
    let diags = elab_diags(&[src], "top");
    let conflicts: Vec<_> = diags
        .iter()
        .filter(|d| d.code == DiagnosticCode::MODPORT_CONFLICT)
        .collect();
    assert!(
        conflicts.is_empty(),
        "data member should not be treated as modport: {conflicts:?}"
    );
}

#[test]
fn modport_conflict_member_shadows_modport_name() {
    let src = "\
interface bus;
  logic req, gnt;
  logic master;
  modport master(output req, input gnt);
endinterface
module wants_master(bus.master p); endmodule
module top(bus b_port);
  wants_master u1(.p(b_port.master));
endmodule";
    let diags = elab_diags(&[src], "top");
    let conflicts: Vec<_> = diags
        .iter()
        .filter(|d| d.code == DiagnosticCode::MODPORT_CONFLICT)
        .collect();
    assert!(
        conflicts.is_empty(),
        "data member 'master' should shadow modport name: {conflicts:?}"
    );
}

#[test]
fn modport_conflict_implicit_named_port() {
    let src = "\
interface bus;
  logic req, gnt;
  modport master(output req, input gnt);
  modport slave(input req, output gnt);
endinterface
module wants_master(bus.master p); endmodule
module top(bus.slave p);
  wants_master u1(.p);
endmodule";
    let diags = elab_diags(&[src], "top");
    let conflicts: Vec<_> = diags
        .iter()
        .filter(|d| d.code == DiagnosticCode::MODPORT_CONFLICT)
        .collect();
    assert_eq!(
        conflicts.len(),
        1,
        "implicit named port should detect modport conflict: {conflicts:?}"
    );
    let msg = render_message(&conflicts[0].message);
    assert!(
        msg.contains("master"),
        "should mention formal modport: {msg}"
    );
    assert!(
        msg.contains("slave"),
        "should mention actual modport: {msg}"
    );
}

#[test]
fn modport_conflict_implicit_named_port_matching() {
    let src = "\
interface bus;
  logic req, gnt;
  modport master(output req, input gnt);
endinterface
module wants_master(bus.master p); endmodule
module top(bus.master p);
  wants_master u1(.p);
endmodule";
    let diags = elab_diags(&[src], "top");
    let conflicts: Vec<_> = diags
        .iter()
        .filter(|d| d.code == DiagnosticCode::MODPORT_CONFLICT)
        .collect();
    assert!(
        conflicts.is_empty(),
        "matching implicit named port should not conflict: {conflicts:?}"
    );
}

#[test]
fn modport_conflict_wildcard_error() {
    let src = "\
interface bus;
  logic req, gnt;
  modport master(output req, input gnt);
  modport slave(input req, output gnt);
endinterface
module wants_master(bus.master p); endmodule
module top(bus.slave p);
  wants_master u1(.*);
endmodule";
    let diags = elab_diags(&[src], "top");
    let conflicts: Vec<_> = diags
        .iter()
        .filter(|d| d.code == DiagnosticCode::MODPORT_CONFLICT)
        .collect();
    assert_eq!(
        conflicts.len(),
        1,
        "wildcard should detect modport conflict: {conflicts:?}"
    );
}

#[test]
fn modport_conflict_wildcard_matching() {
    let src = "\
interface bus;
  logic req, gnt;
  modport master(output req, input gnt);
endinterface
module wants_master(bus.master p); endmodule
module top(bus.master p);
  wants_master u1(.*);
endmodule";
    let diags = elab_diags(&[src], "top");
    let conflicts: Vec<_> = diags
        .iter()
        .filter(|d| d.code == DiagnosticCode::MODPORT_CONFLICT)
        .collect();
    assert!(
        conflicts.is_empty(),
        "matching wildcard should not conflict: {conflicts:?}"
    );
}
