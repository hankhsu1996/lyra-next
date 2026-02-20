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
