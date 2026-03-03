use super::*;

#[test]
fn interface_param_default_no_override() {
    let (_, tree) = elab_tree(
        &[
            "interface ifc #(parameter int W = 8)(); endinterface",
            "module top; ifc u1(); endmodule",
        ],
        "top",
    );
    let top_id = tree.top.expect("top should exist");
    assert!(tree.diagnostics.is_empty(), "no diags expected");
    let vals = child_param_values(&tree, top_id, "u1");
    assert_eq!(vals, vec![ConstInt::Known(8)]);
}

#[test]
fn interface_param_named_override() {
    let (_, tree) = elab_tree(
        &[
            "interface ifc #(parameter int W = 8)(); endinterface",
            "module top; ifc #(.W(16)) u1(); endmodule",
        ],
        "top",
    );
    let top_id = tree.top.expect("top should exist");
    assert!(tree.diagnostics.is_empty(), "no diags expected");
    let vals = child_param_values(&tree, top_id, "u1");
    assert_eq!(vals, vec![ConstInt::Known(16)]);
}

#[test]
fn interface_param_positional_override() {
    let (_, tree) = elab_tree(
        &[
            "interface ifc #(parameter int W = 8)(); endinterface",
            "module top; ifc #(16) u1(); endmodule",
        ],
        "top",
    );
    let top_id = tree.top.expect("top should exist");
    assert!(tree.diagnostics.is_empty(), "no diags expected");
    let vals = child_param_values(&tree, top_id, "u1");
    assert_eq!(vals, vec![ConstInt::Known(16)]);
}

#[test]
fn interface_param_unknown_name() {
    let diags = elab_diags(
        &[
            "interface ifc #(parameter int W = 8)(); endinterface",
            "module top; ifc #(.BOGUS(1)) u1(); endmodule",
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
fn interface_param_too_many_positional() {
    let diags = elab_diags(
        &[
            "interface ifc #(parameter int W = 8)(); endinterface",
            "module top; ifc #(1, 2) u1(); endmodule",
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
fn interface_param_duplicate_named() {
    let diags = elab_diags(
        &[
            "interface ifc #(parameter int W = 8)(); endinterface",
            "module top; ifc #(.W(1), .W(2)) u1(); endmodule",
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
fn interface_param_multiple_with_defaults() {
    let (_, tree) = elab_tree(
        &[
            "interface ifc #(parameter int W = 8, parameter int D = 4)(); endinterface",
            "module top; ifc #(.W(16)) u1(); endmodule",
        ],
        "top",
    );
    let top_id = tree.top.expect("top should exist");
    assert!(tree.diagnostics.is_empty(), "no diags expected");
    let vals = child_param_values(&tree, top_id, "u1");
    assert_eq!(vals, vec![ConstInt::Known(16), ConstInt::Known(4)]);
}
