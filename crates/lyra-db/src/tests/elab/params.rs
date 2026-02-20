use super::*;

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
    let top_id = tree.top.expect("top should exist");
    let children = child_instance_names(&tree, top_id);
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
    let top_id = tree.top.expect("top should exist");
    let children = child_instance_names(&tree, top_id);
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
