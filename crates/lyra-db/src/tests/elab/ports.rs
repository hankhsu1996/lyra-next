use super::*;

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
