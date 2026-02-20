use super::*;

fn type_diag_warnings(src: &str) -> Vec<lyra_diag::Diagnostic> {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, src);
    let unit = single_file_unit(&db, file);
    type_diagnostics(&db, file, unit)
        .iter()
        .filter(|d| d.severity == lyra_diag::Severity::Warning)
        .cloned()
        .collect()
}

fn diag_message(d: &lyra_diag::Diagnostic) -> String {
    lyra_diag::render_message(&d.message)
}

#[test]
fn extension_continuous_assign_no_diag() {
    let src = "module m; logic [7:0] a; logic [3:0] b; assign a = b; endmodule";
    let diags = type_diag_warnings(src);
    assert!(diags.is_empty(), "extension (4 -> 8 bits) should not warn");
}

#[test]
fn truncation_continuous_assign() {
    let src = "module m; logic [3:0] a; logic [7:0] b; assign a = b; endmodule";
    let diags = type_diag_warnings(src);
    assert_eq!(diags.len(), 1);
    let msg = diag_message(&diags[0]);
    assert!(msg.contains("4-bit"), "msg: {msg}");
    assert!(msg.contains("8-bit"), "msg: {msg}");
    assert_eq!(diags[0].code, lyra_diag::DiagnosticCode::WIDTH_MISMATCH);
}

#[test]
fn width_match_no_diag() {
    let src = "module m; logic [7:0] a, b; assign a = b; endmodule";
    let diags = type_diag_warnings(src);
    assert!(diags.is_empty(), "no warning for matching widths");
}

#[test]
fn width_unknown_no_diag() {
    let src =
        "module m; parameter int W = 8; logic [W-1:0] a; logic [7:0] b; assign a = b; endmodule";
    let diags = type_diag_warnings(src);
    assert!(diags.is_empty(), "no warning when LHS width is unknown");
}

#[test]
fn extension_blocking_assign_no_diag() {
    let src = "module m; logic [7:0] a; logic [3:0] b; always_comb begin a = b; end endmodule";
    let diags = type_diag_warnings(src);
    assert!(diags.is_empty(), "extension (4 -> 8 bits) should not warn");
}

#[test]
fn truncation_blocking_assign() {
    let src = "module m; logic [3:0] a; logic [7:0] b; always_comb begin a = b; end endmodule";
    let diags = type_diag_warnings(src);
    assert_eq!(diags.len(), 1);
    let msg = diag_message(&diags[0]);
    assert!(msg.contains("4-bit"), "msg: {msg}");
    assert!(msg.contains("8-bit"), "msg: {msg}");
}

#[test]
fn truncation_var_init() {
    let src = "module m; logic [7:0] x = 16'hFFFF; endmodule";
    let diags = type_diag_warnings(src);
    assert_eq!(diags.len(), 1);
    let msg = diag_message(&diags[0]);
    assert!(msg.contains("8-bit"), "msg: {msg}");
    assert!(msg.contains("16-bit"), "msg: {msg}");
}

#[test]
fn non_bit_assign_no_diag() {
    let src = "module m; real r; real s; assign r = s; endmodule";
    let diags = type_diag_warnings(src);
    assert!(diags.is_empty(), "no width diagnostic for real types");
}

#[test]
fn concat_lhs_no_diag() {
    let src = "module m; logic [3:0] a; logic [3:0] b; logic [7:0] c; assign {a, b} = c; endmodule";
    let diags = type_diag_warnings(src);
    assert!(diags.is_empty(), "no diagnostic for concat LHS");
}

#[test]
fn unbased_unsized_var_init_no_diag() {
    let src = "module m; logic [7:0] x = '1; endmodule";
    let diags = type_diag_warnings(src);
    assert!(
        diags.is_empty(),
        "no width warning for unbased unsized var init"
    );
}

#[test]
fn unbased_unsized_continuous_assign_no_diag() {
    let src = "module m; logic [7:0] a; assign a = '0; endmodule";
    let diags = type_diag_warnings(src);
    assert!(
        diags.is_empty(),
        "no width warning for unbased unsized continuous assign"
    );
}

#[test]
fn unbased_unsized_blocking_assign_no_diag() {
    let src = "module m; logic [7:0] a; always_comb begin a = 'x; end endmodule";
    let diags = type_diag_warnings(src);
    assert!(
        diags.is_empty(),
        "no width warning for unbased unsized blocking assign"
    );
}

#[test]
fn truncation_has_labels() {
    let src = "module m; logic [3:0] a; logic [7:0] b; assign a = b; endmodule";
    let diags = type_diag_warnings(src);
    assert_eq!(diags.len(), 1);
    // Should have primary + 2 secondary labels
    assert_eq!(diags[0].labels.len(), 3);
    assert_eq!(diags[0].labels[0].kind, lyra_diag::LabelKind::Primary);
    assert_eq!(diags[0].labels[1].kind, lyra_diag::LabelKind::Secondary);
    assert_eq!(diags[0].labels[2].kind, lyra_diag::LabelKind::Secondary);
}

#[test]
fn extension_var_init_no_diag() {
    let src = "module m; logic [15:0] x = 8'hFF; endmodule";
    let diags = type_diag_warnings(src);
    assert!(diags.is_empty(), "extension (8 -> 16 bits) should not warn");
}

#[test]
fn context_determined_literal_no_diag() {
    let src = "module m; logic [15:0] x = '1; endmodule";
    let diags = type_diag_warnings(src);
    assert!(
        diags.is_empty(),
        "context-determined literal expands to fit LHS"
    );
}

#[test]
fn comparison_result_ignores_outer_context() {
    // (a < b) is always 1-bit, even in 16-bit context.
    // This should truncate if assigned to wider target? No, 1 < 16 -> extension, no warn.
    let src = "module m; logic [3:0] a, b; logic [15:0] z; assign z = (a < b); endmodule";
    let diags = type_diag_warnings(src);
    assert!(
        diags.is_empty(),
        "comparison result is 1-bit, extension to 16-bit should not warn"
    );
}

#[test]
fn comparison_truncation_to_narrower() {
    // Assigning 1-bit comparison result to 1-bit target -> no warning
    let src = "module m; logic [3:0] a, b; logic c; assign c = (a < b); endmodule";
    let diags = type_diag_warnings(src);
    assert!(diags.is_empty(), "1-bit result to 1-bit target: no warning");
}
