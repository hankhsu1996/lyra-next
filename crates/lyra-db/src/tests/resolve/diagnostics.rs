use super::*;

#[test]
fn unresolved_diag() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; assign y = unknown_name; endmodule");
    let unit = single_file_unit(&db, file);
    let diags = file_diagnostics(&db, file, unit);
    let semantic_diags: Vec<_> = diags
        .iter()
        .filter(|d| d.render_message().contains("unresolved"))
        .collect();
    assert!(
        !semantic_diags.is_empty(),
        "should have unresolved name diagnostic"
    );
    assert!(
        semantic_diags
            .iter()
            .any(|d| d.render_message().contains("unknown_name")),
        "diagnostic should mention 'unknown_name': {semantic_diags:?}"
    );
}

#[test]
fn duplicate_diag() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; logic x; logic x; endmodule");
    let unit = single_file_unit(&db, file);
    let diags = file_diagnostics(&db, file, unit);
    let dup_diags: Vec<_> = diags
        .iter()
        .filter(|d| d.render_message().contains("duplicate"))
        .collect();
    assert!(
        !dup_diags.is_empty(),
        "should have duplicate definition diagnostic"
    );
}

#[test]
fn duplicate_diag_has_secondary_label() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; logic x; logic x; endmodule");
    let unit = single_file_unit(&db, file);
    let diags = file_diagnostics(&db, file, unit);
    let dup = diags
        .iter()
        .find(|d| d.code == lyra_diag::DiagnosticCode::DUPLICATE_DEFINITION)
        .expect("should have duplicate diagnostic");
    assert_eq!(dup.labels.len(), 2, "primary + secondary labels");
    assert_eq!(dup.labels[0].kind, lyra_diag::LabelKind::Primary);
    assert_eq!(dup.labels[1].kind, lyra_diag::LabelKind::Secondary);
}

#[test]
fn unresolved_diag_has_code() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; assign y = unknown_name; endmodule");
    let unit = single_file_unit(&db, file);
    let diags = file_diagnostics(&db, file, unit);
    let unresolved = diags
        .iter()
        .find(|d| {
            d.code == lyra_diag::DiagnosticCode::UNRESOLVED_NAME
                && d.render_message().contains("unknown_name")
        })
        .expect("should have unresolved name diagnostic for unknown_name");
    assert_eq!(unresolved.severity, lyra_diag::Severity::Error);
}

#[test]
fn parse_error_has_code() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module top;");
    let unit = single_file_unit(&db, file);
    let diags = file_diagnostics(&db, file, unit);
    assert!(
        diags
            .iter()
            .any(|d| d.code == lyra_diag::DiagnosticCode::PARSE_ERROR),
        "should have parse error diagnostic with PARSE_ERROR code"
    );
}

#[test]
fn undeclared_type_diag() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; unknown_t x; endmodule");
    let unit = single_file_unit(&db, file);
    let diags = file_diagnostics(&db, file, unit);
    let diag = diags
        .iter()
        .find(|d| d.code == lyra_diag::DiagnosticCode::UNDECLARED_TYPE)
        .expect("should have undeclared type diagnostic");
    assert_eq!(diag.severity, lyra_diag::Severity::Error);
    assert!(
        diag.render_message()
            .contains("undeclared type `unknown_t`"),
        "message should mention undeclared type: {}",
        diag.render_message()
    );
}

#[test]
fn undeclared_type_not_value() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; logic [7:0] a; assign a = unknown_name; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let diags = file_diagnostics(&db, file, unit);
    let unresolved = diags
        .iter()
        .find(|d| d.render_message().contains("unknown_name"))
        .expect("should have diagnostic for unknown_name");
    assert_eq!(
        unresolved.code,
        lyra_diag::DiagnosticCode::UNRESOLVED_NAME,
        "value-position name should use UNRESOLVED_NAME, not UNDECLARED_TYPE"
    );
}

#[test]
fn not_a_type_diag() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; int my_var; my_var x; endmodule");
    let unit = single_file_unit(&db, file);
    let diags = file_diagnostics(&db, file, unit);
    let diag = diags
        .iter()
        .find(|d| d.code == lyra_diag::DiagnosticCode::NOT_A_TYPE)
        .expect("should have not-a-type diagnostic");
    assert_eq!(diag.severity, lyra_diag::Severity::Error);
    assert!(
        diag.render_message().contains("`my_var` is not a type"),
        "message: {}",
        diag.render_message()
    );
}

#[test]
fn typedef_resolves_ok() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; typedef logic [7:0] my_type; my_type x; endmodule",
    );
    let unit = single_file_unit(&db, file);
    let diags = file_diagnostics(&db, file, unit);
    let type_diags: Vec<_> = diags
        .iter()
        .filter(|d| {
            d.code == lyra_diag::DiagnosticCode::UNDECLARED_TYPE
                || d.code == lyra_diag::DiagnosticCode::NOT_A_TYPE
        })
        .collect();
    assert!(
        type_diags.is_empty(),
        "typedef should resolve without type errors: {type_diags:?}"
    );
}

#[test]
fn qualified_pkg_not_found() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; assign y = no_pkg::t; endmodule");
    let unit = single_file_unit(&db, file);
    let diags = file_diagnostics(&db, file, unit);
    let diag = diags
        .iter()
        .find(|d| d.render_message().contains("no_pkg"))
        .expect("should have diagnostic mentioning no_pkg");
    assert_eq!(
        diag.code,
        lyra_diag::DiagnosticCode::PACKAGE_NOT_FOUND,
        "should be PACKAGE_NOT_FOUND, not UNDECLARED_TYPE"
    );
}

#[test]
fn qualified_member_not_found() {
    let db = LyraDatabase::default();
    let file_a = new_file(&db, 0, "package pkg; typedef int my_type; endpackage");
    let file_b = new_file(
        &db,
        1,
        "module m; typedef pkg::unknown_t local_t; endmodule",
    );
    let unit = new_compilation_unit(&db, vec![file_a, file_b]);
    let diags = file_diagnostics(&db, file_b, unit);
    let diag = diags
        .iter()
        .find(|d| d.render_message().contains("unknown_t"))
        .expect("should have diagnostic mentioning unknown_t");
    assert_eq!(
        diag.code,
        lyra_diag::DiagnosticCode::MEMBER_NOT_FOUND,
        "should be MEMBER_NOT_FOUND, not UNDECLARED_TYPE"
    );
}
