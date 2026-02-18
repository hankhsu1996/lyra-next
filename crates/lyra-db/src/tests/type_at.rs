use super::*;

fn type_at_str(src: &str, offset: u32) -> Option<String> {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, src);
    let unit = single_file_unit(&db, file);
    type_at(&db, file, unit, lyra_source::TextSize::new(offset)).map(|r| r.pretty().to_string())
}

fn find_offset(src: &str, needle: &str) -> u32 {
    src.find(needle).expect("needle not found") as u32
}

#[test]
fn type_at_variable() {
    let src = "module m; logic [7:0] x; assign x = 0; endmodule";
    let offset = find_offset(src, "x = 0");
    let result = type_at_str(src, offset);
    assert!(result.is_some());
    // Symbol takes priority -- should be the declared type
    let pretty = result.as_deref().expect("should have result");
    assert!(
        pretty.contains("logic"),
        "expected logic type, got: {pretty}"
    );
}

#[test]
fn type_at_literal() {
    let src = "module m; parameter int P = 42; endmodule";
    let offset = find_offset(src, "42");
    let result = type_at_str(src, offset);
    assert!(result.is_some());
}

#[test]
fn type_at_no_type() {
    let src = "module m; endmodule";
    let offset = find_offset(src, "module");
    let result = type_at_str(src, offset);
    // `module` keyword is not a symbol or expression
    assert!(result.is_none(), "keyword should not have type");
}

#[test]
fn type_at_precedence() {
    // Cursor on identifier inside `x + y` returns Symbol (not Expr for the BinExpr)
    let src = "module m; logic [7:0] x; logic [7:0] y; assign x = x + y; endmodule";
    // Find the second 'x' (in the RHS of assign, before ` + y`)
    let rhs_start = src.find("x + y").expect("needle not found");
    let result = type_at_str(src, rhs_start as u32);
    assert!(result.is_some());
    // Should be Symbol type (NameRef resolves), not expr type of the BinExpr
    let pretty = result.as_deref().expect("should have result");
    assert!(
        pretty.contains("logic"),
        "expected logic type, got: {pretty}"
    );
}

#[test]
fn type_at_includes_unpacked_dims() {
    // Cursor on a use-site NameRef of x (in "x = 0") to get its declared type
    let src = "module m; logic [7:0] x [4]; logic [7:0] y; assign y = x[0]; endmodule";
    let offset = find_offset(src, "x[0]");
    let result = type_at_str(src, offset).expect("should have type");
    assert!(
        result.contains("[4]"),
        "printing must include unpacked dim, got: {result}"
    );
    assert!(
        result.contains("[7:0]"),
        "printing must include packed dim, got: {result}"
    );
}

#[test]
fn type_at_enriched_includes_enum_name_cross_file() {
    let db = LyraDatabase::default();
    let pkg = new_file(
        &db,
        0,
        "package pkg; typedef enum logic [1:0] { A, B, C } color_t; endpackage",
    );
    // Use-site: assign c = A, cursor on "c =" to get enriched type
    let mod_src = "module m; import pkg::*; color_t c; assign c = A; endmodule";
    let mod_file = new_file(&db, 1, mod_src);
    let unit = new_compilation_unit(&db, vec![pkg, mod_file]);
    let offset = mod_src.find("c = A").expect("needle not found") as u32;
    let result =
        type_at(&db, mod_file, unit, lyra_source::TextSize::new(offset)).expect("should have type");
    let pretty = result.pretty();
    assert!(
        pretty.contains("enum") && pretty.contains("color_t"),
        "enriched printing must include enum name, got: {pretty}"
    );
}

#[test]
fn type_at_enriched_cross_file_enum_with_array_dims() {
    let db = LyraDatabase::default();
    let pkg = new_file(
        &db,
        0,
        "package pkg; typedef enum logic [1:0] { R, G, B } color_t; endpackage",
    );
    let mod_src = "module m; import pkg::*; color_t x [2][3]; assign x[0][0] = R; endmodule";
    let mod_file = new_file(&db, 1, mod_src);
    let unit = new_compilation_unit(&db, vec![pkg, mod_file]);
    let offset = mod_src.find("x[0][0]").expect("needle not found") as u32;
    let result =
        type_at(&db, mod_file, unit, lyra_source::TextSize::new(offset)).expect("should have type");
    let pretty = result.pretty();
    assert!(
        pretty.contains("enum") && pretty.contains("color_t"),
        "enriched base must include enum name, got: {pretty}"
    );
    assert!(
        pretty.contains("[2]") && pretty.contains("[3]"),
        "enriched printing must include array dims in correct order, got: {pretty}"
    );
}
