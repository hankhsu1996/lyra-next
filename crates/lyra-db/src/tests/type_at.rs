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
