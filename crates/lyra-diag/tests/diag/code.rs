use lyra_diag::code;

#[test]
fn key_is_string() {
    assert_eq!(code::PARSE_ERROR, "lyra.parse.error");
    assert_eq!(code::UNRESOLVED_NAME, "lyra.semantic.unresolved_name");
    assert_eq!(
        code::DUPLICATE_DEFINITION,
        "lyra.semantic.duplicate_definition"
    );
    assert_eq!(code::PREPROCESS_ERROR, "lyra.preprocess.error");
}
