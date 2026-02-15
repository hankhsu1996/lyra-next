use lyra_diag::DiagnosticCode;

#[test]
fn code_as_str() {
    assert_eq!(DiagnosticCode::PARSE_ERROR.as_str(), "lyra.parse[1]");
    assert_eq!(DiagnosticCode::UNRESOLVED_NAME.as_str(), "lyra.semantic[1]");
    assert_eq!(
        DiagnosticCode::DUPLICATE_DEFINITION.as_str(),
        "lyra.semantic[2]"
    );
    assert_eq!(
        DiagnosticCode::PREPROCESS_ERROR.as_str(),
        "lyra.preprocess[1]"
    );
}

#[test]
fn code_display() {
    assert_eq!(
        format!("{}", DiagnosticCode::UNRESOLVED_NAME),
        "lyra.semantic[1]"
    );
}
