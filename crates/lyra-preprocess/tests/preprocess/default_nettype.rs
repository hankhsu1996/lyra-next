use lyra_preprocess::{
    DefaultNettypeDirective, DefaultNettypeValue, DirectiveEventKind, DirectiveKeyword, MacroEnv,
    PreprocessInputs, preprocess,
};
use lyra_source::FileId;

struct NoIncludes;

impl lyra_preprocess::IncludeProvider for NoIncludes {
    fn resolve(&self, _: &str) -> Option<lyra_preprocess::ResolvedInclude<'_>> {
        None
    }
}

fn pp(text: &str) -> lyra_preprocess::PreprocOutput {
    let tokens = lyra_lexer::lex_with_mode(text, lyra_lexer::LexMode::Preprocess);
    preprocess(&PreprocessInputs {
        file: FileId(0),
        tokens: &tokens,
        text,
        provider: &NoIncludes,
        starting_env: &MacroEnv::empty(),
        macro_recursion_limit: PreprocessInputs::DEFAULT_RECURSION_LIMIT,
    })
}

fn find_default_nettype_event(
    out: &lyra_preprocess::PreprocOutput,
) -> Option<&DefaultNettypeDirective> {
    out.directive_events.iter().find_map(|e| match &e.kind {
        DirectiveEventKind::DefaultNettype(dn) => Some(dn),
        _ => None,
    })
}

fn has_any_default_nettype_event(out: &lyra_preprocess::PreprocOutput) -> bool {
    out.directive_events.iter().any(|e| {
        matches!(
            &e.kind,
            DirectiveEventKind::DefaultNettype(_)
                | DirectiveEventKind::KnownDirective(DirectiveKeyword::DefaultNettype)
        )
    })
}

#[test]
fn all_valid_values() {
    let cases = [
        ("wire", DefaultNettypeValue::Wire),
        ("tri", DefaultNettypeValue::Tri),
        ("tri0", DefaultNettypeValue::Tri0),
        ("tri1", DefaultNettypeValue::Tri1),
        ("wand", DefaultNettypeValue::Wand),
        ("triand", DefaultNettypeValue::Triand),
        ("wor", DefaultNettypeValue::Wor),
        ("trior", DefaultNettypeValue::Trior),
        ("trireg", DefaultNettypeValue::Trireg),
        ("uwire", DefaultNettypeValue::Uwire),
        ("none", DefaultNettypeValue::None),
    ];
    for (text, expected) in cases {
        let src = format!("`default_nettype {text}\n");
        let out = pp(&src);
        let dn = find_default_nettype_event(&out);
        assert!(dn.is_some(), "expected structured event for {text}");
        assert_eq!(dn.unwrap().value, expected, "mismatch for {text}");
        assert!(out.errors.is_empty(), "errors for {text}: {:?}", out.errors);
    }
}

#[test]
fn triand_and_trior_preserved_distinctly() {
    let out = pp("`default_nettype triand\n`default_nettype trior\n");
    let events: Vec<_> = out
        .directive_events
        .iter()
        .filter_map(|e| match &e.kind {
            DirectiveEventKind::DefaultNettype(dn) => Some(dn.value),
            _ => None,
        })
        .collect();
    assert_eq!(
        events,
        vec![DefaultNettypeValue::Triand, DefaultNettypeValue::Trior]
    );
}

#[test]
fn invalid_value_produces_error() {
    let out = pp("`default_nettype foo\n");
    assert!(!has_any_default_nettype_event(&out));
    assert!(!out.errors.is_empty());
    let msg = &out.errors[0].message;
    assert!(msg.contains("invalid value"), "message: {msg}");
    assert!(
        msg.contains("foo"),
        "message should include bad token: {msg}"
    );
}

#[test]
fn missing_value_produces_error() {
    let out = pp("`default_nettype\n");
    assert!(!has_any_default_nettype_event(&out));
    assert!(!out.errors.is_empty());
    assert!(
        out.errors[0].message.contains("missing"),
        "message: {}",
        out.errors[0].message
    );
}

#[test]
fn whitespace_between_keyword_and_value() {
    let out = pp("`default_nettype   wire  \n");
    let dn = find_default_nettype_event(&out);
    assert!(dn.is_some());
    assert_eq!(dn.unwrap().value, DefaultNettypeValue::Wire);
    assert!(out.errors.is_empty());
}

#[test]
fn stripped_from_output() {
    let out = pp("`default_nettype none\nmodule m; endmodule\n");
    assert!(!out.expanded_text.contains("default_nettype"));
    assert!(out.expanded_text.contains("module m; endmodule"));
}

#[test]
fn inactive_conditional_branch() {
    let out = pp("`ifdef NOPE\n`default_nettype none\n`endif\n");
    assert!(!has_any_default_nettype_event(&out));
    assert!(out.errors.is_empty());
}

#[test]
fn expanded_offset_correct() {
    let out = pp("module a; endmodule\n`default_nettype none\n");
    let event = out
        .directive_events
        .iter()
        .find(|e| matches!(&e.kind, DirectiveEventKind::DefaultNettype(_)));
    assert!(event.is_some());
    assert_eq!(
        u32::from(event.unwrap().expanded_offset),
        "module a; endmodule\n".len() as u32,
    );
}

#[test]
fn event_span_covers_keyword_through_argument() {
    let out = pp("`default_nettype none\n");
    let event = out
        .directive_events
        .iter()
        .find(|e| matches!(&e.kind, DirectiveEventKind::DefaultNettype(_)));
    assert!(event.is_some());
    let span = &event.unwrap().span;
    assert_eq!(u32::from(span.range.start()), 0);
    assert_eq!(
        u32::from(span.range.end()),
        "`default_nettype none".len() as u32
    );
}

#[test]
fn resetall_produces_event() {
    let out = pp("`default_nettype none\n`resetall\nmodule m; endmodule\n");
    let events: Vec<_> = out
        .directive_events
        .iter()
        .filter_map(|e| match &e.kind {
            DirectiveEventKind::DefaultNettype(dn) => Some(dn.value),
            _ => None,
        })
        .collect();
    assert_eq!(events, vec![DefaultNettypeValue::None]);
    assert!(out.directive_events.iter().any(|e| matches!(
        &e.kind,
        DirectiveEventKind::KnownDirective(DirectiveKeyword::Resetall)
    )));
}

#[test]
fn trailing_junk_produces_error() {
    let out = pp("`default_nettype wire foo\n");
    assert!(!has_any_default_nettype_event(&out));
    assert!(!out.errors.is_empty());
    assert!(
        out.errors[0].message.contains("unexpected tokens"),
        "message: {}",
        out.errors[0].message
    );
}
