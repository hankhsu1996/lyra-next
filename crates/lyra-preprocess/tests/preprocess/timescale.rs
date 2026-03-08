use lyra_preprocess::{
    DirectiveEventKind, DirectiveKeyword, MacroEnv, PreprocessInputs, TimescaleDirective,
    preprocess,
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
        file_path: "",
    })
}

fn find_timescale_event(out: &lyra_preprocess::PreprocOutput) -> Option<&TimescaleDirective> {
    out.directive_events.iter().find_map(|e| match &e.kind {
        DirectiveEventKind::Timescale(ts) => Some(ts),
        _ => None,
    })
}

fn has_known_timescale(out: &lyra_preprocess::PreprocOutput) -> bool {
    out.directive_events.iter().any(|e| {
        matches!(
            &e.kind,
            DirectiveEventKind::KnownDirective(DirectiveKeyword::Timescale)
        )
    })
}

#[test]
fn basic_valid_directive() {
    let out = pp("`timescale 1ns / 1ps\nmodule top; endmodule\n");
    let ts = find_timescale_event(&out);
    assert!(ts.is_some(), "events: {:?}", out.directive_events);
    let ts = ts.unwrap();
    assert_eq!(ts.unit_text, "1ns");
    assert_eq!(ts.precision_text, "1ps");
    assert!(out.errors.is_empty(), "errors: {:?}", out.errors);
}

#[test]
fn all_supported_units() {
    for (unit, prec) in &[
        ("1s", "1s"),
        ("10ms", "1ms"),
        ("100us", "1us"),
        ("1ns", "1ps"),
        ("10ps", "1ps"),
        ("100fs", "1fs"),
    ] {
        let src = format!("`timescale {unit} / {prec}\n");
        let out = pp(&src);
        let ts = find_timescale_event(&out);
        assert!(ts.is_some(), "unit={unit}, prec={prec}");
        let ts = ts.unwrap();
        assert_eq!(ts.unit_text.as_str(), *unit);
        assert_eq!(ts.precision_text.as_str(), *prec);
    }
}

#[test]
fn whitespace_variants() {
    let out = pp("`timescale   1ns   /   1ps  \nmodule m; endmodule\n");
    let ts = find_timescale_event(&out);
    assert!(ts.is_some());
    let ts = ts.unwrap();
    assert_eq!(ts.unit_text, "1ns");
    assert_eq!(ts.precision_text, "1ps");
}

#[test]
fn no_space_around_slash() {
    let out = pp("`timescale 1ns/1ps\n");
    let ts = find_timescale_event(&out);
    assert!(ts.is_some());
    let ts = ts.unwrap();
    assert_eq!(ts.unit_text, "1ns");
    assert_eq!(ts.precision_text, "1ps");
}

#[test]
fn malformed_missing_precision() {
    let out = pp("`timescale 1ns\n");
    assert!(find_timescale_event(&out).is_none());
    assert!(has_known_timescale(&out));
    assert!(!out.errors.is_empty());
}

#[test]
fn malformed_empty_payload() {
    let out = pp("`timescale\n");
    assert!(find_timescale_event(&out).is_none());
    assert!(has_known_timescale(&out));
    assert!(!out.errors.is_empty());
}

#[test]
fn invalid_magnitude_passes_structural_parse() {
    let out = pp("`timescale 2ns / 1ps\n");
    let ts = find_timescale_event(&out);
    assert!(
        ts.is_some(),
        "structural parse should accept any token shape"
    );
    let ts = ts.unwrap();
    assert_eq!(ts.unit_text, "2ns");
    assert_eq!(ts.precision_text, "1ps");
    assert!(
        out.errors.is_empty(),
        "value validation deferred to DB layer"
    );
}

#[test]
fn inactive_conditional_branch() {
    let out = pp("`ifdef NOPE\n`timescale 1ns / 1ps\n`endif\n");
    assert!(find_timescale_event(&out).is_none());
    assert!(!has_known_timescale(&out));
    assert!(out.errors.is_empty());
}

#[test]
fn multiple_directives_in_file() {
    let out = pp("`timescale 1ns / 1ps\n`timescale 10us / 100ns\n");
    let events: Vec<_> = out
        .directive_events
        .iter()
        .filter_map(|e| match &e.kind {
            DirectiveEventKind::Timescale(ts) => Some(ts),
            _ => None,
        })
        .collect();
    assert_eq!(events.len(), 2);
    assert_eq!(events[0].unit_text, "1ns");
    assert_eq!(events[1].unit_text, "10us");
}

#[test]
fn stripped_from_output() {
    let out = pp("`timescale 1ns / 1ps\nmodule m; endmodule\n");
    assert!(!out.expanded_text.contains("timescale"));
    assert!(out.expanded_text.contains("module m; endmodule"));
}

#[test]
fn expanded_offset_correct() {
    let out = pp("module a; endmodule\n`timescale 1ns / 1ps\n");
    let ts_event = out
        .directive_events
        .iter()
        .find(|e| matches!(&e.kind, DirectiveEventKind::Timescale(_)));
    assert!(ts_event.is_some());
    let ts_event = ts_event.unwrap();
    assert_eq!(
        u32::from(ts_event.expanded_offset),
        "module a; endmodule\n".len() as u32,
    );
}
