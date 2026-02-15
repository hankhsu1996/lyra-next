use lyra_diag::{Arg, Diagnostic, DiagnosticCode, Label, LabelKind, Message, MessageId, Severity};
use lyra_preprocess::PreprocOutput;
use lyra_semantic::def_index::DefIndex;
use lyra_semantic::diagnostic::SemanticDiagKind;
use lyra_semantic::resolve_index::ResolveIndex;
use lyra_source::{FileId, Span, TextRange, TextSize};
use smol_str::SmolStr;

/// Convert parse, preprocess, and semantic errors into structured diagnostics.
pub(crate) fn lower_file_diagnostics(
    file_id: FileId,
    pp: &PreprocOutput,
    parse: &lyra_parser::Parse,
    def: &DefIndex,
    resolve: &ResolveIndex,
) -> Vec<Diagnostic> {
    let mut diags = Vec::new();

    // Preprocess errors
    for e in &pp.errors {
        let (span, extra) = map_span_or_fallback(file_id, &pp.source_map, e.range);
        let text = freeform_text(&e.message, extra);
        diags.push(
            Diagnostic::new(
                Severity::Error,
                DiagnosticCode::PREPROCESS_ERROR,
                Message::new(MessageId::PreprocessError, vec![Arg::Name(text.clone())]),
            )
            .with_label(Label {
                kind: LabelKind::Primary,
                span,
                message: Message::new(MessageId::PreprocessError, vec![Arg::Name(text)]),
            }),
        );
    }

    // Parse errors
    for e in &parse.errors {
        let (span, extra) = map_span_or_fallback(file_id, &pp.source_map, e.range);
        let text = freeform_text(&e.message, extra);
        diags.push(
            Diagnostic::new(
                Severity::Error,
                DiagnosticCode::PARSE_ERROR,
                Message::new(MessageId::ParseError, vec![Arg::Name(text.clone())]),
            )
            .with_label(Label {
                kind: LabelKind::Primary,
                span,
                message: Message::new(MessageId::ParseError, vec![Arg::Name(text)]),
            }),
        );
    }

    // Semantic diagnostics
    for diag in def.diagnostics.iter().chain(resolve.diagnostics.iter()) {
        let (primary_span, _) = map_span_or_fallback(file_id, &pp.source_map, diag.range);
        match &diag.kind {
            SemanticDiagKind::UnresolvedName { name } => {
                diags.push(
                    Diagnostic::new(
                        Severity::Error,
                        DiagnosticCode::UNRESOLVED_NAME,
                        Message::new(MessageId::UnresolvedName, vec![Arg::Name(name.clone())]),
                    )
                    .with_label(Label {
                        kind: LabelKind::Primary,
                        span: primary_span,
                        message: Message::simple(MessageId::NotFoundInScope),
                    }),
                );
            }
            SemanticDiagKind::DuplicateDefinition { name, original } => {
                let mut d = Diagnostic::new(
                    Severity::Error,
                    DiagnosticCode::DUPLICATE_DEFINITION,
                    Message::new(
                        MessageId::DuplicateDefinition,
                        vec![Arg::Name(name.clone())],
                    ),
                )
                .with_label(Label {
                    kind: LabelKind::Primary,
                    span: primary_span,
                    message: Message::simple(MessageId::RedefinedHere),
                });
                if let Some(orig_span) = pp.source_map.map_span(*original) {
                    d = d.with_label(Label {
                        kind: LabelKind::Secondary,
                        span: orig_span,
                        message: Message::simple(MessageId::FirstDefinedHere),
                    });
                }
                diags.push(d);
            }
        }
    }

    diags
}

/// Map an expanded-text range to a `Span` via the source map.
/// Returns the mapped span and an optional annotation suffix for
/// unmappable ranges.
fn map_span_or_fallback(
    file_id: FileId,
    source_map: &lyra_preprocess::SourceMap,
    range: TextRange,
) -> (Span, Option<&'static str>) {
    if let Some(span) = source_map.map_span(range) {
        (span, None)
    } else {
        let span = Span {
            file: file_id,
            range: TextRange::empty(TextSize::new(0)),
        };
        (span, Some(" [unmapped]"))
    }
}

/// Build a `SmolStr` from a freeform error message, optionally appending
/// an unmapped-range annotation.
fn freeform_text(message: &str, extra: Option<&str>) -> SmolStr {
    match extra {
        Some(suffix) => SmolStr::new(format!("{message}{suffix}")),
        None => SmolStr::new(message),
    }
}
