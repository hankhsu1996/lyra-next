use lyra_diag::{Arg, Diagnostic, DiagnosticCode, Label, LabelKind, Message, MessageId, Severity};
use lyra_preprocess::PreprocOutput;
use lyra_semantic::def_index::DefIndex;
use lyra_semantic::diagnostic::{SemanticDiag, SemanticDiagKind};
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

    for diag in def.diagnostics.iter().chain(resolve.diagnostics.iter()) {
        let (primary_span, _) = map_span_or_fallback(file_id, &pp.source_map, diag.range);
        diags.push(lower_semantic_diag(diag, primary_span, &pp.source_map));
    }

    diags
}

fn lower_semantic_diag(
    diag: &SemanticDiag,
    primary_span: Span,
    source_map: &lyra_preprocess::SourceMap,
) -> Diagnostic {
    match &diag.kind {
        SemanticDiagKind::UnresolvedName { name } => lower_name_diag(
            DiagnosticCode::UNRESOLVED_NAME,
            MessageId::UnresolvedName,
            MessageId::NotFoundInScope,
            name,
            primary_span,
        ),
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
            if let Some(orig_span) = source_map.map_span(*original) {
                d = d.with_label(Label {
                    kind: LabelKind::Secondary,
                    span: orig_span,
                    message: Message::simple(MessageId::FirstDefinedHere),
                });
            }
            d
        }
        SemanticDiagKind::PackageNotFound { package } => lower_args_diag(
            DiagnosticCode::PACKAGE_NOT_FOUND,
            MessageId::PackageNotFound,
            vec![Arg::Name(package.clone())],
            primary_span,
        ),
        SemanticDiagKind::MemberNotFound { package, member } => lower_args_diag(
            DiagnosticCode::MEMBER_NOT_FOUND,
            MessageId::MemberNotFound,
            vec![Arg::Name(package.clone()), Arg::Name(member.clone())],
            primary_span,
        ),
        SemanticDiagKind::AmbiguousWildcardImport { name, candidates } => {
            let pkgs = candidates.join("`, `");
            Diagnostic::new(
                Severity::Error,
                DiagnosticCode::AMBIGUOUS_IMPORT,
                Message::new(
                    MessageId::AmbiguousWildcardImport,
                    vec![
                        Arg::Name(name.clone()),
                        Arg::Name(SmolStr::new(format!("`{pkgs}`"))),
                    ],
                ),
            )
            .with_label(Label {
                kind: LabelKind::Primary,
                span: primary_span,
                message: Message::simple(MessageId::AmbiguousWildcardImport),
            })
        }
        SemanticDiagKind::UnsupportedQualifiedPath { path } => lower_args_diag(
            DiagnosticCode::UNSUPPORTED_QUALIFIED_PATH,
            MessageId::UnsupportedQualifiedPath,
            vec![Arg::Name(path.clone())],
            primary_span,
        ),
        SemanticDiagKind::UndeclaredType { name } => lower_name_diag(
            DiagnosticCode::UNDECLARED_TYPE,
            MessageId::UndeclaredType,
            MessageId::NotFoundAsType,
            name,
            primary_span,
        ),
        SemanticDiagKind::NotAType { name } => lower_name_diag(
            DiagnosticCode::NOT_A_TYPE,
            MessageId::NotAType,
            MessageId::ValueNotType,
            name,
            primary_span,
        ),
    }
}

fn lower_name_diag(
    code: DiagnosticCode,
    msg_id: MessageId,
    label_id: MessageId,
    name: &SmolStr,
    span: Span,
) -> Diagnostic {
    Diagnostic::new(
        Severity::Error,
        code,
        Message::new(msg_id, vec![Arg::Name(name.clone())]),
    )
    .with_label(Label {
        kind: LabelKind::Primary,
        span,
        message: Message::simple(label_id),
    })
}

fn lower_args_diag(
    code: DiagnosticCode,
    msg_id: MessageId,
    args: Vec<Arg>,
    span: Span,
) -> Diagnostic {
    Diagnostic::new(Severity::Error, code, Message::new(msg_id, args.clone())).with_label(Label {
        kind: LabelKind::Primary,
        span,
        message: Message::new(msg_id, args),
    })
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
