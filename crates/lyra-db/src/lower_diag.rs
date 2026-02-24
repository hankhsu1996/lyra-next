use lyra_diag::{
    Arg, Diagnostic, DiagnosticCode, DiagnosticOrigin, Label, LabelKind, Message, MessageId,
    Severity,
};
use lyra_preprocess::PreprocOutput;
use lyra_semantic::def_index::DefIndex;
use lyra_semantic::diagnostic::{SemanticDiag, SemanticDiagKind};
use lyra_semantic::resolve_index::{
    CoreResolveOutput, ImportConflict, ImportConflictKind, ResolveIndex,
};
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

    for (range, detail) in &*def.internal_errors {
        let (span, extra) = map_span_or_fallback(file_id, &pp.source_map, *range);
        let text = freeform_text(&format!("internal error: {detail}"), extra);
        diags.push(
            Diagnostic::new(
                Severity::Error,
                DiagnosticCode::INTERNAL_ERROR,
                Message::new(MessageId::InternalError, vec![Arg::Name(text.clone())]),
            )
            .with_label(Label {
                kind: LabelKind::Primary,
                span,
                message: Message::new(MessageId::InternalError, vec![Arg::Name(text)]),
            })
            .with_origin(DiagnosticOrigin::Internal),
        );
    }

    diags
}

pub(crate) fn lower_semantic_diag(
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
            lower_duplicate_def(name, *original, primary_span, source_map)
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
            lower_ambiguous_wildcard(name, candidates, primary_span)
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
        SemanticDiagKind::UnsupportedTaggedUnion => lower_args_diag(
            DiagnosticCode::UNSUPPORTED_TAGGED_UNION,
            MessageId::UnsupportedTaggedUnion,
            vec![],
            primary_span,
        ),
        SemanticDiagKind::IllegalEnumBaseType { name } => lower_name_diag(
            DiagnosticCode::ILLEGAL_ENUM_BASE,
            MessageId::IllegalEnumBaseType,
            MessageId::IllegalEnumBaseType,
            name,
            primary_span,
        ),
        SemanticDiagKind::EnumBaseDimsNotConstant => lower_args_diag(
            DiagnosticCode::ENUM_BASE_DIMS_NOT_CONST,
            MessageId::EnumBaseDimsNotConstant,
            vec![],
            primary_span,
        ),
        SemanticDiagKind::EnumRangeBoundNotEvaluable => lower_args_diag(
            DiagnosticCode::ENUM_RANGE_INVALID,
            MessageId::EnumRangeBoundNotEvaluable,
            vec![],
            primary_span,
        ),
        SemanticDiagKind::EnumRangeCountNegative { count } => lower_args_diag(
            DiagnosticCode::ENUM_RANGE_INVALID,
            MessageId::EnumRangeCountNegative,
            vec![Arg::Name(SmolStr::new(format!("{count}")))],
            primary_span,
        ),
        SemanticDiagKind::EnumRangeTooLarge { count } => lower_args_diag(
            DiagnosticCode::ENUM_RANGE_INVALID,
            MessageId::EnumRangeTooLarge,
            vec![Arg::Name(SmolStr::new(format!("{count}")))],
            primary_span,
        ),
        SemanticDiagKind::InternalError { detail } => lower_internal_error(detail, primary_span),
    }
}

/// Lower import conflicts (LRM 26.5) to structured diagnostics.
pub(crate) fn lower_import_conflicts(
    file_id: FileId,
    pp: &PreprocOutput,
    def: &DefIndex,
    conflicts: &[ImportConflict],
) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    for conflict in conflicts {
        if let Some(d) = lower_single_import_conflict(file_id, pp, def, conflict) {
            diags.push(d);
        }
    }
    diags
}

/// Lower LRM 26.3 wildcard-local declaration conflicts to diagnostics.
pub(crate) fn lower_wildcard_local_conflicts(
    file_id: FileId,
    pp: &PreprocOutput,
    def: &DefIndex,
    core: &CoreResolveOutput,
) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    for conflict in &*core.wildcard_local_conflicts {
        // Look up local decl by (scope, ordinal)
        let local = def
            .local_decls
            .binary_search_by_key(
                &(conflict.local_decl_id.scope, conflict.local_decl_id.ordinal),
                |d| (d.id.scope, d.id.ordinal),
            )
            .ok()
            .map(|idx| &def.local_decls[idx]);
        let Some(local) = local else { continue };

        // Look up import by (scope, ordinal)
        let import = def
            .imports
            .binary_search_by_key(
                &(conflict.import_id.scope, conflict.import_id.ordinal),
                |i| (i.id.scope, i.id.ordinal),
            )
            .ok()
            .map(|idx| &def.imports[idx]);

        let local_range = local.name_span.text_range_or(local.decl_site.text_range());
        let (primary_span, _) = map_span_or_fallback(file_id, &pp.source_map, local_range);
        let name = local.name.clone();
        let pkg = import.map_or_else(|| SmolStr::new("?"), |i| i.package.clone());

        let mut d = Diagnostic::new(
            Severity::Error,
            DiagnosticCode::IMPORT_CONFLICT,
            Message::new(
                MessageId::WildcardLocalConflict,
                vec![Arg::Name(name.clone()), Arg::Name(pkg)],
            ),
        )
        .with_label(Label {
            kind: LabelKind::Primary,
            span: primary_span,
            message: Message::simple(MessageId::RedefinedHere),
        });

        // Secondary label: "wildcard import of `name` realized here"
        if let Some(us) = def.use_sites.get(conflict.use_site_idx as usize)
            && let Some(use_span) = pp.source_map.map_span(us.range)
        {
            d = d.with_label(Label {
                kind: LabelKind::Secondary,
                span: use_span,
                message: Message::new(MessageId::RealizedHere, vec![Arg::Name(name.clone())]),
            });
        }

        // Secondary label: "wildcard import here"
        if let Some(imp) = import
            && let Some(imp_span) = pp.source_map.map_span(imp.import_stmt_site.text_range())
        {
            d = d.with_label(Label {
                kind: LabelKind::Secondary,
                span: imp_span,
                message: Message::simple(MessageId::WildcardImportHere),
            });
        }

        diags.push(d);
    }
    diags
}

fn find_import_range(def: &DefIndex, conflict: &ImportConflict) -> Option<TextRange> {
    def.imports.iter().find_map(|imp| {
        if imp.scope != conflict.scope {
            return None;
        }
        let matches = match (&conflict.kind, &imp.name) {
            (
                ImportConflictKind::ExplicitVsLocal { package, .. },
                lyra_semantic::def_index::ImportName::Explicit(member),
            ) => member.as_str() == conflict.name.as_str() && imp.package == *package,
            (
                ImportConflictKind::ExplicitVsWildcard {
                    explicit_package, ..
                },
                lyra_semantic::def_index::ImportName::Explicit(member),
            ) => member.as_str() == conflict.name.as_str() && imp.package == *explicit_package,
            _ => false,
        };
        if matches {
            Some(imp.import_stmt_site.text_range())
        } else {
            None
        }
    })
}

fn lower_single_import_conflict(
    file_id: FileId,
    pp: &PreprocOutput,
    def: &DefIndex,
    conflict: &ImportConflict,
) -> Option<Diagnostic> {
    // For export-triggered conflicts, use the export declaration range.
    // For pure explicit import conflicts, scan imports by (package, name, scope).
    let range = if let Some(export_id) = conflict.export_sources.first() {
        def.export_decls
            .binary_search_by_key(&(export_id.scope, export_id.ordinal), |e| {
                (e.id.scope, e.id.ordinal)
            })
            .ok()
            .map(|idx| def.export_decls[idx].export_stmt_site.text_range())?
    } else {
        find_import_range(def, conflict)?
    };
    let (span, _) = map_span_or_fallback(file_id, &pp.source_map, range);

    let d = match &conflict.kind {
        ImportConflictKind::ExplicitVsLocal { package, .. } => lower_args_diag(
            DiagnosticCode::IMPORT_CONFLICT,
            MessageId::ExplicitImportConflictsWithLocal,
            vec![Arg::Name(conflict.name.clone()), Arg::Name(package.clone())],
            span,
        ),
        ImportConflictKind::ExplicitVsWildcard {
            explicit_package,
            wildcard_package,
            ..
        } => lower_args_diag(
            DiagnosticCode::IMPORT_CONFLICT,
            MessageId::ExplicitConflictsWithWildcard,
            vec![
                Arg::Name(conflict.name.clone()),
                Arg::Name(explicit_package.clone()),
                Arg::Name(wildcard_package.clone()),
            ],
            span,
        ),
    };
    Some(d)
}

fn lower_duplicate_def(
    name: &SmolStr,
    original: TextRange,
    primary_span: Span,
    source_map: &lyra_preprocess::SourceMap,
) -> Diagnostic {
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
    if let Some(orig_span) = source_map.map_span(original) {
        d = d.with_label(Label {
            kind: LabelKind::Secondary,
            span: orig_span,
            message: Message::simple(MessageId::FirstDefinedHere),
        });
    }
    d
}

fn lower_ambiguous_wildcard(name: &SmolStr, candidates: &[SmolStr], span: Span) -> Diagnostic {
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
        span,
        message: Message::simple(MessageId::AmbiguousWildcardImport),
    })
}

fn lower_internal_error(detail: &SmolStr, span: Span) -> Diagnostic {
    let text = SmolStr::new(format!("internal error: {detail}"));
    Diagnostic::new(
        Severity::Error,
        DiagnosticCode::INTERNAL_ERROR,
        Message::new(MessageId::InternalError, vec![Arg::Name(text.clone())]),
    )
    .with_label(Label {
        kind: LabelKind::Primary,
        span,
        message: Message::new(MessageId::InternalError, vec![Arg::Name(text)]),
    })
    .with_origin(DiagnosticOrigin::Internal)
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
pub(crate) fn map_span_or_fallback(
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

#[cfg(test)]
mod tests {
    use super::*;
    use lyra_diag::{DiagnosticCode, DiagnosticOrigin, MessageId};

    #[test]
    fn internal_errors_lowered_end_to_end() {
        let src = "module m; endmodule";
        let tokens = lyra_lexer::lex(src);
        let pp = lyra_preprocess::preprocess_identity(FileId(0), &tokens, src);
        let parse = lyra_parser::parse(&pp.tokens, &pp.expanded_text);
        let map = lyra_ast::AstIdMap::from_root(FileId(0), &parse.syntax());
        let mut def = lyra_semantic::build_def_index(FileId(0), &parse, &map);
        // Inject a synthetic internal error to test the lowering path
        let mut errors = def.internal_errors.to_vec();
        errors.push((
            TextRange::new(TextSize::new(0), TextSize::new(6)),
            SmolStr::new("test invariant violation"),
        ));
        def.internal_errors = errors.into_boxed_slice();

        let resolve = lyra_semantic::resolve_index::ResolveIndex {
            file: FileId(0),
            resolutions: std::collections::HashMap::new(),
            diagnostics: Box::new([]),
        };
        let diags = lower_file_diagnostics(FileId(0), &pp, &parse, &def, &resolve);

        let internal: Vec<_> = diags
            .iter()
            .filter(|d| d.origin == DiagnosticOrigin::Internal)
            .collect();
        assert_eq!(internal.len(), 1, "should have exactly one internal diag");
        let d = &internal[0];
        assert_eq!(d.code, DiagnosticCode::INTERNAL_ERROR);
        assert_eq!(d.message.id, MessageId::InternalError);
        let rendered = d.render_message();
        assert!(
            rendered.contains("internal error:"),
            "message should contain 'internal error:': {rendered}"
        );
        assert!(
            rendered.contains("test invariant violation"),
            "message should contain detail text: {rendered}"
        );
    }
}
