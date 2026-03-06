use lyra_diag::{
    Arg, DiagKey, Diagnostic, DiagnosticOrigin, Label, LabelKind, Message, MessageId, Severity,
    code,
};
use lyra_preprocess::PreprocOutput;
use lyra_semantic::def_index::DefIndex;
use lyra_semantic::diagnostic::{DiagSpan, SemanticDiag, SemanticDiagKind};
use lyra_semantic::resolve_index::{
    CoreResolveOutput, ImportConflict, ImportConflictKind, ResolveIndex,
};
use lyra_source::{FileId, Span, TextRange, TextSize};
use smol_str::SmolStr;

/// Pick the best `DiagSpan` from primary+label.
/// Uses `is_valid()` on `NameSpan`/`TokenSpan`, not `SourceMap`.
pub(crate) fn choose_best_diag_span(primary: DiagSpan, label: Option<DiagSpan>) -> TextRange {
    let chosen = match label {
        Some(DiagSpan::Name(ns)) if ns.is_valid() => DiagSpan::Name(ns),
        Some(DiagSpan::Token(ts)) if ts.is_valid() => DiagSpan::Token(ts),
        Some(DiagSpan::Site(s)) => DiagSpan::Site(s),
        _ => primary,
    };
    diag_span_to_text_range(chosen)
}

/// Convert a `DiagSpan` to `TextRange` for source-map mapping.
fn diag_span_to_text_range(span: DiagSpan) -> TextRange {
    match span {
        DiagSpan::Site(s) => s.text_range(),
        DiagSpan::Name(ns) => ns.text_range(),
        DiagSpan::Token(ts) => ts.text_range(),
    }
}

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
        let text = SmolStr::new(&e.message);
        diags.push(
            Diagnostic::new(
                Severity::Error,
                code::PREPROCESS_ERROR,
                Message::new(MessageId::PreprocessError, vec![Arg::Name(text.clone())]),
            )
            .with_label(Label {
                kind: LabelKind::Primary,
                span: e.span,
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
                code::PARSE_ERROR,
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
        let tr = choose_best_diag_span(diag.primary, diag.label);
        let (primary_span, _) = map_span_or_fallback(file_id, &pp.source_map, tr);
        diags.push(lower_semantic_diag(diag, primary_span, &pp.source_map));
    }

    for (site_opt, detail) in &*def.internal_errors {
        let (span, suffix) = match site_opt {
            Some(site) => map_span_or_fallback(file_id, &pp.source_map, site.text_range()),
            None => (
                Span {
                    file: file_id,
                    range: TextRange::empty(TextSize::new(0)),
                },
                Some(" [unanchored]"),
            ),
        };
        let qualified = match suffix {
            Some(s) => SmolStr::new(format!("{detail}{s}")),
            None => detail.clone(),
        };
        diags.push(internal_error_diag(&qualified, span));
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
            code::UNRESOLVED_NAME,
            MessageId::UnresolvedName,
            MessageId::NotFoundInScope,
            name,
            primary_span,
        ),
        SemanticDiagKind::DuplicateDefinition {
            name,
            original_primary,
            original_label,
        } => lower_duplicate_def(
            name,
            *original_primary,
            *original_label,
            primary_span,
            source_map,
        ),
        SemanticDiagKind::PackageNotFound { package } => lower_args_diag(
            code::PACKAGE_NOT_FOUND,
            MessageId::PackageNotFound,
            vec![Arg::Name(package.clone())],
            primary_span,
        ),
        SemanticDiagKind::MemberNotFound { package, member } => lower_args_diag(
            code::MEMBER_NOT_FOUND,
            MessageId::MemberNotFound,
            vec![Arg::Name(package.clone()), Arg::Name(member.clone())],
            primary_span,
        ),
        SemanticDiagKind::AmbiguousWildcardImport { name, candidates } => {
            lower_ambiguous_wildcard(name, candidates, primary_span)
        }
        SemanticDiagKind::UnsupportedQualifiedPath { path } => lower_args_diag(
            code::UNSUPPORTED_QUALIFIED_PATH,
            MessageId::UnsupportedQualifiedPath,
            vec![Arg::Name(path.clone())],
            primary_span,
        ),
        SemanticDiagKind::UndeclaredType { name } => lower_name_diag(
            code::UNDECLARED_TYPE,
            MessageId::UndeclaredType,
            MessageId::NotFoundAsType,
            name,
            primary_span,
        ),
        SemanticDiagKind::NotAType { name } => lower_name_diag(
            code::NOT_A_TYPE,
            MessageId::NotAType,
            MessageId::ValueNotType,
            name,
            primary_span,
        ),
        SemanticDiagKind::VoidMemberNonTagged { .. }
        | SemanticDiagKind::IllegalUnionMemberType { .. }
        | SemanticDiagKind::IllegalEnumBaseType { .. }
        | SemanticDiagKind::EnumBaseDimsNotConstant
        | SemanticDiagKind::EnumRangeBoundNotEvaluable
        | SemanticDiagKind::EnumRangeCountNegative { .. }
        | SemanticDiagKind::EnumRangeTooLarge { .. }
        | SemanticDiagKind::NonIntegralPackedMember { .. } => {
            lower_record_enum_diag(&diag.kind, primary_span)
        }
        SemanticDiagKind::NotASubroutine { name } => lower_name_diag(
            code::NOT_A_SUBROUTINE,
            MessageId::NotASubroutine,
            MessageId::NotASubroutine,
            name,
            primary_span,
        ),
        SemanticDiagKind::PrototypeMismatch { name, mismatch } => {
            lower_prototype_mismatch(name, mismatch, primary_span)
        }
        SemanticDiagKind::InternalError { detail } => internal_error_diag(detail, primary_span),
    }
}

/// Lower record/enum validation diagnostics.
///
/// Total over its input: all `SemanticDiagKind` variants are explicitly matched.
/// Non-handled variants (dispatched directly in `lower_semantic_diag`) produce
/// an internal error so new variants cause compile errors, not silent fallbacks.
fn lower_record_enum_diag(kind: &SemanticDiagKind, primary_span: Span) -> Diagnostic {
    match kind {
        SemanticDiagKind::VoidMemberNonTagged { name } => lower_name_diag(
            code::VOID_MEMBER_NON_TAGGED,
            MessageId::VoidMemberNonTagged,
            MessageId::OnlyInTaggedUnion,
            name,
            primary_span,
        ),
        SemanticDiagKind::IllegalUnionMemberType { name, category } => lower_args_diag(
            code::ILLEGAL_UNION_MEMBER_TYPE,
            MessageId::IllegalUnionMemberType,
            vec![Arg::Name(name.clone()), Arg::Category(category.clone())],
            primary_span,
        ),
        SemanticDiagKind::IllegalEnumBaseType { name } => lower_name_diag(
            code::ILLEGAL_ENUM_BASE,
            MessageId::IllegalEnumBaseType,
            MessageId::IllegalEnumBaseType,
            name,
            primary_span,
        ),
        SemanticDiagKind::EnumBaseDimsNotConstant => lower_args_diag(
            code::ENUM_BASE_DIMS_NOT_CONST,
            MessageId::EnumBaseDimsNotConstant,
            vec![],
            primary_span,
        ),
        SemanticDiagKind::EnumRangeBoundNotEvaluable => lower_args_diag(
            code::ENUM_RANGE_INVALID,
            MessageId::EnumRangeBoundNotEvaluable,
            vec![],
            primary_span,
        ),
        SemanticDiagKind::EnumRangeCountNegative { count } => lower_args_diag(
            code::ENUM_RANGE_INVALID,
            MessageId::EnumRangeCountNegative,
            vec![Arg::Name(SmolStr::new(format!("{count}")))],
            primary_span,
        ),
        SemanticDiagKind::EnumRangeTooLarge { count } => lower_args_diag(
            code::ENUM_RANGE_INVALID,
            MessageId::EnumRangeTooLarge,
            vec![Arg::Name(SmolStr::new(format!("{count}")))],
            primary_span,
        ),
        SemanticDiagKind::NonIntegralPackedMember {
            name,
            record_kind,
            packing,
            category,
        } => lower_args_diag(
            code::NON_INTEGRAL_PACKED_MEMBER,
            MessageId::NonIntegralPackedMember,
            vec![
                Arg::Category(record_kind.clone()),
                Arg::Name(name.clone()),
                Arg::Category(packing.clone()),
                Arg::Category(category.clone()),
            ],
            primary_span,
        ),
        SemanticDiagKind::UnresolvedName { .. }
        | SemanticDiagKind::DuplicateDefinition { .. }
        | SemanticDiagKind::PackageNotFound { .. }
        | SemanticDiagKind::MemberNotFound { .. }
        | SemanticDiagKind::AmbiguousWildcardImport { .. }
        | SemanticDiagKind::UnsupportedQualifiedPath { .. }
        | SemanticDiagKind::UndeclaredType { .. }
        | SemanticDiagKind::NotAType { .. }
        | SemanticDiagKind::NotASubroutine { .. }
        | SemanticDiagKind::PrototypeMismatch { .. }
        | SemanticDiagKind::InternalError { .. } => {
            internal_error_diag("unexpected variant in record/enum lowering", primary_span)
        }
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
            code::IMPORT_CONFLICT,
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
            && let Some(use_span) = pp.source_map.map_span(us.name_ref_site.text_range())
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
            code::IMPORT_CONFLICT,
            MessageId::ExplicitImportConflictsWithLocal,
            vec![Arg::Name(conflict.name.clone()), Arg::Name(package.clone())],
            span,
        ),
        ImportConflictKind::ExplicitVsWildcard {
            explicit_package,
            wildcard_package,
            ..
        } => lower_args_diag(
            code::IMPORT_CONFLICT,
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
    original_primary: DiagSpan,
    original_label: Option<DiagSpan>,
    primary_span: Span,
    source_map: &lyra_preprocess::SourceMap,
) -> Diagnostic {
    let mut d = Diagnostic::new(
        Severity::Error,
        code::DUPLICATE_DEFINITION,
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
    let orig_tr = choose_best_diag_span(original_primary, original_label);
    if let Some(orig_span) = source_map.map_span(orig_tr) {
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
        code::AMBIGUOUS_IMPORT,
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

/// Construct an internal-error diagnostic with consistent severity, code, arg kind, and origin.
pub(crate) fn internal_error_diag(detail: &str, span: Span) -> Diagnostic {
    let text = SmolStr::new(format!("internal error: {detail}"));
    let msg_args = vec![Arg::Detail(text)];
    Diagnostic::new(
        Severity::Error,
        code::INTERNAL_ERROR,
        Message::new(MessageId::InternalError, msg_args.clone()),
    )
    .with_label(Label {
        kind: LabelKind::Primary,
        span,
        message: Message::new(MessageId::InternalError, msg_args),
    })
    .with_origin(DiagnosticOrigin::Internal)
}

fn lower_name_diag(
    code: DiagKey,
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

fn lower_args_diag(code: DiagKey, msg_id: MessageId, args: Vec<Arg>, span: Span) -> Diagnostic {
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

/// Lower jump-statement legality items (LRM 12.8) into diagnostics.
///
/// Each `JumpCheckItem` carries a keyword `TokenSpan` extracted during
/// the builder phase. Lowering maps it through the `SourceMap`.
pub(crate) fn lower_jump_check_items(
    file_id: FileId,
    source_map: &lyra_preprocess::SourceMap,
    items: &[crate::jump_check::JumpCheckItem],
    diags: &mut Vec<Diagnostic>,
) {
    use crate::jump_check::JumpCheckItem;
    for item in items {
        let kw = item.kw();
        if !kw.is_valid() {
            continue;
        }
        let (span, _) = map_span_or_fallback(file_id, source_map, kw.text_range());
        let (code, msg_id) = match item {
            JumpCheckItem::BreakOutsideLoop { .. } => {
                (code::BREAK_OUTSIDE_LOOP, MessageId::BreakOutsideLoop)
            }
            JumpCheckItem::ContinueOutsideLoop { .. } => {
                (code::CONTINUE_OUTSIDE_LOOP, MessageId::ContinueOutsideLoop)
            }
            JumpCheckItem::ReturnOutsideCallable { .. } => (
                code::RETURN_OUTSIDE_CALLABLE,
                MessageId::ReturnOutsideCallable,
            ),
            JumpCheckItem::ReturnValueInVoid { .. } => {
                (code::RETURN_VALUE_IN_VOID, MessageId::ReturnValueInVoid)
            }
            JumpCheckItem::ReturnMissingValue { .. } => {
                (code::RETURN_MISSING_VALUE, MessageId::ReturnMissingValue)
            }
        };
        diags.push(
            Diagnostic::new(Severity::Error, code, Message::simple(msg_id)).with_label(Label {
                kind: LabelKind::Primary,
                span,
                message: Message::simple(msg_id),
            }),
        );
    }
}

/// Lower case-statement legality items (LRM 12.5.4) into diagnostics.
pub(crate) fn lower_case_check_items(
    file_id: FileId,
    source_map: &lyra_preprocess::SourceMap,
    items: &[lyra_semantic::case_check::CaseCheckItem],
    diags: &mut Vec<Diagnostic>,
) {
    use lyra_semantic::case_check::CaseCheckItem;
    for item in items {
        match item {
            CaseCheckItem::IllegalInsideCaseKind { kw_span, .. } => {
                if !kw_span.is_valid() {
                    continue;
                }
                let (span, _) = map_span_or_fallback(file_id, source_map, kw_span.text_range());
                let msg_id = MessageId::CaseInsideRequiresPlainCase;
                diags.push(
                    Diagnostic::new(
                        Severity::Error,
                        code::CASE_INSIDE_REQUIRES_PLAIN_CASE,
                        Message::simple(msg_id),
                    )
                    .with_label(Label {
                        kind: LabelKind::Primary,
                        span,
                        message: Message::simple(msg_id),
                    }),
                );
            }
        }
    }
}

/// Lower foreach-legality items (LRM 12.7.3) into diagnostics.
pub(crate) fn lower_foreach_check_items(
    file_id: FileId,
    source_map: &lyra_preprocess::SourceMap,
    items: &[lyra_semantic::foreach_check::ForeachCheckItem],
    diags: &mut Vec<Diagnostic>,
) {
    use lyra_semantic::foreach_check::ForeachCheckItem;
    for item in items {
        let (span_range, code, msg) = match item {
            ForeachCheckItem::AssignToForeachVar {
                lhs_name_span,
                var_name,
            } => {
                if !lhs_name_span.is_valid() {
                    continue;
                }
                (
                    lhs_name_span.text_range(),
                    code::ASSIGN_TO_FOREACH_VAR,
                    Message::new(
                        MessageId::AssignToForeachVar,
                        vec![Arg::Name(var_name.clone())],
                    ),
                )
            }
            ForeachCheckItem::VarSameNameAsArray {
                var_name_span,
                array_name,
            } => {
                if !var_name_span.is_valid() {
                    continue;
                }
                (
                    var_name_span.text_range(),
                    code::FOREACH_VAR_SAME_AS_ARRAY,
                    Message::new(
                        MessageId::ForeachVarSameAsArray,
                        vec![Arg::Name(array_name.clone())],
                    ),
                )
            }
            ForeachCheckItem::TooManyVars {
                excess_var_span,
                dim_count,
                var_count,
            } => {
                if !excess_var_span.is_valid() {
                    continue;
                }
                (
                    excess_var_span.text_range(),
                    code::FOREACH_TOO_MANY_VARS,
                    Message::new(
                        MessageId::ForeachTooManyVars,
                        vec![
                            Arg::Count(*dim_count as usize),
                            Arg::Count(*var_count as usize),
                        ],
                    ),
                )
            }
        };
        let (span, _) = map_span_or_fallback(file_id, source_map, span_range);
        diags.push(
            Diagnostic::new(Severity::Error, code, msg.clone()).with_label(Label {
                kind: LabelKind::Primary,
                span,
                message: msg,
            }),
        );
    }
}

fn lower_prototype_mismatch(
    name: &SmolStr,
    mismatch: &lyra_semantic::diagnostic::PrototypeMismatchDetail,
    primary_span: Span,
) -> Diagnostic {
    use lyra_semantic::diagnostic::PrototypeMismatchDetail;

    let (msg_id, args) = match mismatch {
        PrototypeMismatchDetail::ReturnType => (
            MessageId::PrototypeMismatchReturnType,
            vec![Arg::Name(name.clone())],
        ),
        PrototypeMismatchDetail::PortCount { proto, actual } => (
            MessageId::PrototypeMismatchPortCount,
            vec![
                Arg::Name(name.clone()),
                Arg::Count(*proto),
                Arg::Count(*actual),
            ],
        ),
        PrototypeMismatchDetail::PortDirection {
            index,
            proto_dir,
            actual_dir,
        } => (
            MessageId::PrototypeMismatchPortDirection,
            vec![
                Arg::Name(name.clone()),
                Arg::Count(index + 1),
                Arg::Name(SmolStr::new(proto_dir.label())),
                Arg::Name(SmolStr::new(actual_dir.label())),
            ],
        ),
        PrototypeMismatchDetail::PortType { index } => (
            MessageId::PrototypeMismatchPortType,
            vec![Arg::Name(name.clone()), Arg::Count(index + 1)],
        ),
    };
    lower_args_diag(code::PROTOTYPE_MISMATCH, msg_id, args, primary_span)
}

#[cfg(test)]
mod tests {
    use super::*;
    use lyra_diag::{DiagnosticOrigin, MessageId, code};

    #[test]
    fn internal_errors_lowered_end_to_end() {
        let src = "module m; endmodule";
        let tokens = lyra_lexer::lex(src);
        let pp = lyra_preprocess::preprocess_identity(FileId(0), &tokens, src);
        let parse = lyra_parser::parse(&pp.tokens, &pp.expanded_text);
        let map = lyra_ast::AstIdMap::from_root(FileId(0), &parse.syntax());
        let mut def = lyra_semantic::build_def_index(FileId(0), &parse, &map);
        // Inject a synthetic internal error to test the lowering path
        let module_site = map.erased_ast_id(&parse.syntax().first_child().unwrap());
        let mut errors = def.internal_errors.to_vec();
        errors.push((module_site, SmolStr::new("test invariant violation")));
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
        assert_eq!(d.code, code::INTERNAL_ERROR);
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
