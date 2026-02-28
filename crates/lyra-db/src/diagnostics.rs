use lyra_ast::{AstIdMap, AstNode, Expr, HasSyntax};
use lyra_semantic::coerce::IntegralCtx;
use lyra_semantic::modport_def::PortDirection;
use lyra_semantic::type_check::{AccessKind, TypeCheckCtx, TypeCheckItem};
use lyra_semantic::type_infer::ExprType;
use lyra_semantic::types::SymbolType;

use crate::checks_index::{CheckKind, checks_index};
use crate::enum_queries::{EnumRef, enum_sem, enum_variant_index};
use crate::expr_queries::{ExprRef, IntegralCtxKey};
use crate::facts::modport::field_access_facts;
use crate::pipeline::ast_id_map as query_ast_id_map;
use crate::pipeline::{ast_id_map, parse_file, preprocess_file};
use crate::record_queries::{RecordRef, record_diagnostics};
use crate::semantic::{
    def_index_file, global_def_index, import_conflicts_file, resolve_core_file, resolve_index_file,
};
use crate::type_queries::{SymbolRef, type_of_symbol};
use crate::{CompilationUnit, SourceFile, source_file_by_id};

/// Convert parse, preprocess, and semantic errors into structured diagnostics (Salsa-cached).
#[salsa::tracked(return_ref)]
pub fn file_diagnostics(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
) -> Vec<lyra_diag::Diagnostic> {
    let pp = preprocess_file(db, file);
    let parse = parse_file(db, file);
    let def = def_index_file(db, file);
    let resolve = resolve_index_file(db, file, unit);
    let conflicts = import_conflicts_file(db, file, unit);
    let core = resolve_core_file(db, file, unit);
    let mut diags =
        crate::lower_diag::lower_file_diagnostics(file.file_id(db), pp, parse, def, resolve);
    diags.extend(crate::lower_diag::lower_import_conflicts(
        file.file_id(db),
        pp,
        def,
        conflicts,
    ));
    diags.extend(crate::lower_diag::lower_wildcard_local_conflicts(
        file.file_id(db),
        pp,
        def,
        core,
    ));
    diags.extend(type_diagnostics(db, file, unit).iter().cloned());

    // Enum base type diagnostics
    let file_id = file.file_id(db);
    for enum_def in &*def.enum_defs {
        let enum_id = lyra_semantic::enum_def::EnumId::new(enum_def.enum_type_site);
        let eref = EnumRef::new(db, unit, enum_id);
        let sem = enum_sem(db, eref);
        for diag in &*sem.diags {
            let chosen = crate::lower_diag::choose_best_diag_span(diag.primary, diag.label);
            let (primary_span, _) =
                crate::lower_diag::map_span_or_fallback(file_id, &pp.source_map, chosen);
            diags.push(crate::lower_diag::lower_semantic_diag(
                diag,
                primary_span,
                &pp.source_map,
            ));
        }
        if !sem.value_diags.is_empty() {
            let ast_map = query_ast_id_map(db, file);
            for vd in &*sem.value_diags {
                if let Some(d) = lower_enum_value_diag(vd, file_id, ast_map, &pp.source_map) {
                    diags.push(d);
                }
            }
        }
    }

    // Enum variant expansion diagnostics (range bounds, collisions)
    let ev_idx = enum_variant_index(db, file, unit);
    for diag in &*ev_idx.diagnostics {
        let chosen = crate::lower_diag::choose_best_diag_span(diag.primary, diag.label);
        let (primary_span, _) =
            crate::lower_diag::map_span_or_fallback(file_id, &pp.source_map, chosen);
        diags.push(crate::lower_diag::lower_semantic_diag(
            diag,
            primary_span,
            &pp.source_map,
        ));
    }

    // Record diagnostics (type resolution errors + packed union width)
    for record_def in &*def.record_defs {
        let record_id = lyra_semantic::record::RecordId::new(record_def.record_type_site);
        let rref = RecordRef::new(db, unit, record_id);
        diags.extend(record_diagnostics(db, rref).iter().cloned());
    }

    // Type-extraction internal errors (MissingSite in normalized types)
    for (sym_id, _sym) in def.symbols.iter() {
        let gsym = lyra_semantic::symbols::GlobalSymbolId {
            file: file_id,
            local: sym_id,
        };
        let sym_ref = SymbolRef::new(db, unit, gsym);
        let errors = crate::type_queries::type_of_symbol_internal_errors(db, sym_ref);
        for fact in errors {
            if let Some(span) = pp.source_map.map_span(fact.site.text_range()) {
                diags.push(crate::lower_diag::internal_error_diag(&fact.detail, span));
            }
        }
    }

    diags
}

/// Per-file type-check diagnostics (Salsa-cached).
///
/// Iterates the `ChecksIndex` for this file and dispatches each entry
/// to the appropriate pure `check_*` function in `lyra-semantic`.
#[salsa::tracked(return_ref)]
pub fn type_diagnostics(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
) -> Vec<lyra_diag::Diagnostic> {
    let parse = parse_file(db, file);
    let map = ast_id_map(db, file);
    let pp = preprocess_file(db, file);
    let index = checks_index(db, file);
    let facts = field_access_facts(db, file, unit);

    let ctx = DbTypeCheckCtx {
        db,
        unit,
        source_file: file,
        ast_id_map: map,
    };

    let root = parse.syntax();
    let mut items = Vec::new();
    for entry in &*index.entries {
        let Some(node) = map.get_node(&root, entry.site) else {
            continue;
        };
        let fallback = entry.site;
        match entry.kind {
            CheckKind::ContinuousAssign => {
                if let Some(ca) = lyra_ast::ContinuousAssign::cast(node) {
                    lyra_semantic::type_check::check_continuous_assign(
                        &ca, &ctx, fallback, &mut items,
                    );
                }
            }
            CheckKind::AssignStmt => {
                if let Some(assign) = lyra_ast::AssignStmt::cast(node) {
                    lyra_semantic::type_check::check_assign_stmt(
                        &assign, &ctx, fallback, &mut items,
                    );
                }
            }
            CheckKind::VarDecl => {
                if let Some(vd) = lyra_ast::VarDecl::cast(node) {
                    lyra_semantic::type_check::check_var_decl(&vd, &ctx, fallback, &mut items);
                }
            }
            CheckKind::SystemTfCall => {
                if let Some(stf) = lyra_ast::SystemTfCall::cast(node) {
                    lyra_semantic::type_check::check_system_call(&stf, &ctx, fallback, &mut items);
                }
            }
            CheckKind::FieldExpr => {
                if let Some(field) = lyra_ast::FieldExpr::cast(node) {
                    lyra_semantic::type_check::check_field_direction(
                        &field,
                        &ctx,
                        facts,
                        entry.access,
                        &mut items,
                    );
                }
            }
            CheckKind::CastExpr => {
                if let Some(cast) = lyra_ast::CastExpr::cast(node) {
                    lyra_semantic::type_check::check_cast_expr(&cast, &ctx, fallback, &mut items);
                }
            }
            CheckKind::StreamOperandItem => {
                if let Some(soi) = lyra_ast::StreamOperandItem::cast(node) {
                    lyra_semantic::type_check::check_stream_operand(
                        &soi, &ctx, fallback, &mut items,
                    );
                }
            }
            CheckKind::CallExpr => {
                if let Some(call) = lyra_ast::CallExpr::cast(node) {
                    lyra_semantic::type_check::check_method_call(&call, &ctx, &mut items);
                }
            }
        }
    }

    let mut seen = std::collections::HashSet::new();
    let mut diags = Vec::new();
    for item in &items {
        lower_type_check_item(db, unit, item, &pp.source_map, &mut seen, &mut diags);
    }
    diags
}

fn lower_type_check_item(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    item: &TypeCheckItem,
    source_map: &lyra_preprocess::SourceMap,
    seen: &mut std::collections::HashSet<(
        lyra_ast::ErasedAstId,
        lyra_ast::ErasedAstId,
        lyra_ast::ErasedAstId,
    )>,
    diags: &mut Vec<lyra_diag::Diagnostic>,
) {
    match item {
        TypeCheckItem::AssignTruncation { .. } => {
            lower_assign_truncation(item, source_map, seen, diags);
        }
        TypeCheckItem::BitsNonDataType { .. } => {
            lower_bits_non_data_type(item, source_map, diags);
        }
        TypeCheckItem::EnumAssignFromNonEnum { .. } | TypeCheckItem::EnumAssignWrongEnum { .. } => {
            lower_enum_assign_item(db, unit, item, source_map, diags);
        }
        TypeCheckItem::ConversionArgCategory { .. }
        | TypeCheckItem::ConversionWidthMismatch { .. } => {
            lower_conversion_item(item, source_map, diags);
        }
        TypeCheckItem::ModportDirectionViolation { .. }
        | TypeCheckItem::ModportRefUnsupported { .. }
        | TypeCheckItem::ModportEmptyPortAccess { .. }
        | TypeCheckItem::ModportExprNotAssignable { .. } => {
            lower_modport_item(item, source_map, diags);
        }
        TypeCheckItem::EnumCastOutOfRange { .. } => {
            lower_enum_cast_item(db, unit, item, source_map, diags);
        }
        TypeCheckItem::StreamWithNonArray { .. } => {
            lower_stream_with_non_array(item, source_map, diags);
        }
        TypeCheckItem::MethodCallError { .. } => {
            lower_method_call_error(item, source_map, diags);
        }
        TypeCheckItem::UnsupportedLhsForm { .. } | TypeCheckItem::InvalidLhs { .. } => {
            lower_lhs_item(item, source_map, diags);
        }
        TypeCheckItem::InternalError { .. } => {
            lower_internal_type_check_error(item, source_map, diags);
        }
    }
}

fn lower_assign_truncation(
    item: &TypeCheckItem,
    source_map: &lyra_preprocess::SourceMap,
    seen: &mut std::collections::HashSet<(
        lyra_ast::ErasedAstId,
        lyra_ast::ErasedAstId,
        lyra_ast::ErasedAstId,
    )>,
    diags: &mut Vec<lyra_diag::Diagnostic>,
) {
    let TypeCheckItem::AssignTruncation {
        assign_site,
        lhs_site,
        rhs_site,
        lhs_width,
        rhs_width,
    } = item
    else {
        return;
    };
    let key = (*assign_site, *lhs_site, *rhs_site);
    if !seen.insert(key) {
        return;
    }
    let Some(assign_span) = source_map.map_span(assign_site.text_range()) else {
        return;
    };
    let lhs_span = source_map
        .map_span(lhs_site.text_range())
        .unwrap_or(assign_span);
    let rhs_span = source_map
        .map_span(rhs_site.text_range())
        .unwrap_or(assign_span);
    diags.push(truncation_diag(
        *lhs_width,
        *rhs_width,
        assign_span,
        lhs_span,
        rhs_span,
    ));
}

fn lower_conversion_item(
    item: &TypeCheckItem,
    source_map: &lyra_preprocess::SourceMap,
    diags: &mut Vec<lyra_diag::Diagnostic>,
) {
    let (call_site, arg_site, msg_id, msg_args) = match item {
        TypeCheckItem::ConversionArgCategory {
            call_site,
            arg_site,
            fn_name,
            expected,
        } => (
            call_site,
            arg_site,
            lyra_diag::MessageId::ConversionArgCategory,
            vec![
                lyra_diag::Arg::Name(fn_name.clone()),
                lyra_diag::Arg::Name(smol_str::SmolStr::new(expected)),
            ],
        ),
        TypeCheckItem::ConversionWidthMismatch {
            call_site,
            arg_site,
            fn_name,
            expected_width,
            actual_width,
        } => (
            call_site,
            arg_site,
            lyra_diag::MessageId::ConversionWidthMismatch,
            vec![
                lyra_diag::Arg::Name(fn_name.clone()),
                lyra_diag::Arg::Width(*expected_width),
                lyra_diag::Arg::Width(*actual_width),
            ],
        ),
        _ => return,
    };
    let Some(call_span) = source_map.map_span(call_site.text_range()) else {
        return;
    };
    let arg_span = source_map
        .map_span(arg_site.text_range())
        .unwrap_or(call_span);
    diags.push(conversion_diag(msg_id, msg_args, arg_span));
}

fn lower_internal_type_check_error(
    item: &TypeCheckItem,
    source_map: &lyra_preprocess::SourceMap,
    diags: &mut Vec<lyra_diag::Diagnostic>,
) {
    let TypeCheckItem::InternalError { detail, site } = item else {
        return;
    };
    let Some(span) = source_map.map_span(site.text_range()) else {
        return;
    };
    diags.push(crate::lower_diag::internal_error_diag(detail, span));
}

fn lower_modport_item(
    item: &TypeCheckItem,
    source_map: &lyra_preprocess::SourceMap,
    diags: &mut Vec<lyra_diag::Diagnostic>,
) {
    match item {
        TypeCheckItem::ModportDirectionViolation {
            member_name_span,
            direction,
            access,
        } => {
            let Some(member_span) = source_map.map_span(member_name_span.text_range()) else {
                return;
            };
            let dir_name = match direction {
                PortDirection::Input => "input",
                PortDirection::Output => "output",
                PortDirection::Inout => "inout",
                PortDirection::Ref => "ref",
            };
            let access_name = match access {
                AccessKind::Read => "read",
                AccessKind::Write => "write",
            };
            let msg_args = vec![
                lyra_diag::Arg::Name(smol_str::SmolStr::new(dir_name)),
                lyra_diag::Arg::Name(smol_str::SmolStr::new(access_name)),
            ];
            diags.push(
                lyra_diag::Diagnostic::new(
                    lyra_diag::Severity::Error,
                    lyra_diag::DiagnosticCode::MODPORT_DIRECTION,
                    lyra_diag::Message::new(
                        lyra_diag::MessageId::ModportDirectionViolation,
                        msg_args.clone(),
                    ),
                )
                .with_label(lyra_diag::Label {
                    kind: lyra_diag::LabelKind::Primary,
                    span: member_span,
                    message: lyra_diag::Message::new(
                        lyra_diag::MessageId::ModportDirectionViolation,
                        msg_args,
                    ),
                }),
            );
        }
        TypeCheckItem::ModportRefUnsupported { member_name_span } => {
            emit_simple_modport_diag(
                source_map,
                diags,
                *member_name_span,
                lyra_diag::Severity::Warning,
                lyra_diag::DiagnosticCode::MODPORT_REF_UNSUPPORTED,
                lyra_diag::MessageId::ModportRefUnsupported,
            );
        }
        TypeCheckItem::ModportEmptyPortAccess { member_name_span } => {
            emit_simple_modport_diag(
                source_map,
                diags,
                *member_name_span,
                lyra_diag::Severity::Error,
                lyra_diag::DiagnosticCode::MODPORT_EMPTY_PORT,
                lyra_diag::MessageId::ModportEmptyPortAccess,
            );
        }
        TypeCheckItem::ModportExprNotAssignable { member_name_span } => {
            emit_simple_modport_diag(
                source_map,
                diags,
                *member_name_span,
                lyra_diag::Severity::Error,
                lyra_diag::DiagnosticCode::MODPORT_EXPR_NOT_ASSIGNABLE,
                lyra_diag::MessageId::ModportExprNotAssignable,
            );
        }
        _ => {}
    }
}

fn emit_simple_modport_diag(
    source_map: &lyra_preprocess::SourceMap,
    diags: &mut Vec<lyra_diag::Diagnostic>,
    name_span: lyra_source::NameSpan,
    severity: lyra_diag::Severity,
    code: lyra_diag::DiagnosticCode,
    message_id: lyra_diag::MessageId,
) {
    let Some(span) = source_map.map_span(name_span.text_range()) else {
        return;
    };
    diags.push(
        lyra_diag::Diagnostic::new(severity, code, lyra_diag::Message::simple(message_id))
            .with_label(lyra_diag::Label {
                kind: lyra_diag::LabelKind::Primary,
                span,
                message: lyra_diag::Message::simple(message_id),
            }),
    );
}

fn lower_lhs_item(
    item: &TypeCheckItem,
    source_map: &lyra_preprocess::SourceMap,
    diags: &mut Vec<lyra_diag::Diagnostic>,
) {
    let (site, code, msg_id) = match item {
        TypeCheckItem::UnsupportedLhsForm { lhs_site } => (
            *lhs_site,
            lyra_diag::DiagnosticCode::UNSUPPORTED_LHS_FORM,
            lyra_diag::MessageId::UnsupportedLhsForm,
        ),
        TypeCheckItem::InvalidLhs { lhs_site } => (
            *lhs_site,
            lyra_diag::DiagnosticCode::INVALID_ASSIGNMENT_LHS,
            lyra_diag::MessageId::InvalidAssignmentLhs,
        ),
        _ => return,
    };
    let Some(span) = source_map.map_span(site.text_range()) else {
        return;
    };
    diags.push(
        lyra_diag::Diagnostic::new(
            lyra_diag::Severity::Warning,
            code,
            lyra_diag::Message::simple(msg_id),
        )
        .with_label(lyra_diag::Label {
            kind: lyra_diag::LabelKind::Primary,
            span,
            message: lyra_diag::Message::simple(msg_id),
        }),
    );
}

fn lower_enum_cast_item(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    item: &TypeCheckItem,
    source_map: &lyra_preprocess::SourceMap,
    diags: &mut Vec<lyra_diag::Diagnostic>,
) {
    let TypeCheckItem::EnumCastOutOfRange {
        cast_site,
        enum_id,
        value,
    } = item
    else {
        return;
    };
    let Some(cast_span) = source_map.map_span(cast_site.text_range()) else {
        return;
    };
    let ename = enum_name(db, unit, enum_id);
    let msg_args = vec![
        lyra_diag::Arg::Name(smol_str::SmolStr::new(value.to_string())),
        lyra_diag::Arg::Name(ename),
    ];
    diags.push(
        lyra_diag::Diagnostic::new(
            lyra_diag::Severity::Warning,
            lyra_diag::DiagnosticCode::ENUM_CAST_OUT_OF_RANGE,
            lyra_diag::Message::new(lyra_diag::MessageId::EnumCastOutOfRange, msg_args.clone()),
        )
        .with_label(lyra_diag::Label {
            kind: lyra_diag::LabelKind::Primary,
            span: cast_span,
            message: lyra_diag::Message::new(lyra_diag::MessageId::EnumCastOutOfRange, msg_args),
        }),
    );
}

fn lower_enum_assign_item(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    item: &TypeCheckItem,
    source_map: &lyra_preprocess::SourceMap,
    diags: &mut Vec<lyra_diag::Diagnostic>,
) {
    let (assign_site, lhs_site, rhs_site, msg_id, rhs_label, lhs_enum) = match item {
        TypeCheckItem::EnumAssignFromNonEnum {
            assign_site,
            lhs_site,
            rhs_site,
            lhs_enum,
            rhs_ty,
        } => (
            *assign_site,
            *lhs_site,
            *rhs_site,
            lyra_diag::MessageId::EnumAssignFromNonEnum,
            rhs_ty.pretty(),
            lhs_enum,
        ),
        TypeCheckItem::EnumAssignWrongEnum {
            assign_site,
            lhs_site,
            rhs_site,
            lhs_enum,
            rhs_enum,
        } => (
            *assign_site,
            *lhs_site,
            *rhs_site,
            lyra_diag::MessageId::EnumAssignWrongEnum,
            enum_name(db, unit, rhs_enum),
            lhs_enum,
        ),
        _ => return,
    };
    let Some(assign_span) = source_map.map_span(assign_site.text_range()) else {
        return;
    };
    let rhs_span = source_map
        .map_span(rhs_site.text_range())
        .unwrap_or(assign_span);
    let lhs_span = source_map
        .map_span(lhs_site.text_range())
        .unwrap_or(assign_span);
    let lhs_name = enum_name(db, unit, lhs_enum);
    diags.push(enum_assign_diag(
        msg_id, rhs_label, lhs_name, rhs_span, lhs_span,
    ));
}

fn enum_name(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    id: &lyra_semantic::enum_def::EnumId,
) -> smol_str::SmolStr {
    let Some(sf) = source_file_by_id(db, unit, id.file()) else {
        return smol_str::SmolStr::new_static("<anonymous enum>");
    };
    let def = def_index_file(db, sf);
    match def.enum_def_by_id(*id) {
        Some(enum_def) => enum_def
            .name
            .clone()
            .unwrap_or_else(|| smol_str::SmolStr::new_static("<anonymous enum>")),
        None => smol_str::SmolStr::new_static("<anonymous enum>"),
    }
}

fn truncation_diag(
    lhs_width: u32,
    rhs_width: u32,
    assign_span: lyra_source::Span,
    lhs_span: lyra_source::Span,
    rhs_span: lyra_source::Span,
) -> lyra_diag::Diagnostic {
    let width_args = || {
        vec![
            lyra_diag::Arg::Width(lhs_width),
            lyra_diag::Arg::Width(rhs_width),
        ]
    };
    lyra_diag::Diagnostic::new(
        lyra_diag::Severity::Warning,
        lyra_diag::DiagnosticCode::WIDTH_MISMATCH,
        lyra_diag::Message::new(lyra_diag::MessageId::WidthMismatch, width_args()),
    )
    .with_label(lyra_diag::Label {
        kind: lyra_diag::LabelKind::Primary,
        span: assign_span,
        message: lyra_diag::Message::new(lyra_diag::MessageId::WidthMismatch, width_args()),
    })
    .with_label(lyra_diag::Label {
        kind: lyra_diag::LabelKind::Secondary,
        span: lhs_span,
        message: lyra_diag::Message::new(
            lyra_diag::MessageId::BitsWide,
            vec![lyra_diag::Arg::Width(lhs_width)],
        ),
    })
    .with_label(lyra_diag::Label {
        kind: lyra_diag::LabelKind::Secondary,
        span: rhs_span,
        message: lyra_diag::Message::new(
            lyra_diag::MessageId::BitsWide,
            vec![lyra_diag::Arg::Width(rhs_width)],
        ),
    })
}

fn enum_assign_diag(
    msg_id: lyra_diag::MessageId,
    rhs_label: smol_str::SmolStr,
    lhs_name: smol_str::SmolStr,
    rhs_span: lyra_source::Span,
    lhs_span: lyra_source::Span,
) -> lyra_diag::Diagnostic {
    lyra_diag::Diagnostic::new(
        lyra_diag::Severity::Error,
        lyra_diag::DiagnosticCode::ENUM_ASSIGN_INCOMPAT,
        lyra_diag::Message::new(
            msg_id,
            vec![
                lyra_diag::Arg::Name(rhs_label.clone()),
                lyra_diag::Arg::Name(lhs_name.clone()),
            ],
        ),
    )
    .with_label(lyra_diag::Label {
        kind: lyra_diag::LabelKind::Primary,
        span: rhs_span,
        message: lyra_diag::Message::new(
            msg_id,
            vec![
                lyra_diag::Arg::Name(rhs_label),
                lyra_diag::Arg::Name(lhs_name.clone()),
            ],
        ),
    })
    .with_label(lyra_diag::Label {
        kind: lyra_diag::LabelKind::Secondary,
        span: lhs_span,
        message: lyra_diag::Message::new(
            lyra_diag::MessageId::EnumTypeHere,
            vec![lyra_diag::Arg::Name(lhs_name)],
        ),
    })
}

fn lower_method_call_error(
    item: &TypeCheckItem,
    source_map: &lyra_preprocess::SourceMap,
    diags: &mut Vec<lyra_diag::Diagnostic>,
) {
    use lyra_semantic::type_infer::ExprTypeErrorKind;

    let TypeCheckItem::MethodCallError {
        call_name_span,
        method_name,
        error_kind,
    } = item
    else {
        return;
    };
    let Some(call_span) = source_map.map_span(call_name_span.text_range()) else {
        return;
    };
    let msg_id = match error_kind {
        ExprTypeErrorKind::UnknownMember => lyra_diag::MessageId::MethodUnknown,
        ExprTypeErrorKind::NoMembersOnReceiver => lyra_diag::MessageId::MethodNoMethodsOnType,
        ExprTypeErrorKind::MethodArityMismatch => lyra_diag::MessageId::MethodArityMismatch,
        ExprTypeErrorKind::MethodArgTypeMismatch
        | ExprTypeErrorKind::MethodArgNotIntegral
        | ExprTypeErrorKind::MethodNotValidOnReceiver(_) => {
            lyra_diag::MessageId::MethodArgTypeMismatch
        }
        _ => return,
    };
    let msg_args = vec![lyra_diag::Arg::Name(method_name.clone())];
    diags.push(
        lyra_diag::Diagnostic::new(
            lyra_diag::Severity::Error,
            lyra_diag::DiagnosticCode::METHOD_CALL_ERROR,
            lyra_diag::Message::new(msg_id, msg_args.clone()),
        )
        .with_label(lyra_diag::Label {
            kind: lyra_diag::LabelKind::Primary,
            span: call_span,
            message: lyra_diag::Message::new(msg_id, msg_args),
        }),
    );
}

fn lower_bits_non_data_type(
    item: &TypeCheckItem,
    source_map: &lyra_preprocess::SourceMap,
    diags: &mut Vec<lyra_diag::Diagnostic>,
) {
    let TypeCheckItem::BitsNonDataType {
        call_site,
        arg_site,
    } = item
    else {
        return;
    };
    let Some(call_span) = source_map.map_span(call_site.text_range()) else {
        return;
    };
    let arg_span = source_map
        .map_span(arg_site.text_range())
        .unwrap_or(call_span);
    diags.push(
        lyra_diag::Diagnostic::new(
            lyra_diag::Severity::Error,
            lyra_diag::DiagnosticCode::BITS_NON_DATA_TYPE,
            lyra_diag::Message::simple(lyra_diag::MessageId::BitsNonDataType),
        )
        .with_label(lyra_diag::Label {
            kind: lyra_diag::LabelKind::Primary,
            span: arg_span,
            message: lyra_diag::Message::simple(lyra_diag::MessageId::NotADataType),
        }),
    );
}

fn lower_stream_with_non_array(
    item: &TypeCheckItem,
    source_map: &lyra_preprocess::SourceMap,
    diags: &mut Vec<lyra_diag::Diagnostic>,
) {
    let TypeCheckItem::StreamWithNonArray { with_site } = item else {
        return;
    };
    let Some(with_span) = source_map.map_span(with_site.text_range()) else {
        return;
    };
    diags.push(
        lyra_diag::Diagnostic::new(
            lyra_diag::Severity::Error,
            lyra_diag::DiagnosticCode::STREAM_WITH_NON_ARRAY,
            lyra_diag::Message::simple(lyra_diag::MessageId::StreamWithNonArray),
        )
        .with_label(lyra_diag::Label {
            kind: lyra_diag::LabelKind::Primary,
            span: with_span,
            message: lyra_diag::Message::simple(lyra_diag::MessageId::StreamWithNonArray),
        }),
    );
}

fn conversion_diag(
    msg_id: lyra_diag::MessageId,
    msg_args: Vec<lyra_diag::Arg>,
    arg_span: lyra_source::Span,
) -> lyra_diag::Diagnostic {
    lyra_diag::Diagnostic::new(
        lyra_diag::Severity::Error,
        lyra_diag::DiagnosticCode::CONVERSION_ARG_TYPE,
        lyra_diag::Message::new(msg_id, msg_args.clone()),
    )
    .with_label(lyra_diag::Label {
        kind: lyra_diag::LabelKind::Primary,
        span: arg_span,
        message: lyra_diag::Message::new(msg_id, msg_args),
    })
}

fn anchor_span(
    anchor: lyra_ast::ErasedAstId,
    file_id: lyra_source::FileId,
    ast_id_map: &lyra_ast::AstIdMap,
    source_map: &lyra_preprocess::SourceMap,
) -> Option<lyra_source::Span> {
    let range = ast_id_map.range_of(anchor)?;
    let (span, _) = crate::lower_diag::map_span_or_fallback(file_id, source_map, range);
    Some(span)
}

fn error_with_primary(
    code: lyra_diag::DiagnosticCode,
    msg_id: lyra_diag::MessageId,
    msg_args: Vec<lyra_diag::Arg>,
    span: lyra_source::Span,
) -> lyra_diag::Diagnostic {
    lyra_diag::Diagnostic::new(
        lyra_diag::Severity::Error,
        code,
        lyra_diag::Message::new(msg_id, msg_args.clone()),
    )
    .with_label(lyra_diag::Label {
        kind: lyra_diag::LabelKind::Primary,
        span,
        message: lyra_diag::Message::new(msg_id, msg_args),
    })
}

fn lower_enum_value_diag(
    vd: &lyra_semantic::enum_def::EnumValueDiag,
    file_id: lyra_source::FileId,
    ast_id_map: &lyra_ast::AstIdMap,
    source_map: &lyra_preprocess::SourceMap,
) -> Option<lyra_diag::Diagnostic> {
    use lyra_semantic::enum_def::EnumValueDiag;

    match vd {
        EnumValueDiag::DuplicateValue {
            anchor,
            original,
            value,
            member_name,
        } => {
            let span = anchor_span(*anchor, file_id, ast_id_map, source_map)?;
            let msg_args = vec![
                lyra_diag::Arg::Name(smol_str::SmolStr::new(value.to_string())),
                lyra_diag::Arg::Name(member_name.clone()),
            ];
            let mut d = error_with_primary(
                lyra_diag::DiagnosticCode::ENUM_DUPLICATE_VALUE,
                lyra_diag::MessageId::EnumDuplicateValue,
                msg_args,
                span,
            );
            if let Some(orig_range) = ast_id_map.range_of(*original)
                && let Some(orig_span) = source_map.map_span(orig_range)
            {
                d = d.with_label(lyra_diag::Label {
                    kind: lyra_diag::LabelKind::Secondary,
                    span: orig_span,
                    message: lyra_diag::Message::simple(
                        lyra_diag::MessageId::EnumDuplicateOriginalHere,
                    ),
                });
            }
            Some(d)
        }
        EnumValueDiag::Overflow {
            anchor,
            value,
            width,
            signed,
            member_name,
        } => {
            let span = anchor_span(*anchor, file_id, ast_id_map, source_map)?;
            let sign_str = if *signed { "signed" } else { "unsigned" };
            let msg_args = vec![
                lyra_diag::Arg::Name(smol_str::SmolStr::new(value.to_string())),
                lyra_diag::Arg::Name(member_name.clone()),
                lyra_diag::Arg::Width(*width),
                lyra_diag::Arg::Name(smol_str::SmolStr::new(sign_str)),
            ];
            Some(error_with_primary(
                lyra_diag::DiagnosticCode::ENUM_VALUE_OVERFLOW,
                lyra_diag::MessageId::EnumValueOverflow,
                msg_args,
                span,
            ))
        }
        EnumValueDiag::SizedLiteralWidth {
            anchor,
            literal_width,
            base_width,
        } => {
            let span = anchor_span(*anchor, file_id, ast_id_map, source_map)?;
            let msg_args = vec![
                lyra_diag::Arg::Width(*literal_width),
                lyra_diag::Arg::Width(*base_width),
            ];
            Some(error_with_primary(
                lyra_diag::DiagnosticCode::ENUM_SIZED_LITERAL_WIDTH,
                lyra_diag::MessageId::EnumSizedLiteralWidthMismatch,
                msg_args,
                span,
            ))
        }
    }
}

struct DbTypeCheckCtx<'a> {
    db: &'a dyn salsa::Database,
    unit: CompilationUnit,
    source_file: SourceFile,
    ast_id_map: &'a lyra_ast::AstIdMap,
}

impl TypeCheckCtx for DbTypeCheckCtx<'_> {
    fn file_id(&self) -> lyra_source::FileId {
        self.source_file.file_id(self.db)
    }

    fn ast_id_map(&self) -> &AstIdMap {
        self.ast_id_map
    }

    fn expr_type(&self, expr: &Expr) -> ExprType {
        let Some(ast_id) = self.ast_id_map.erased_ast_id(expr.syntax()) else {
            return ExprType::error(lyra_semantic::type_infer::ExprTypeErrorKind::Unresolved);
        };
        let expr_ref = ExprRef::new(self.db, self.unit, ast_id);
        crate::expr_queries::type_of_expr(self.db, expr_ref)
    }

    fn expr_type_in_ctx(&self, expr: &Expr, ctx: &IntegralCtx) -> ExprType {
        let Some(ast_id) = self.ast_id_map.erased_ast_id(expr.syntax()) else {
            return ExprType::error(lyra_semantic::type_infer::ExprTypeErrorKind::Unresolved);
        };
        let expr_ref = ExprRef::new(self.db, self.unit, ast_id);
        let ctx_key = IntegralCtxKey::new(self.db, ctx.width, ctx.signed, ctx.four_state);
        crate::expr_queries::type_of_expr_in_ctx(self.db, expr_ref, ctx_key)
    }

    fn expr_type_stmt(&self, expr: &Expr) -> ExprType {
        let Some(ast_id) = self.ast_id_map.erased_ast_id(expr.syntax()) else {
            return ExprType::error(lyra_semantic::type_infer::ExprTypeErrorKind::Unresolved);
        };
        let expr_ref = ExprRef::new(self.db, self.unit, ast_id);
        crate::expr_queries::type_of_expr_stmt(self.db, expr_ref)
    }

    fn symbol_type_of_declarator(&self, declarator: &lyra_ast::Declarator) -> Option<SymbolType> {
        let ast_id = self.ast_id_map.id_of(declarator)?;
        let def = def_index_file(self.db, self.source_file);
        let sym_id = def.name_site_to_symbol.get(&ast_id).copied()?;
        let gsym = lyra_semantic::symbols::GlobalSymbolId {
            file: self.source_file.file_id(self.db),
            local: sym_id,
        };
        let sym_ref = SymbolRef::new(self.db, self.unit, gsym);
        Some(type_of_symbol(self.db, sym_ref))
    }

    fn resolve_type_arg(
        &self,
        utr: &lyra_semantic::UserTypeRef,
    ) -> Option<lyra_semantic::types::Ty> {
        crate::resolve_helpers::resolve_type_arg_impl(
            self.db,
            self.unit,
            self.source_file,
            self.ast_id_map,
            utr,
        )
    }

    fn const_eval_int(&self, expr: &Expr) -> Option<i64> {
        let ast_id = self.ast_id_map.erased_ast_id(expr.syntax())?;
        let expr_ref = crate::const_eval::ConstExprRef::new(self.db, self.unit, ast_id);
        match crate::const_eval::eval_const_int(self.db, expr_ref) {
            lyra_semantic::types::ConstInt::Known(v) => Some(v),
            _ => None,
        }
    }

    fn enum_known_value_set(
        &self,
        id: &lyra_semantic::enum_def::EnumId,
    ) -> Option<std::sync::Arc<[i64]>> {
        let eref = EnumRef::new(self.db, self.unit, *id);
        crate::enum_queries::enum_known_value_set(self.db, eref)
    }

    fn is_modport_target_lvalue(&self, expr_id: lyra_ast::ErasedAstId) -> bool {
        let file_id = expr_id.file();
        let Some(src) = source_file_by_id(self.db, self.unit, file_id) else {
            return false;
        };
        expr_is_assignable_ref(self.db, src, expr_id)
    }
}

/// Check whether a modport expression target is an assignable reference.
/// Modport ports require a concrete variable, not a concat or stream.
fn expr_is_assignable_ref(
    db: &dyn salsa::Database,
    file: SourceFile,
    expr_id: lyra_ast::ErasedAstId,
) -> bool {
    let parse = parse_file(db, file);
    let map = ast_id_map(db, file);
    let Some(node) = map.get_node(&parse.syntax(), expr_id) else {
        return false;
    };
    let Some(expr) = Expr::cast(node) else {
        return false;
    };
    lyra_semantic::lhs::is_assignable_ref(&expr)
}

/// Unit-level diagnostics: duplicate definitions in the definitions namespace.
///
/// Walks `GlobalDefIndex.definitions()`, finds adjacent entries with the
/// same name, and emits one diagnostic per duplicate. Catches module/module,
/// package/package, and module/package name collisions.
#[salsa::tracked(return_ref)]
pub fn unit_diagnostics(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
) -> Box<[lyra_diag::Diagnostic]> {
    let global = global_def_index(db, unit);
    let defs = global.definitions();
    let mut diags = Vec::new();

    let mut i = 0;
    while i < defs.len() {
        let (name, _, _) = &defs[i];
        let mut j = i + 1;
        while j < defs.len() && defs[j].0 == *name {
            j += 1;
        }
        if j - i > 1 {
            // Duplicate group: emit diagnostics for entries [i+1..j]
            for (_, dup_def_id, _) in &defs[(i + 1)..j] {
                let dup_file_id = dup_def_id.file();
                if let Some(dup_file) = source_file_by_id(db, unit, dup_file_id) {
                    let dup_def = def_index_file(db, dup_file);
                    if let Some(entry) = dup_def.def_entry(*dup_def_id) {
                        let pp = preprocess_file(db, dup_file);
                        let label_range = entry.name_span.text_range();
                        if let Some(span) = pp.source_map.map_span(label_range) {
                            diags.push(
                                lyra_diag::Diagnostic::new(
                                    lyra_diag::Severity::Error,
                                    lyra_diag::DiagnosticCode::DUPLICATE_DEFINITION,
                                    lyra_diag::Message::new(
                                        lyra_diag::MessageId::DuplicateDefinitionInUnit,
                                        vec![lyra_diag::Arg::Name(name.clone())],
                                    ),
                                )
                                .with_label(lyra_diag::Label {
                                    kind: lyra_diag::LabelKind::Primary,
                                    span,
                                    message: lyra_diag::Message::simple(
                                        lyra_diag::MessageId::RedefinedHere,
                                    ),
                                }),
                            );
                        }
                    }
                }
            }
        }
        i = j;
    }

    diags.into_boxed_slice()
}
