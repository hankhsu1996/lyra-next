use lyra_semantic::modport_def::PortDirection;
use lyra_semantic::type_check::{AccessKind, TypeCheckItem};

use crate::semantic::def_index_file;
use crate::{CompilationUnit, source_file_by_id};

pub(crate) fn lower_type_check_item(
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
        TypeCheckItem::NewExprNotDynArray { .. }
        | TypeCheckItem::NewExprTooManyInitArgs { .. }
        | TypeCheckItem::NewExprSizeNotLongint { .. }
        | TypeCheckItem::NewExprSizeNegative { .. } => {
            lower_new_expr_item(item, source_map, diags);
        }
        TypeCheckItem::NewExprInitIncompat { .. } => {
            lower_new_expr_init_incompat(item, source_map, diags);
        }
        TypeCheckItem::ArrayIncompatible { .. } => {
            lower_array_incompat(item, source_map, diags);
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

pub(crate) fn lower_enum_value_diag(
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

fn lower_new_expr_item(
    item: &TypeCheckItem,
    source_map: &lyra_preprocess::SourceMap,
    diags: &mut Vec<lyra_diag::Diagnostic>,
) {
    let (site, code, msg_id) = match item {
        TypeCheckItem::NewExprNotDynArray { new_site, .. } => (
            *new_site,
            lyra_diag::DiagnosticCode::NEW_EXPR_NOT_DYN_ARRAY,
            lyra_diag::MessageId::NewExprNotDynArray,
        ),
        TypeCheckItem::NewExprTooManyInitArgs { new_site } => (
            *new_site,
            lyra_diag::DiagnosticCode::NEW_EXPR_TOO_MANY_INIT_ARGS,
            lyra_diag::MessageId::NewExprTooManyInitArgs,
        ),
        TypeCheckItem::NewExprSizeNotLongint { new_site, .. } => (
            *new_site,
            lyra_diag::DiagnosticCode::NEW_EXPR_SIZE_NOT_LONGINT,
            lyra_diag::MessageId::NewExprSizeNotLongint,
        ),
        TypeCheckItem::NewExprSizeNegative { new_site, .. } => (
            *new_site,
            lyra_diag::DiagnosticCode::NEW_EXPR_SIZE_NEGATIVE,
            lyra_diag::MessageId::NewExprSizeNegative,
        ),
        _ => return,
    };
    let Some(span) = source_map.map_span(site.text_range()) else {
        return;
    };
    diags.push(
        lyra_diag::Diagnostic::new(
            lyra_diag::Severity::Error,
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

fn lower_new_expr_init_incompat(
    item: &TypeCheckItem,
    source_map: &lyra_preprocess::SourceMap,
    diags: &mut Vec<lyra_diag::Diagnostic>,
) {
    let TypeCheckItem::NewExprInitIncompat {
        new_site,
        init_site,
        lhs_ty,
        init_ty,
        ..
    } = item
    else {
        return;
    };
    let Some(new_span) = source_map.map_span(new_site.text_range()) else {
        return;
    };
    let init_span = source_map
        .map_span(init_site.text_range())
        .unwrap_or(new_span);
    let msg_args = vec![
        lyra_diag::Arg::Name(lhs_ty.pretty()),
        lyra_diag::Arg::Name(init_ty.pretty()),
    ];
    diags.push(
        lyra_diag::Diagnostic::new(
            lyra_diag::Severity::Error,
            lyra_diag::DiagnosticCode::NEW_EXPR_INIT_INCOMPAT,
            lyra_diag::Message::new(lyra_diag::MessageId::NewExprInitIncompat, msg_args.clone()),
        )
        .with_label(lyra_diag::Label {
            kind: lyra_diag::LabelKind::Primary,
            span: init_span,
            message: lyra_diag::Message::new(lyra_diag::MessageId::NewExprInitIncompat, msg_args),
        }),
    );
}

fn lower_array_incompat(
    item: &TypeCheckItem,
    source_map: &lyra_preprocess::SourceMap,
    diags: &mut Vec<lyra_diag::Diagnostic>,
) {
    let TypeCheckItem::ArrayIncompatible {
        assign_site,
        rhs_site,
        lhs_ty,
        rhs_ty,
        ..
    } = item
    else {
        return;
    };
    let Some(assign_span) = source_map.map_span(assign_site.text_range()) else {
        return;
    };
    let rhs_span = source_map
        .map_span(rhs_site.text_range())
        .unwrap_or(assign_span);
    let msg_args = vec![
        lyra_diag::Arg::Name(lhs_ty.pretty()),
        lyra_diag::Arg::Name(rhs_ty.pretty()),
    ];
    diags.push(
        lyra_diag::Diagnostic::new(
            lyra_diag::Severity::Error,
            lyra_diag::DiagnosticCode::ARRAY_INCOMPAT,
            lyra_diag::Message::new(lyra_diag::MessageId::ArrayIncompatible, msg_args.clone()),
        )
        .with_label(lyra_diag::Label {
            kind: lyra_diag::LabelKind::Primary,
            span: rhs_span,
            message: lyra_diag::Message::new(lyra_diag::MessageId::ArrayIncompatible, msg_args),
        }),
    );
}
