use lyra_semantic::modport_def::PortDirection;
use lyra_semantic::type_check::{AccessKind, TypeCheckItem};

pub(super) fn lower_modport_item(
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

pub(super) fn lower_lhs_item(
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

pub(super) fn lower_internal_type_check_error(
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

pub(super) fn lower_assign_to_readonly(
    item: &TypeCheckItem,
    source_map: &lyra_preprocess::SourceMap,
    diags: &mut Vec<lyra_diag::Diagnostic>,
) {
    let TypeCheckItem::AssignToReadonly {
        kind,
        assign_site,
        lhs_name_span,
        name,
    } = item
    else {
        return;
    };
    let Some(assign_span) = source_map.map_span(assign_site.text_range()) else {
        return;
    };
    let lhs_span = source_map
        .map_span(lhs_name_span.text_range())
        .unwrap_or(assign_span);
    let (code, msg_id) = match kind {
        lyra_semantic::type_check::ReadonlyKind::Const => (
            lyra_diag::DiagnosticCode::ASSIGN_TO_CONST,
            lyra_diag::MessageId::AssignToConst,
        ),
    };
    let msg_args = vec![lyra_diag::Arg::Name(name.clone())];
    diags.push(
        lyra_diag::Diagnostic::new(
            lyra_diag::Severity::Error,
            code,
            lyra_diag::Message::new(msg_id, msg_args.clone()),
        )
        .with_label(lyra_diag::Label {
            kind: lyra_diag::LabelKind::Primary,
            span: lhs_span,
            message: lyra_diag::Message::new(msg_id, msg_args),
        }),
    );
}

pub(super) fn lower_const_missing_init(
    item: &TypeCheckItem,
    source_map: &lyra_preprocess::SourceMap,
    diags: &mut Vec<lyra_diag::Diagnostic>,
) {
    let TypeCheckItem::ConstMissingInit {
        decl_site,
        name_span,
    } = item
    else {
        return;
    };
    let Some(decl_span) = source_map.map_span(decl_site.text_range()) else {
        return;
    };
    let span = source_map
        .map_span(name_span.text_range())
        .unwrap_or(decl_span);
    diags.push(
        lyra_diag::Diagnostic::new(
            lyra_diag::Severity::Error,
            lyra_diag::DiagnosticCode::CONST_MISSING_INIT,
            lyra_diag::Message::simple(lyra_diag::MessageId::ConstMissingInit),
        )
        .with_label(lyra_diag::Label {
            kind: lyra_diag::LabelKind::Primary,
            span,
            message: lyra_diag::Message::simple(lyra_diag::MessageId::ConstMissingInit),
        }),
    );
}

pub(super) fn lower_void_object_type(
    decl_site: lyra_semantic::Site,
    name_span: lyra_source::NameSpan,
    source_map: &lyra_preprocess::SourceMap,
    diags: &mut Vec<lyra_diag::Diagnostic>,
) {
    let Some(decl_span) = source_map.map_span(decl_site.text_range()) else {
        return;
    };
    let span = source_map
        .map_span(name_span.text_range())
        .unwrap_or(decl_span);
    diags.push(
        lyra_diag::Diagnostic::new(
            lyra_diag::Severity::Error,
            lyra_diag::DiagnosticCode::VOID_OBJECT_TYPE,
            lyra_diag::Message::simple(lyra_diag::MessageId::VoidObjectType),
        )
        .with_label(lyra_diag::Label {
            kind: lyra_diag::LabelKind::Primary,
            span,
            message: lyra_diag::Message::simple(lyra_diag::MessageId::VoidObjectType),
        }),
    );
}

pub(super) fn lower_void_used_as_value(
    expr_site: lyra_semantic::Site,
    source_map: &lyra_preprocess::SourceMap,
    diags: &mut Vec<lyra_diag::Diagnostic>,
) {
    let Some(span) = source_map.map_span(expr_site.text_range()) else {
        return;
    };
    diags.push(
        lyra_diag::Diagnostic::new(
            lyra_diag::Severity::Error,
            lyra_diag::DiagnosticCode::VOID_USED_AS_VALUE,
            lyra_diag::Message::simple(lyra_diag::MessageId::VoidUsedAsValue),
        )
        .with_label(lyra_diag::Label {
            kind: lyra_diag::LabelKind::Primary,
            span,
            message: lyra_diag::Message::simple(lyra_diag::MessageId::VoidUsedAsValue),
        }),
    );
}

pub(super) fn lower_illegal_drive_strength(
    strength_site: lyra_semantic::Site,
    source_map: &lyra_preprocess::SourceMap,
    diags: &mut Vec<lyra_diag::Diagnostic>,
) {
    let Some(span) = source_map.map_span(strength_site.text_range()) else {
        return;
    };
    diags.push(
        lyra_diag::Diagnostic::new(
            lyra_diag::Severity::Error,
            lyra_diag::DiagnosticCode::ILLEGAL_DRIVE_STRENGTH_BOTH_HIGHZ,
            lyra_diag::Message::simple(lyra_diag::MessageId::IllegalDriveStrengthBothHighz),
        )
        .with_label(lyra_diag::Label {
            kind: lyra_diag::LabelKind::Primary,
            span,
            message: lyra_diag::Message::simple(
                lyra_diag::MessageId::IllegalDriveStrengthBothHighz,
            ),
        }),
    );
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
