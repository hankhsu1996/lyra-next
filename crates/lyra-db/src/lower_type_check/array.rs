use lyra_semantic::type_check::TypeCheckItem;

pub(super) fn lower_assign_truncation(
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
    diags.push(truncation_diag(*lhs_width, *rhs_width, lhs_span, rhs_span));
}

pub(super) fn lower_array_incompat(
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

pub(super) fn lower_array_query_item(
    item: &TypeCheckItem,
    source_map: &lyra_preprocess::SourceMap,
    diags: &mut Vec<lyra_diag::Diagnostic>,
) {
    let (call_site, label_site, code, msg_id, fn_name) = match item {
        TypeCheckItem::ArrayQueryDynTypeForm {
            call_site,
            arg_site,
            fn_name,
        } => (
            call_site,
            arg_site,
            lyra_diag::DiagnosticCode::ARRAY_QUERY_DYN_TYPE_FORM,
            lyra_diag::MessageId::ArrayQueryDynTypeForm,
            fn_name,
        ),
        TypeCheckItem::ArrayQueryVarSizedDimByNumber {
            call_site,
            dim_arg_site,
            fn_name,
        } => (
            call_site,
            dim_arg_site,
            lyra_diag::DiagnosticCode::ARRAY_QUERY_VAR_SIZED_DIM,
            lyra_diag::MessageId::ArrayQueryVarSizedDimByNumber,
            fn_name,
        ),
        _ => return,
    };
    let Some(call_span) = source_map.map_span(call_site.text_range()) else {
        return;
    };
    let label_span = source_map
        .map_span(label_site.text_range())
        .unwrap_or(call_span);
    let msg_args = vec![lyra_diag::Arg::Name(fn_name.clone())];
    diags.push(
        lyra_diag::Diagnostic::new(
            lyra_diag::Severity::Error,
            code,
            lyra_diag::Message::new(msg_id, msg_args.clone()),
        )
        .with_label(lyra_diag::Label {
            kind: lyra_diag::LabelKind::Primary,
            span: label_span,
            message: lyra_diag::Message::new(msg_id, msg_args),
        }),
    );
}

fn truncation_diag(
    lhs_width: u32,
    rhs_width: u32,
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
        span: lhs_span,
        message: lyra_diag::Message::new(lyra_diag::MessageId::WidthMismatch, width_args()),
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
