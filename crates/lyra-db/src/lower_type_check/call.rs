use lyra_semantic::type_check::TypeCheckItem;

pub(super) fn lower_method_call_error(
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
        ExprTypeErrorKind::WithClauseRequired => lyra_diag::MessageId::MethodWithClauseRequired,
        ExprTypeErrorKind::WithClauseNotAccepted => {
            lyra_diag::MessageId::MethodWithClauseNotAccepted
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

pub(super) fn lower_bits_non_data_type(
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

pub(super) fn lower_conversion_item(
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

pub(super) fn lower_new_expr_item(
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

pub(super) fn lower_new_expr_init_incompat(
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
