use lyra_semantic::type_check::TypeCheckItem;

pub(super) fn lower_stream_with_non_array(
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
            lyra_diag::code::STREAM_WITH_NON_ARRAY,
            lyra_diag::Message::simple(lyra_diag::MessageId::StreamWithNonArray),
        )
        .with_label(lyra_diag::Label {
            kind: lyra_diag::LabelKind::Primary,
            span: with_span,
            message: lyra_diag::Message::simple(lyra_diag::MessageId::StreamWithNonArray),
        }),
    );
}

pub(super) fn lower_stream_slice_size_not_const(
    item: &TypeCheckItem,
    source_map: &lyra_preprocess::SourceMap,
    diags: &mut Vec<lyra_diag::Diagnostic>,
) {
    let TypeCheckItem::StreamSliceSizeNotConst { slice_size_site } = item else {
        return;
    };
    let Some(span) = source_map.map_span(slice_size_site.text_range()) else {
        return;
    };
    diags.push(
        lyra_diag::Diagnostic::new(
            lyra_diag::Severity::Error,
            lyra_diag::code::STREAM_SLICE_SIZE_NOT_CONST,
            lyra_diag::Message::simple(lyra_diag::MessageId::StreamSliceSizeNotConst),
        )
        .with_label(lyra_diag::Label {
            kind: lyra_diag::LabelKind::Primary,
            span,
            message: lyra_diag::Message::simple(lyra_diag::MessageId::StreamSliceSizeNotConst),
        }),
    );
}

pub(super) fn lower_stream_unpack_operand(
    item: &TypeCheckItem,
    source_map: &lyra_preprocess::SourceMap,
    diags: &mut Vec<lyra_diag::Diagnostic>,
) {
    let (site, code, msg_id, msg_args) = match item {
        TypeCheckItem::StreamUnpackOperandInvalid { operand_site } => (
            *operand_site,
            lyra_diag::code::STREAM_UNPACK_OPERAND_INVALID,
            lyra_diag::MessageId::StreamUnpackOperandInvalid,
            vec![],
        ),
        TypeCheckItem::StreamUnpackOperandUnsupported {
            operand_site,
            operand_ty,
        } => (
            *operand_site,
            lyra_diag::code::STREAM_UNPACK_OPERAND_UNSUPPORTED,
            lyra_diag::MessageId::StreamUnpackOperandUnsupported,
            vec![lyra_diag::Arg::Name(operand_ty.pretty())],
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
            lyra_diag::Message::new(msg_id, msg_args.clone()),
        )
        .with_label(lyra_diag::Label {
            kind: lyra_diag::LabelKind::Primary,
            span,
            message: lyra_diag::Message::new(msg_id, msg_args),
        }),
    );
}

pub(super) fn lower_stream_unpack_greedy_remainder(
    item: &TypeCheckItem,
    source_map: &lyra_preprocess::SourceMap,
    diags: &mut Vec<lyra_diag::Diagnostic>,
) {
    let TypeCheckItem::StreamUnpackGreedyRemainder {
        assign_site,
        greedy_site,
        remaining,
        elem_width,
        ..
    } = item
    else {
        return;
    };
    let Some(assign_span) = source_map.map_span(assign_site.text_range()) else {
        return;
    };
    let greedy_span = source_map
        .map_span(greedy_site.text_range())
        .unwrap_or(assign_span);
    let msg = lyra_diag::Message::new(
        lyra_diag::MessageId::StreamUnpackGreedyRemainder,
        vec![
            lyra_diag::Arg::Width(*remaining),
            lyra_diag::Arg::Width(*elem_width),
        ],
    );
    diags.push(
        lyra_diag::Diagnostic::new(
            lyra_diag::Severity::Error,
            lyra_diag::code::STREAM_UNPACK_GREEDY_REMAINDER,
            msg.clone(),
        )
        .with_label(lyra_diag::Label {
            kind: lyra_diag::LabelKind::Primary,
            span: greedy_span,
            message: msg,
        }),
    );
}

pub(super) fn lower_stream_unpack_width_mismatch(
    item: &TypeCheckItem,
    source_map: &lyra_preprocess::SourceMap,
    diags: &mut Vec<lyra_diag::Diagnostic>,
) {
    let TypeCheckItem::StreamUnpackWidthMismatch {
        assign_site,
        op_range,
        lhs_site,
        rhs_site,
        lhs_width,
        rhs_width,
    } = item
    else {
        return;
    };
    let primary_range = op_range.unwrap_or_else(|| assign_site.text_range());
    let Some(assign_span) = source_map.map_span(primary_range) else {
        return;
    };
    let lhs_span = source_map
        .map_span(lhs_site.text_range())
        .unwrap_or(assign_span);
    let rhs_span = source_map
        .map_span(rhs_site.text_range())
        .unwrap_or(assign_span);
    let width_args = || {
        vec![
            lyra_diag::Arg::Width(*lhs_width),
            lyra_diag::Arg::Width(*rhs_width),
        ]
    };
    diags.push(
        lyra_diag::Diagnostic::new(
            lyra_diag::Severity::Error,
            lyra_diag::code::STREAM_UNPACK_WIDTH_MISMATCH,
            lyra_diag::Message::new(
                lyra_diag::MessageId::StreamUnpackWidthMismatch,
                width_args(),
            ),
        )
        .with_label(lyra_diag::Label {
            kind: lyra_diag::LabelKind::Primary,
            span: assign_span,
            message: lyra_diag::Message::new(
                lyra_diag::MessageId::StreamUnpackWidthMismatch,
                width_args(),
            ),
        })
        .with_label(lyra_diag::Label {
            kind: lyra_diag::LabelKind::Secondary,
            span: lhs_span,
            message: lyra_diag::Message::new(
                lyra_diag::MessageId::BitsWide,
                vec![lyra_diag::Arg::Width(*lhs_width)],
            ),
        })
        .with_label(lyra_diag::Label {
            kind: lyra_diag::LabelKind::Secondary,
            span: rhs_span,
            message: lyra_diag::Message::new(
                lyra_diag::MessageId::BitsWide,
                vec![lyra_diag::Arg::Width(*rhs_width)],
            ),
        }),
    );
}
