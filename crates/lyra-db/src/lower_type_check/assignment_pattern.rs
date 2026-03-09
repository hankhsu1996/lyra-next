use lyra_semantic::type_check::TypeCheckItem;

pub(super) fn lower_assignment_pattern_item(
    item: &TypeCheckItem,
    source_map: &lyra_preprocess::SourceMap,
    diags: &mut Vec<lyra_diag::Diagnostic>,
) {
    match item {
        TypeCheckItem::AssignPatternPositionalInAssocArray { item_site } => {
            let Some(span) = source_map.map_span(item_site.text_range()) else {
                return;
            };
            diags.push(
                lyra_diag::Diagnostic::new(
                    lyra_diag::Severity::Error,
                    lyra_diag::code::ASSIGN_PATTERN_POSITIONAL_IN_ASSOC,
                    lyra_diag::Message::simple(
                        lyra_diag::MessageId::AssignPatternPositionalInAssocArray,
                    ),
                )
                .with_label(lyra_diag::Label {
                    kind: lyra_diag::LabelKind::Primary,
                    span,
                    message: lyra_diag::Message::simple(
                        lyra_diag::MessageId::AssignPatternPositionalInAssocArray,
                    ),
                }),
            );
        }
        TypeCheckItem::AssignPatternDuplicateDefault { item_site } => {
            let Some(span) = source_map.map_span(item_site.text_range()) else {
                return;
            };
            diags.push(
                lyra_diag::Diagnostic::new(
                    lyra_diag::Severity::Error,
                    lyra_diag::code::ASSIGN_PATTERN_DUPLICATE_DEFAULT,
                    lyra_diag::Message::simple(lyra_diag::MessageId::AssignPatternDuplicateDefault),
                )
                .with_label(lyra_diag::Label {
                    kind: lyra_diag::LabelKind::Primary,
                    span,
                    message: lyra_diag::Message::simple(
                        lyra_diag::MessageId::AssignPatternDuplicateDefault,
                    ),
                }),
            );
        }
        TypeCheckItem::AssignPatternKeyTypeMismatch {
            item_site,
            expected,
            actual,
        } => {
            let Some(span) = source_map.map_span(item_site.text_range()) else {
                return;
            };
            let args = vec![
                lyra_diag::Arg::Name(expected.pretty()),
                lyra_diag::Arg::Name(actual.pretty()),
            ];
            diags.push(
                lyra_diag::Diagnostic::new(
                    lyra_diag::Severity::Error,
                    lyra_diag::code::ASSIGN_PATTERN_KEY_TYPE_MISMATCH,
                    lyra_diag::Message::new(
                        lyra_diag::MessageId::AssignPatternKeyTypeMismatch,
                        args.clone(),
                    ),
                )
                .with_label(lyra_diag::Label {
                    kind: lyra_diag::LabelKind::Primary,
                    span,
                    message: lyra_diag::Message::new(
                        lyra_diag::MessageId::AssignPatternKeyTypeMismatch,
                        args,
                    ),
                }),
            );
        }
        _ => {}
    }
}
