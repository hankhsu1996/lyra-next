use lyra_semantic::record::{RecordId, RecordKind};
use lyra_semantic::type_check::TypeCheckItem;

use crate::semantic::def_index_file;
use crate::{CompilationUnit, source_file_by_id};

pub(super) fn lower_record_assign_item(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    item: &TypeCheckItem,
    source_map: &lyra_preprocess::SourceMap,
    diags: &mut Vec<lyra_diag::Diagnostic>,
) {
    let TypeCheckItem::RecordAssignWrongRecord {
        assign_site,
        lhs_site,
        rhs_site,
        lhs_record,
        rhs_record,
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
    let lhs_span = source_map
        .map_span(lhs_site.text_range())
        .unwrap_or(assign_span);

    let (lhs_kind, lhs_name) = record_display_name(db, unit, lhs_record);
    let (rhs_kind, rhs_name) = record_display_name(db, unit, rhs_record);

    let lhs_kind_str = record_kind_str(lhs_kind);
    let rhs_kind_str = record_kind_str(rhs_kind);

    // Args: [rhs_kind, rhs_name, lhs_kind, lhs_name] (contract with message.rs)
    let primary_args = vec![
        lyra_diag::Arg::Name(smol_str::SmolStr::new(rhs_kind_str)),
        lyra_diag::Arg::Name(rhs_name.clone()),
        lyra_diag::Arg::Name(smol_str::SmolStr::new(lhs_kind_str)),
        lyra_diag::Arg::Name(lhs_name.clone()),
    ];

    diags.push(
        lyra_diag::Diagnostic::new(
            lyra_diag::Severity::Error,
            lyra_diag::code::RECORD_ASSIGN_INCOMPAT,
            lyra_diag::Message::new(
                lyra_diag::MessageId::RecordAssignWrongRecord,
                primary_args.clone(),
            ),
        )
        .with_label(lyra_diag::Label {
            kind: lyra_diag::LabelKind::Primary,
            span: rhs_span,
            message: lyra_diag::Message::new(
                lyra_diag::MessageId::RecordAssignWrongRecord,
                primary_args,
            ),
        })
        .with_label(lyra_diag::Label {
            kind: lyra_diag::LabelKind::Secondary,
            span: lhs_span,
            message: lyra_diag::Message::new(
                lyra_diag::MessageId::RecordTypeHere,
                vec![
                    lyra_diag::Arg::Name(smol_str::SmolStr::new(lhs_kind_str)),
                    lyra_diag::Arg::Name(lhs_name),
                ],
            ),
        }),
    );
}

pub(super) fn lower_unpacked_record_integral_assign(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    item: &TypeCheckItem,
    source_map: &lyra_preprocess::SourceMap,
    diags: &mut Vec<lyra_diag::Diagnostic>,
) {
    let TypeCheckItem::UnpackedRecordIntegralAssign {
        assign_site,
        lhs_site,
        rhs_site,
        record_id,
        other_ty,
        record_is_lhs,
    } = item
    else {
        return;
    };
    let Some(assign_span) = source_map.map_span(assign_site.text_range()) else {
        return;
    };

    let record_site = if *record_is_lhs { lhs_site } else { rhs_site };
    let record_span = source_map
        .map_span(record_site.text_range())
        .unwrap_or(assign_span);

    let (kind, name) = record_display_name(db, unit, record_id);
    let kind_str = record_kind_str(kind);
    let other_name = other_ty.pretty();

    let args = vec![
        lyra_diag::Arg::Name(smol_str::SmolStr::new(kind_str)),
        lyra_diag::Arg::Name(name.clone()),
        lyra_diag::Arg::Name(other_name),
    ];

    diags.push(
        lyra_diag::Diagnostic::new(
            lyra_diag::Severity::Error,
            lyra_diag::code::UNPACKED_RECORD_INTEGRAL_ASSIGN,
            lyra_diag::Message::new(
                lyra_diag::MessageId::UnpackedRecordIntegralAssign,
                args.clone(),
            ),
        )
        .with_label(lyra_diag::Label {
            kind: lyra_diag::LabelKind::Primary,
            span: assign_span,
            message: lyra_diag::Message::new(
                lyra_diag::MessageId::UnpackedRecordIntegralAssign,
                args,
            ),
        })
        .with_label(lyra_diag::Label {
            kind: lyra_diag::LabelKind::Secondary,
            span: record_span,
            message: lyra_diag::Message::new(
                lyra_diag::MessageId::RecordTypeHere,
                vec![
                    lyra_diag::Arg::Name(smol_str::SmolStr::new(kind_str)),
                    lyra_diag::Arg::Name(name),
                ],
            ),
        }),
    );
}

fn record_display_name(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    id: &RecordId,
) -> (RecordKind, smol_str::SmolStr) {
    let fallback_kind = RecordKind::Struct;
    let fallback_name = smol_str::SmolStr::new_static("<anonymous>");
    let Some(sf) = source_file_by_id(db, unit, id.file()) else {
        return (fallback_kind, fallback_name);
    };
    let def = def_index_file(db, sf);
    match def.record_def_by_id(*id) {
        Some(record_def) => {
            let name = record_def
                .name
                .clone()
                .unwrap_or_else(|| fallback_name.clone());
            (record_def.kind, name)
        }
        None => (fallback_kind, fallback_name),
    }
}

fn record_kind_str(kind: RecordKind) -> &'static str {
    match kind {
        RecordKind::Struct => "struct",
        RecordKind::Union | RecordKind::TaggedUnion => "union",
    }
}
