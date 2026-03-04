use lyra_semantic::type_check::TypeCheckItem;

use crate::semantic::def_index_file;
use crate::{CompilationUnit, source_file_by_id};

pub(super) fn lower_enum_cast_item(
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
            lyra_diag::code::ENUM_CAST_OUT_OF_RANGE,
            lyra_diag::Message::new(lyra_diag::MessageId::EnumCastOutOfRange, msg_args.clone()),
        )
        .with_label(lyra_diag::Label {
            kind: lyra_diag::LabelKind::Primary,
            span: cast_span,
            message: lyra_diag::Message::new(lyra_diag::MessageId::EnumCastOutOfRange, msg_args),
        }),
    );
}

pub(super) fn lower_enum_assign_item(
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

pub(super) fn lower_enum_value_diag(
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
                lyra_diag::code::ENUM_DUPLICATE_VALUE,
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
                lyra_diag::code::ENUM_VALUE_OVERFLOW,
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
                lyra_diag::code::ENUM_SIZED_LITERAL_WIDTH,
                lyra_diag::MessageId::EnumSizedLiteralWidthMismatch,
                msg_args,
                span,
            ))
        }
    }
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

fn enum_assign_diag(
    msg_id: lyra_diag::MessageId,
    rhs_label: smol_str::SmolStr,
    lhs_name: smol_str::SmolStr,
    rhs_span: lyra_source::Span,
    lhs_span: lyra_source::Span,
) -> lyra_diag::Diagnostic {
    lyra_diag::Diagnostic::new(
        lyra_diag::Severity::Error,
        lyra_diag::code::ENUM_ASSIGN_INCOMPAT,
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
    code: lyra_diag::DiagKey,
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
