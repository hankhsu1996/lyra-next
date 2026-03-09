mod array;
mod assignment_pattern;
mod call;
mod enum_diag;
mod misc;
mod record;
mod stream;

use lyra_semantic::type_check::TypeCheckItem;

pub(crate) fn lower_type_check_item(
    db: &dyn salsa::Database,
    unit: crate::CompilationUnit,
    item: &TypeCheckItem,
    source_map: &lyra_preprocess::SourceMap,
    expanded_text: &str,
    seen: &mut std::collections::HashSet<(
        lyra_ast::ErasedAstId,
        lyra_ast::ErasedAstId,
        lyra_ast::ErasedAstId,
    )>,
    diags: &mut Vec<lyra_diag::Diagnostic>,
) {
    match item {
        TypeCheckItem::AssignTruncation { .. } => {
            array::lower_assign_truncation(item, source_map, seen, diags);
        }
        TypeCheckItem::BitsNonDataType { .. } => {
            call::lower_bits_non_data_type(item, source_map, diags);
        }
        TypeCheckItem::EnumAssignFromNonEnum { .. } | TypeCheckItem::EnumAssignWrongEnum { .. } => {
            enum_diag::lower_enum_assign_item(db, unit, item, source_map, diags);
        }
        TypeCheckItem::ConversionArgCategory { .. }
        | TypeCheckItem::ConversionWidthMismatch { .. } => {
            call::lower_conversion_item(item, source_map, diags);
        }
        TypeCheckItem::ModportDirectionViolation { .. }
        | TypeCheckItem::ModportRefUnsupported { .. }
        | TypeCheckItem::ModportEmptyPortAccess { .. }
        | TypeCheckItem::ModportExprNotAssignable { .. }
        | TypeCheckItem::MemberNotInModport { .. } => {
            misc::lower_modport_item(item, source_map, expanded_text, diags);
        }
        TypeCheckItem::EnumCastOutOfRange { .. } => {
            enum_diag::lower_enum_cast_item(db, unit, item, source_map, diags);
        }
        TypeCheckItem::StreamWithNonArray { .. } => {
            stream::lower_stream_with_non_array(item, source_map, diags);
        }
        TypeCheckItem::MethodCallError { .. } => {
            call::lower_method_call_error(item, source_map, expanded_text, diags);
        }
        TypeCheckItem::AssignPatternPositionalInAssocArray { .. }
        | TypeCheckItem::AssignPatternDuplicateDefault { .. }
        | TypeCheckItem::AssignPatternKeyTypeMismatch { .. } => {
            assignment_pattern::lower_assignment_pattern_item(item, source_map, diags);
        }
        TypeCheckItem::UnsupportedLhsForm { .. } | TypeCheckItem::InvalidLhs { .. } => {
            misc::lower_lhs_item(item, source_map, diags);
        }
        TypeCheckItem::InternalError { .. } => {
            misc::lower_internal_type_check_error(item, source_map, diags);
        }
        _ => lower_type_check_item_continued(db, unit, item, source_map, diags),
    }
}

fn lower_type_check_item_continued(
    db: &dyn salsa::Database,
    unit: crate::CompilationUnit,
    item: &TypeCheckItem,
    source_map: &lyra_preprocess::SourceMap,
    diags: &mut Vec<lyra_diag::Diagnostic>,
) {
    match item {
        TypeCheckItem::NewExprNotDynArray { .. }
        | TypeCheckItem::NewExprTooManyInitArgs { .. }
        | TypeCheckItem::NewExprSizeNotLongint { .. }
        | TypeCheckItem::NewExprSizeNegative { .. } => {
            call::lower_new_expr_item(item, source_map, diags);
        }
        TypeCheckItem::NewExprInitIncompat { .. } => {
            call::lower_new_expr_init_incompat(item, source_map, diags);
        }
        TypeCheckItem::ArrayIncompatible { .. } => {
            array::lower_array_incompat(item, source_map, diags);
        }
        TypeCheckItem::StreamUnpackOperandInvalid { .. }
        | TypeCheckItem::StreamUnpackOperandUnsupported { .. } => {
            stream::lower_stream_unpack_operand(item, source_map, diags);
        }
        TypeCheckItem::StreamUnpackGreedyRemainder { .. } => {
            stream::lower_stream_unpack_greedy_remainder(item, source_map, diags);
        }
        TypeCheckItem::StreamUnpackWidthMismatch { .. } => {
            stream::lower_stream_unpack_width_mismatch(item, source_map, diags);
        }
        TypeCheckItem::AssignToReadonly { .. } => {
            misc::lower_assign_to_readonly(item, source_map, diags);
        }
        TypeCheckItem::ConstMissingInit { .. } => {
            misc::lower_const_missing_init(item, source_map, diags);
        }
        TypeCheckItem::VoidObjectType { .. } | TypeCheckItem::VoidUsedAsValue { .. } => {
            misc::lower_void_item(item, source_map, diags);
        }
        TypeCheckItem::ArrayQueryDynTypeForm { .. }
        | TypeCheckItem::ArrayQueryVarSizedDimByNumber { .. } => {
            array::lower_array_query_item(item, source_map, diags);
        }
        TypeCheckItem::IllegalDriveStrengthBothHighz { strength_site } => {
            misc::lower_illegal_drive_strength(*strength_site, source_map, diags);
        }
        TypeCheckItem::QueueBoundNotConst { .. } | TypeCheckItem::QueueBoundNotPositive { .. } => {
            misc::lower_queue_bound_item(item, source_map, diags);
        }
        TypeCheckItem::RecordAssignWrongRecord { .. } => {
            record::lower_record_assign_item(db, unit, item, source_map, diags);
        }
        TypeCheckItem::StreamSliceSizeNotConst { .. } => {
            stream::lower_stream_slice_size_not_const(item, source_map, diags);
        }
        TypeCheckItem::UnpackedRecordIntegralAssign { .. } => {
            record::lower_unpacked_record_integral_assign(db, unit, item, source_map, diags);
        }
        TypeCheckItem::IndexKeyNotIntegral { index_site } => {
            misc::lower_index_key_not_integral(*index_site, source_map, diags);
        }
        TypeCheckItem::AssocIndexKeyMismatch {
            index_site,
            expected,
            actual,
        } => {
            misc::lower_assoc_key_mismatch(*index_site, expected, actual, source_map, diags);
        }
        TypeCheckItem::TaggedExprError { .. } => {
            call::lower_tagged_expr_error(item, source_map, diags);
        }
        TypeCheckItem::FixedPartSelectNonConstant { expr_site } => {
            misc::lower_expr_site_error(
                *expr_site,
                lyra_diag::code::FIXED_PART_SELECT_NON_CONSTANT,
                lyra_diag::MessageId::FixedPartSelectNonConstant,
                source_map,
                diags,
            );
        }
        TypeCheckItem::DollarOutsideQueueContext { expr_site }
        | TypeCheckItem::QueuePartSelectNotAllowed { expr_site }
        | TypeCheckItem::EmptyConcatRequiresContext { expr_site }
        | TypeCheckItem::QueueConcatIncompatible { expr_site } => {
            let (code, msg_id) = match item {
                TypeCheckItem::DollarOutsideQueueContext { .. } => (
                    lyra_diag::code::DOLLAR_OUTSIDE_QUEUE_CONTEXT,
                    lyra_diag::MessageId::DollarOutsideQueueContext,
                ),
                TypeCheckItem::QueuePartSelectNotAllowed { .. } => (
                    lyra_diag::code::QUEUE_PART_SELECT_NOT_ALLOWED,
                    lyra_diag::MessageId::QueuePartSelectNotAllowed,
                ),
                TypeCheckItem::QueueConcatIncompatible { .. } => (
                    lyra_diag::code::QUEUE_CONCAT_INCOMPAT,
                    lyra_diag::MessageId::QueueConcatIncompatible,
                ),
                _ => (
                    lyra_diag::code::EMPTY_CONCAT_REQUIRES_CONTEXT,
                    lyra_diag::MessageId::EmptyConcatRequiresContext,
                ),
            };
            misc::lower_expr_site_error(*expr_site, code, msg_id, source_map, diags);
        }
        _ => {}
    }
}

pub(crate) fn lower_enum_value_diag(
    vd: &lyra_semantic::enum_def::EnumValueDiag,
    file_id: lyra_source::FileId,
    ast_id_map: &lyra_ast::AstIdMap,
    source_map: &lyra_preprocess::SourceMap,
) -> Option<lyra_diag::Diagnostic> {
    enum_diag::lower_enum_value_diag(vd, file_id, ast_id_map, source_map)
}
