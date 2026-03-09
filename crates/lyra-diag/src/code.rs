/// A diagnostic code is a fully-qualified string key that uniquely identifies
/// a diagnostic kind. Keys use dotted hierarchy (e.g. `"lyra.type.width_mismatch"`).
pub type DiagKey = &'static str;

// Parse diagnostics
pub const PARSE_ERROR: DiagKey = "lyra.parse.error";
pub const PREPROCESS_ERROR: DiagKey = "lyra.preprocess.error";

// Semantic diagnostics
pub const UNRESOLVED_NAME: DiagKey = "lyra.semantic.unresolved_name";
pub const DUPLICATE_DEFINITION: DiagKey = "lyra.semantic.duplicate_definition";
pub const PACKAGE_NOT_FOUND: DiagKey = "lyra.semantic.package_not_found";
pub const MEMBER_NOT_FOUND: DiagKey = "lyra.semantic.member_not_found";
pub const AMBIGUOUS_IMPORT: DiagKey = "lyra.semantic.ambiguous_import";
pub const AMBIGUOUS_CU_SCOPE: DiagKey = "lyra.semantic.ambiguous_cu_scope";
pub const UNSUPPORTED_QUALIFIED_PATH: DiagKey = "lyra.semantic.unsupported_qualified_path";
pub const IMPORT_CONFLICT: DiagKey = "lyra.semantic.import_conflict";
pub const ENUM_RANGE_INVALID: DiagKey = "lyra.semantic.enum_range_invalid";
pub const ENUM_DUPLICATE_VALUE: DiagKey = "lyra.semantic.enum_duplicate_value";
pub const ENUM_VALUE_OVERFLOW: DiagKey = "lyra.semantic.enum_value_overflow";
pub const ENUM_SIZED_LITERAL_WIDTH: DiagKey = "lyra.semantic.enum_sized_literal_width";
pub const VOID_MEMBER_NON_TAGGED: DiagKey = "lyra.semantic.void_member_non_tagged";
pub const ILLEGAL_UNION_MEMBER_TYPE: DiagKey = "lyra.semantic.illegal_union_member_type";
pub const TYPE_PARAM_NO_DEFAULT: DiagKey = "lyra.semantic.type_param_no_default";
pub const NON_INTEGRAL_PACKED_MEMBER: DiagKey = "lyra.semantic.non_integral_packed_member";
pub const NOT_A_SUBROUTINE: DiagKey = "lyra.semantic.not_a_subroutine";
pub const BREAK_OUTSIDE_LOOP: DiagKey = "lyra.semantic.break_outside_loop";
pub const CONTINUE_OUTSIDE_LOOP: DiagKey = "lyra.semantic.continue_outside_loop";
pub const RETURN_OUTSIDE_CALLABLE: DiagKey = "lyra.semantic.return_outside_callable";
pub const RETURN_VALUE_IN_VOID: DiagKey = "lyra.semantic.return_value_in_void";
pub const RETURN_MISSING_VALUE: DiagKey = "lyra.semantic.return_missing_value";
pub const ASSIGN_TO_FOREACH_VAR: DiagKey = "lyra.semantic.assign_to_foreach_var";
pub const FOREACH_VAR_SAME_AS_ARRAY: DiagKey = "lyra.semantic.foreach_var_same_as_array";
pub const FOREACH_TOO_MANY_VARS: DiagKey = "lyra.semantic.foreach_too_many_vars";
pub const CASE_INSIDE_REQUIRES_PLAIN_CASE: DiagKey =
    "lyra.semantic.case_inside_requires_plain_case";
pub const PROTOTYPE_MISMATCH: DiagKey = "lyra.semantic.prototype_mismatch";
pub const FOREACH_WILDCARD_ASSOC: DiagKey = "lyra.semantic.foreach_wildcard_assoc";

// Type diagnostics
pub const WIDTH_MISMATCH: DiagKey = "lyra.type.width_mismatch";
pub const UNDECLARED_TYPE: DiagKey = "lyra.type.undeclared_type";
pub const NOT_A_TYPE: DiagKey = "lyra.type.not_a_type";
pub const ILLEGAL_ENUM_BASE: DiagKey = "lyra.type.illegal_enum_base";
pub const ENUM_BASE_DIMS_NOT_CONST: DiagKey = "lyra.type.enum_base_dims_not_const";
pub const BITS_NON_DATA_TYPE: DiagKey = "lyra.type.bits_non_data_type";
pub const ENUM_ASSIGN_INCOMPAT: DiagKey = "lyra.type.enum_assign_incompat";
pub const CONVERSION_ARG_TYPE: DiagKey = "lyra.type.conversion_arg_type";
pub const PACKED_UNION_WIDTH: DiagKey = "lyra.type.packed_union_width";
pub const MODPORT_DIRECTION: DiagKey = "lyra.type.modport_direction";
pub const MODPORT_REF_UNSUPPORTED: DiagKey = "lyra.type.modport_ref_unsupported";
pub const ENUM_CAST_OUT_OF_RANGE: DiagKey = "lyra.type.enum_cast_out_of_range";
pub const STREAM_WITH_NON_ARRAY: DiagKey = "lyra.type.stream_with_non_array";
pub const MODPORT_EMPTY_PORT: DiagKey = "lyra.type.modport_empty_port";
pub const MODPORT_EXPR_NOT_ASSIGNABLE: DiagKey = "lyra.type.modport_expr_not_assignable";
pub const METHOD_CALL_ERROR: DiagKey = "lyra.type.method_call_error";
pub const UNSUPPORTED_LHS_FORM: DiagKey = "lyra.type.unsupported_lhs_form";
pub const INVALID_ASSIGNMENT_LHS: DiagKey = "lyra.type.invalid_assignment_lhs";
pub const NEW_EXPR_NOT_DYN_ARRAY: DiagKey = "lyra.type.new_expr_not_dyn_array";
pub const NEW_EXPR_TOO_MANY_INIT_ARGS: DiagKey = "lyra.type.new_expr_too_many_init_args";
pub const NEW_EXPR_SIZE_NOT_LONGINT: DiagKey = "lyra.type.new_expr_size_not_longint";
pub const NEW_EXPR_SIZE_NEGATIVE: DiagKey = "lyra.type.new_expr_size_negative";
pub const NEW_EXPR_INIT_INCOMPAT: DiagKey = "lyra.type.new_expr_init_incompat";
pub const ARRAY_INCOMPAT: DiagKey = "lyra.type.array_incompat";
pub const STREAM_UNPACK_OPERAND_INVALID: DiagKey = "lyra.type.stream_unpack_operand_invalid";
pub const STREAM_UNPACK_OPERAND_UNSUPPORTED: DiagKey =
    "lyra.type.stream_unpack_operand_unsupported";
pub const STREAM_UNPACK_GREEDY_REMAINDER: DiagKey = "lyra.type.stream_unpack_greedy_remainder";
pub const STREAM_UNPACK_WIDTH_MISMATCH: DiagKey = "lyra.type.stream_unpack_width_mismatch";
pub const ASSIGN_TO_CONST: DiagKey = "lyra.type.assign_to_const";
pub const CONST_MISSING_INIT: DiagKey = "lyra.type.const_missing_init";
pub const VOID_OBJECT_TYPE: DiagKey = "lyra.type.void_object_type";
pub const VOID_USED_AS_VALUE: DiagKey = "lyra.type.void_used_as_value";
pub const ARRAY_QUERY_DYN_TYPE_FORM: DiagKey = "lyra.type.array_query_dyn_type_form";
pub const ARRAY_QUERY_VAR_SIZED_DIM: DiagKey = "lyra.type.array_query_var_sized_dim";
pub const ILLEGAL_DRIVE_STRENGTH_BOTH_HIGHZ: DiagKey =
    "lyra.type.illegal_drive_strength_both_highz";
pub const QUEUE_BOUND_NOT_CONST: DiagKey = "lyra.type.queue_bound_not_const";
pub const QUEUE_BOUND_NOT_POSITIVE: DiagKey = "lyra.type.queue_bound_not_positive";
pub const MEMBER_NOT_IN_MODPORT: DiagKey = "lyra.type.member_not_in_modport";
pub const RECORD_ASSIGN_INCOMPAT: DiagKey = "lyra.type.record_assign_incompat";
pub const STREAM_SLICE_SIZE_NOT_CONST: DiagKey = "lyra.type.stream_slice_size_not_const";
pub const UNPACKED_RECORD_INTEGRAL_ASSIGN: DiagKey = "lyra.type.unpacked_record_integral_assign";
pub const INDEX_KEY_NOT_INTEGRAL: DiagKey = "lyra.type.index_key_not_integral";
pub const ASSOC_INDEX_KEY_MISMATCH: DiagKey = "lyra.type.assoc_index_key_mismatch";
pub const TAGGED_EXPR_ERROR: DiagKey = "lyra.type.tagged_expr_error";
pub const DOLLAR_OUTSIDE_QUEUE_CONTEXT: DiagKey = "lyra.type.dollar_outside_queue_context";
pub const QUEUE_PART_SELECT_NOT_ALLOWED: DiagKey = "lyra.type.queue_part_select_not_allowed";
pub const EMPTY_CONCAT_REQUIRES_CONTEXT: DiagKey = "lyra.type.empty_concat_requires_context";
pub const QUEUE_CONCAT_INCOMPAT: DiagKey = "lyra.type.queue_concat_incompat";
pub const FIXED_PART_SELECT_NON_CONSTANT: DiagKey = "lyra.type.fixed_part_select_non_constant";
pub const IMPLICIT_NET_FORBIDDEN: DiagKey = "lyra.semantic.implicit_net_forbidden";
pub const AUTOMATIC_VAR_NON_PROCEDURAL: DiagKey = "lyra.decl.automatic_var_non_procedural";

// Elaboration diagnostics
pub const UNRESOLVED_MODULE_INST: DiagKey = "lyra.elab.unresolved_module_inst";
pub const NOT_INSTANTIABLE: DiagKey = "lyra.elab.not_instantiable";
pub const UNKNOWN_PORT: DiagKey = "lyra.elab.unknown_port";
pub const DUPLICATE_PORT_CONN: DiagKey = "lyra.elab.duplicate_port_conn";
pub const TOO_MANY_POSITIONAL_PORTS: DiagKey = "lyra.elab.too_many_positional_ports";
pub const MISSING_PORT_CONN: DiagKey = "lyra.elab.missing_port_conn";
pub const ELAB_RECURSION_LIMIT: DiagKey = "lyra.elab.elab_recursion_limit";
pub const UNKNOWN_PARAM: DiagKey = "lyra.elab.unknown_param";
pub const DUPLICATE_PARAM_OVERRIDE: DiagKey = "lyra.elab.duplicate_param_override";
pub const TOO_MANY_POSITIONAL_PARAMS: DiagKey = "lyra.elab.too_many_positional_params";
pub const PARAM_NOT_CONST: DiagKey = "lyra.elab.param_not_const";
pub const GEN_COND_NOT_CONST: DiagKey = "lyra.elab.gen_cond_not_const";
pub const GENVAR_NOT_CONST: DiagKey = "lyra.elab.genvar_not_const";
pub const GENERATE_ITERATION_LIMIT: DiagKey = "lyra.elab.generate_iteration_limit";
pub const MODPORT_CONFLICT: DiagKey = "lyra.elab.modport_conflict";

// Preprocess diagnostics (semantic-level)
pub const TIMESCALE_INVALID_VALUE: DiagKey = "lyra.preprocess.timescale_invalid_value";
pub const TIMESCALE_PRECISION_EXCEEDS_UNIT: DiagKey =
    "lyra.preprocess.timescale_precision_exceeds_unit";
pub const RESETALL_INSIDE_DESIGN_ELEMENT: DiagKey =
    "lyra.preprocess.resetall_inside_design_element";

// Internal diagnostics
pub const INTERNAL_ERROR: DiagKey = "lyra.internal.error";

/// Authoritative list of all diagnostic keys.
pub const ALL_KEYS: &[DiagKey] = &[
    PARSE_ERROR,
    PREPROCESS_ERROR,
    UNRESOLVED_NAME,
    DUPLICATE_DEFINITION,
    PACKAGE_NOT_FOUND,
    MEMBER_NOT_FOUND,
    AMBIGUOUS_IMPORT,
    AMBIGUOUS_CU_SCOPE,
    UNSUPPORTED_QUALIFIED_PATH,
    IMPORT_CONFLICT,
    ENUM_RANGE_INVALID,
    ENUM_DUPLICATE_VALUE,
    ENUM_VALUE_OVERFLOW,
    ENUM_SIZED_LITERAL_WIDTH,
    VOID_MEMBER_NON_TAGGED,
    ILLEGAL_UNION_MEMBER_TYPE,
    TYPE_PARAM_NO_DEFAULT,
    NON_INTEGRAL_PACKED_MEMBER,
    NOT_A_SUBROUTINE,
    BREAK_OUTSIDE_LOOP,
    CONTINUE_OUTSIDE_LOOP,
    RETURN_OUTSIDE_CALLABLE,
    RETURN_VALUE_IN_VOID,
    RETURN_MISSING_VALUE,
    ASSIGN_TO_FOREACH_VAR,
    FOREACH_VAR_SAME_AS_ARRAY,
    FOREACH_TOO_MANY_VARS,
    CASE_INSIDE_REQUIRES_PLAIN_CASE,
    PROTOTYPE_MISMATCH,
    FOREACH_WILDCARD_ASSOC,
    IMPLICIT_NET_FORBIDDEN,
    WIDTH_MISMATCH,
    UNDECLARED_TYPE,
    NOT_A_TYPE,
    ILLEGAL_ENUM_BASE,
    ENUM_BASE_DIMS_NOT_CONST,
    BITS_NON_DATA_TYPE,
    ENUM_ASSIGN_INCOMPAT,
    CONVERSION_ARG_TYPE,
    PACKED_UNION_WIDTH,
    MODPORT_DIRECTION,
    MODPORT_REF_UNSUPPORTED,
    ENUM_CAST_OUT_OF_RANGE,
    STREAM_WITH_NON_ARRAY,
    MODPORT_EMPTY_PORT,
    MODPORT_EXPR_NOT_ASSIGNABLE,
    METHOD_CALL_ERROR,
    UNSUPPORTED_LHS_FORM,
    INVALID_ASSIGNMENT_LHS,
    NEW_EXPR_NOT_DYN_ARRAY,
    NEW_EXPR_TOO_MANY_INIT_ARGS,
    NEW_EXPR_SIZE_NOT_LONGINT,
    NEW_EXPR_SIZE_NEGATIVE,
    NEW_EXPR_INIT_INCOMPAT,
    ARRAY_INCOMPAT,
    STREAM_UNPACK_OPERAND_INVALID,
    STREAM_UNPACK_OPERAND_UNSUPPORTED,
    STREAM_UNPACK_GREEDY_REMAINDER,
    STREAM_UNPACK_WIDTH_MISMATCH,
    ASSIGN_TO_CONST,
    CONST_MISSING_INIT,
    VOID_OBJECT_TYPE,
    VOID_USED_AS_VALUE,
    ARRAY_QUERY_DYN_TYPE_FORM,
    ARRAY_QUERY_VAR_SIZED_DIM,
    ILLEGAL_DRIVE_STRENGTH_BOTH_HIGHZ,
    QUEUE_BOUND_NOT_CONST,
    QUEUE_BOUND_NOT_POSITIVE,
    MEMBER_NOT_IN_MODPORT,
    RECORD_ASSIGN_INCOMPAT,
    STREAM_SLICE_SIZE_NOT_CONST,
    UNPACKED_RECORD_INTEGRAL_ASSIGN,
    INDEX_KEY_NOT_INTEGRAL,
    ASSOC_INDEX_KEY_MISMATCH,
    TAGGED_EXPR_ERROR,
    DOLLAR_OUTSIDE_QUEUE_CONTEXT,
    QUEUE_PART_SELECT_NOT_ALLOWED,
    EMPTY_CONCAT_REQUIRES_CONTEXT,
    QUEUE_CONCAT_INCOMPAT,
    FIXED_PART_SELECT_NON_CONSTANT,
    AUTOMATIC_VAR_NON_PROCEDURAL,
    UNRESOLVED_MODULE_INST,
    NOT_INSTANTIABLE,
    UNKNOWN_PORT,
    DUPLICATE_PORT_CONN,
    TOO_MANY_POSITIONAL_PORTS,
    MISSING_PORT_CONN,
    ELAB_RECURSION_LIMIT,
    UNKNOWN_PARAM,
    DUPLICATE_PARAM_OVERRIDE,
    TOO_MANY_POSITIONAL_PARAMS,
    PARAM_NOT_CONST,
    GEN_COND_NOT_CONST,
    GENVAR_NOT_CONST,
    GENERATE_ITERATION_LIMIT,
    MODPORT_CONFLICT,
    TIMESCALE_INVALID_VALUE,
    TIMESCALE_PRECISION_EXCEEDS_UNIT,
    RESETALL_INSIDE_DESIGN_ELEMENT,
    INTERNAL_ERROR,
];

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use super::*;

    #[test]
    fn all_keys_unique() {
        let mut seen = HashSet::new();
        for key in ALL_KEYS {
            assert!(seen.insert(key), "duplicate diagnostic key: {key}");
        }
    }

    #[test]
    fn all_keys_well_formed() {
        const ALLOWED_DOMAINS: &[&str] = &[
            "parse",
            "preprocess",
            "semantic",
            "type",
            "elab",
            "decl",
            "internal",
        ];

        for key in ALL_KEYS {
            assert!(
                key.starts_with("lyra."),
                "key must start with 'lyra.': {key}"
            );
            let parts: Vec<&str> = key.split('.').collect();
            assert!(
                parts.len() == 3,
                "key must have exactly 3 dotted segments: {key}"
            );
            assert!(
                parts.iter().all(|p| !p.is_empty()),
                "key must not have empty segments: {key}"
            );
            let domain = parts[1];
            assert!(
                ALLOWED_DOMAINS.contains(&domain),
                "unknown domain '{domain}' in key {key}; add to ALLOWED_DOMAINS if intentional"
            );
            let slug = parts[2];
            assert!(
                slug.chars()
                    .all(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || c == '_'),
                "slug must be lower_snake_case: {key}"
            );
        }
    }

    #[test]
    fn all_keys_complete() {
        let source = include_str!("code.rs");
        let const_count = source
            .lines()
            .filter(|line| {
                let trimmed = line.trim();
                trimmed.starts_with("pub const ")
                    && trimmed.contains(": DiagKey")
                    && !trimmed.contains("ALL_KEYS")
            })
            .count();
        assert_eq!(
            ALL_KEYS.len(),
            const_count,
            "ALL_KEYS has {} entries but source has {} pub const DiagKey declarations; \
             did you forget to add a new key to ALL_KEYS?",
            ALL_KEYS.len(),
            const_count
        );
    }
}
