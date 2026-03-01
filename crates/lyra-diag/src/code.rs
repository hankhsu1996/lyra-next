use core::fmt;

/// Identity code for a diagnostic, composed of a namespace and a number.
///
/// Namespace strings use dotted hierarchy (e.g. `"lyra.parse"`).
/// Numbers are unique within a namespace.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DiagnosticCode {
    pub namespace: &'static str,
    pub number: u32,
}

impl DiagnosticCode {
    pub const PARSE_ERROR: Self = Self {
        namespace: "lyra.parse",
        number: 1,
    };
    pub const PREPROCESS_ERROR: Self = Self {
        namespace: "lyra.preprocess",
        number: 1,
    };
    pub const UNRESOLVED_NAME: Self = Self {
        namespace: "lyra.semantic",
        number: 1,
    };
    pub const DUPLICATE_DEFINITION: Self = Self {
        namespace: "lyra.semantic",
        number: 2,
    };
    pub const PACKAGE_NOT_FOUND: Self = Self {
        namespace: "lyra.semantic",
        number: 3,
    };
    pub const MEMBER_NOT_FOUND: Self = Self {
        namespace: "lyra.semantic",
        number: 4,
    };
    pub const AMBIGUOUS_IMPORT: Self = Self {
        namespace: "lyra.semantic",
        number: 5,
    };
    pub const UNSUPPORTED_QUALIFIED_PATH: Self = Self {
        namespace: "lyra.semantic",
        number: 6,
    };
    pub const IMPORT_CONFLICT: Self = Self {
        namespace: "lyra.semantic",
        number: 7,
    };
    pub const UNSUPPORTED_TAGGED_UNION: Self = Self {
        namespace: "lyra.semantic",
        number: 8,
    };
    pub const ENUM_RANGE_INVALID: Self = Self {
        namespace: "lyra.semantic",
        number: 9,
    };
    pub const WIDTH_MISMATCH: Self = Self {
        namespace: "lyra.type",
        number: 1,
    };
    pub const BITS_NON_DATA_TYPE: Self = Self {
        namespace: "lyra.type",
        number: 6,
    };
    pub const ENUM_ASSIGN_INCOMPAT: Self = Self {
        namespace: "lyra.type",
        number: 7,
    };
    pub const UNDECLARED_TYPE: Self = Self {
        namespace: "lyra.type",
        number: 2,
    };
    pub const NOT_A_TYPE: Self = Self {
        namespace: "lyra.type",
        number: 3,
    };
    pub const ILLEGAL_ENUM_BASE: Self = Self {
        namespace: "lyra.type",
        number: 4,
    };
    pub const ENUM_BASE_DIMS_NOT_CONST: Self = Self {
        namespace: "lyra.type",
        number: 5,
    };
    pub const CONVERSION_ARG_TYPE: Self = Self {
        namespace: "lyra.type",
        number: 8,
    };
    pub const PACKED_UNION_WIDTH: Self = Self {
        namespace: "lyra.type",
        number: 9,
    };
    pub const MODPORT_DIRECTION: Self = Self {
        namespace: "lyra.type",
        number: 10,
    };
    pub const MODPORT_REF_UNSUPPORTED: Self = Self {
        namespace: "lyra.type",
        number: 11,
    };
    pub const ENUM_CAST_OUT_OF_RANGE: Self = Self {
        namespace: "lyra.type",
        number: 12,
    };
    pub const STREAM_WITH_NON_ARRAY: Self = Self {
        namespace: "lyra.type",
        number: 13,
    };
    pub const MODPORT_EMPTY_PORT: Self = Self {
        namespace: "lyra.type",
        number: 14,
    };
    pub const MODPORT_EXPR_NOT_ASSIGNABLE: Self = Self {
        namespace: "lyra.type",
        number: 15,
    };
    pub const METHOD_CALL_ERROR: Self = Self {
        namespace: "lyra.type",
        number: 16,
    };
    pub const UNSUPPORTED_LHS_FORM: Self = Self {
        namespace: "lyra.type",
        number: 17,
    };
    pub const INVALID_ASSIGNMENT_LHS: Self = Self {
        namespace: "lyra.type",
        number: 18,
    };
    pub const NEW_EXPR_NOT_DYN_ARRAY: Self = Self {
        namespace: "lyra.type",
        number: 19,
    };
    pub const NEW_EXPR_TOO_MANY_INIT_ARGS: Self = Self {
        namespace: "lyra.type",
        number: 20,
    };
    pub const NEW_EXPR_SIZE_NOT_LONGINT: Self = Self {
        namespace: "lyra.type",
        number: 21,
    };
    pub const NEW_EXPR_SIZE_NEGATIVE: Self = Self {
        namespace: "lyra.type",
        number: 22,
    };
    pub const NEW_EXPR_INIT_INCOMPAT: Self = Self {
        namespace: "lyra.type",
        number: 23,
    };
    pub const ARRAY_INCOMPAT: Self = Self {
        namespace: "lyra.type",
        number: 24,
    };
    pub const ENUM_DUPLICATE_VALUE: Self = Self {
        namespace: "lyra.semantic",
        number: 10,
    };
    pub const ENUM_VALUE_OVERFLOW: Self = Self {
        namespace: "lyra.semantic",
        number: 11,
    };
    pub const ENUM_SIZED_LITERAL_WIDTH: Self = Self {
        namespace: "lyra.semantic",
        number: 12,
    };
    pub const UNRESOLVED_MODULE_INST: Self = Self {
        namespace: "lyra.elab",
        number: 1,
    };
    pub const NOT_INSTANTIABLE: Self = Self {
        namespace: "lyra.elab",
        number: 2,
    };
    pub const UNKNOWN_PORT: Self = Self {
        namespace: "lyra.elab",
        number: 3,
    };
    pub const DUPLICATE_PORT_CONN: Self = Self {
        namespace: "lyra.elab",
        number: 4,
    };
    pub const TOO_MANY_POSITIONAL_PORTS: Self = Self {
        namespace: "lyra.elab",
        number: 5,
    };
    pub const MISSING_PORT_CONN: Self = Self {
        namespace: "lyra.elab",
        number: 6,
    };
    pub const PORT_WIDTH_MISMATCH: Self = Self {
        namespace: "lyra.elab",
        number: 7,
    };
    pub const ELAB_RECURSION_LIMIT: Self = Self {
        namespace: "lyra.elab",
        number: 8,
    };
    pub const UNKNOWN_PARAM: Self = Self {
        namespace: "lyra.elab",
        number: 9,
    };
    pub const DUPLICATE_PARAM_OVERRIDE: Self = Self {
        namespace: "lyra.elab",
        number: 10,
    };
    pub const TOO_MANY_POSITIONAL_PARAMS: Self = Self {
        namespace: "lyra.elab",
        number: 11,
    };
    pub const PARAM_NOT_CONST: Self = Self {
        namespace: "lyra.elab",
        number: 12,
    };
    pub const GEN_COND_NOT_CONST: Self = Self {
        namespace: "lyra.elab",
        number: 13,
    };
    pub const GENVAR_NOT_CONST: Self = Self {
        namespace: "lyra.elab",
        number: 14,
    };
    pub const DUPLICATE_GEN_BLOCK_NAME: Self = Self {
        namespace: "lyra.elab",
        number: 15,
    };
    pub const GENERATE_ITERATION_LIMIT: Self = Self {
        namespace: "lyra.elab",
        number: 16,
    };
    pub const MODPORT_CONFLICT: Self = Self {
        namespace: "lyra.elab",
        number: 17,
    };

    pub const INTERNAL_ERROR: Self = Self {
        namespace: "lyra.internal",
        number: 1,
    };

    /// Format as `"namespace[number]"`, e.g. `"lyra.semantic[1]"`.
    pub fn as_str(&self) -> String {
        format!("{}[{}]", self.namespace, self.number)
    }
}

impl fmt::Display for DiagnosticCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}[{}]", self.namespace, self.number)
    }
}
