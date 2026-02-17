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
    pub const WIDTH_MISMATCH: Self = Self {
        namespace: "lyra.type",
        number: 1,
    };
    pub const UNDECLARED_TYPE: Self = Self {
        namespace: "lyra.type",
        number: 2,
    };
    pub const NOT_A_TYPE: Self = Self {
        namespace: "lyra.type",
        number: 3,
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
