use lyra_source::{NameSpan, TokenSpan};
use smol_str::SmolStr;

use crate::Site;

/// Anchor for a diagnostic span in expanded-text space.
///
/// Pure data with no methods. The lowering layer (`lyra-db`) is
/// responsible for converting anchors to source ranges.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagSpan {
    /// CST node anchor.
    Site(Site),
    /// Identifier token span. May be INVALID (parse recovery).
    Name(NameSpan),
    /// Non-identifier token span (keyword, operator, punctuation).
    Token(TokenSpan),
}

/// Structured semantic diagnostic.
///
/// `primary` is a construct-level anchor (usually `Site`).
/// `label` is a precise highlight span (usually `Name` or `Token`).
/// The DB layer converts to `Span` via `source_map.map_span()`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemanticDiag {
    pub kind: SemanticDiagKind,
    pub primary: DiagSpan,
    pub label: Option<DiagSpan>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SemanticDiagKind {
    UnresolvedName {
        name: SmolStr,
    },
    DuplicateDefinition {
        name: SmolStr,
        original_primary: DiagSpan,
        original_label: Option<DiagSpan>,
    },
    PackageNotFound {
        package: SmolStr,
    },
    MemberNotFound {
        package: SmolStr,
        member: SmolStr,
    },
    AmbiguousWildcardImport {
        name: SmolStr,
        candidates: Box<[SmolStr]>,
    },
    UnsupportedQualifiedPath {
        path: SmolStr,
    },
    UndeclaredType {
        name: SmolStr,
    },
    NotAType {
        name: SmolStr,
    },
    UnsupportedTaggedUnion,
    IllegalEnumBaseType {
        name: SmolStr,
    },
    EnumBaseDimsNotConstant,
    EnumRangeBoundNotEvaluable,
    EnumRangeCountNegative {
        count: i64,
    },
    EnumRangeTooLarge {
        count: u64,
    },
    InternalError {
        detail: SmolStr,
    },
}

impl SemanticDiag {
    /// Format this diagnostic into a human-readable message string.
    pub fn format(&self) -> String {
        match &self.kind {
            SemanticDiagKind::UnresolvedName { name } => {
                format!("unresolved name `{name}`")
            }
            SemanticDiagKind::DuplicateDefinition { name, .. } => {
                format!("duplicate definition of `{name}`")
            }
            SemanticDiagKind::PackageNotFound { package } => {
                format!("package `{package}` not found")
            }
            SemanticDiagKind::MemberNotFound { package, member } => {
                format!("member `{member}` not found in package `{package}`")
            }
            SemanticDiagKind::AmbiguousWildcardImport { name, candidates } => {
                let pkgs = candidates.join("`, `");
                format!("name `{name}` is ambiguous: imported from packages `{pkgs}`")
            }
            SemanticDiagKind::UnsupportedQualifiedPath { path } => {
                format!("qualified path `{path}` is not supported")
            }
            SemanticDiagKind::UndeclaredType { name } => {
                format!("undeclared type `{name}`")
            }
            SemanticDiagKind::NotAType { name } => {
                format!("`{name}` is not a type")
            }
            SemanticDiagKind::UnsupportedTaggedUnion => {
                "tagged unions are not yet supported".to_string()
            }
            SemanticDiagKind::IllegalEnumBaseType { name } => {
                format!("enum base type `{name}` is not an integral type")
            }
            SemanticDiagKind::EnumBaseDimsNotConstant => {
                "enum base type has non-constant packed dimensions".to_string()
            }
            SemanticDiagKind::EnumRangeBoundNotEvaluable => {
                "enum member range bound is not a constant expression".to_string()
            }
            SemanticDiagKind::EnumRangeCountNegative { count } => {
                format!("enum member range count is negative ({count})")
            }
            SemanticDiagKind::EnumRangeTooLarge { count } => {
                format!("enum member range count is too large ({count})")
            }
            SemanticDiagKind::InternalError { detail } => {
                format!("internal error: {detail}")
            }
        }
    }
}
