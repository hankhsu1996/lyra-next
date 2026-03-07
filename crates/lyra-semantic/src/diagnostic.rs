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
    VoidMemberNonTagged {
        name: SmolStr,
    },
    IllegalUnionMemberType {
        name: SmolStr,
        category: SmolStr,
    },
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
    NonIntegralPackedMember {
        name: SmolStr,
        record_kind: SmolStr,
        packing: SmolStr,
        category: SmolStr,
    },
    NotASubroutine {
        name: SmolStr,
    },
    PrototypeMismatch {
        name: SmolStr,
        mismatch: PrototypeMismatchDetail,
    },
    AmbiguousCuScope {
        name: SmolStr,
        sites: Box<[Site]>,
    },
    InternalError {
        detail: SmolStr,
    },
}

/// Structured detail for prototype signature mismatches.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PrototypeMismatchDetail {
    ReturnType,
    PortCount {
        proto: usize,
        actual: usize,
    },
    PortDirection {
        index: usize,
        proto_dir: lyra_ast::PortDirection,
        actual_dir: lyra_ast::PortDirection,
    },
    PortType {
        index: usize,
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
            SemanticDiagKind::VoidMemberNonTagged { name } => {
                format!("void member `{name}` is only allowed in tagged unions")
            }
            SemanticDiagKind::IllegalUnionMemberType { name, category } => {
                format!("{category} member `{name}` is not allowed in untagged unions")
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
            SemanticDiagKind::NonIntegralPackedMember { name, category, .. } => {
                format!("non-integral {category} member `{name}` in packed record")
            }
            SemanticDiagKind::NotASubroutine { name } => {
                format!("`{name}` is not a task or function")
            }
            SemanticDiagKind::PrototypeMismatch { name, mismatch } => {
                let detail = match mismatch {
                    PrototypeMismatchDetail::ReturnType => "return type mismatch".to_string(),
                    PrototypeMismatchDetail::PortCount { proto, actual } => {
                        format!("prototype has {proto} ports but declaration has {actual}")
                    }
                    PrototypeMismatchDetail::PortDirection {
                        index,
                        proto_dir,
                        actual_dir,
                    } => format!(
                        "port {} direction mismatch: prototype has `{}` but declaration has `{}`",
                        index + 1,
                        proto_dir.label(),
                        actual_dir.label(),
                    ),
                    PrototypeMismatchDetail::PortType { index } => {
                        format!("port {} type mismatch", index + 1)
                    }
                };
                format!("prototype signature does not match declaration of `{name}`: {detail}")
            }
            SemanticDiagKind::AmbiguousCuScope { name, sites } => {
                format!(
                    "name `{name}` is ambiguous: {} declarations in compilation-unit scope",
                    sites.len()
                )
            }
            SemanticDiagKind::InternalError { detail } => {
                format!("internal error: {detail}")
            }
        }
    }
}
