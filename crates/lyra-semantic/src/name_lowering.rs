//! Qualified-name lowering from typed AST into semantic `QualifiedPath`.
//!
//! Consumes `QualifiedName::classified_segments()` from `lyra-ast`,
//! which already distinguishes `$unit` from ordinary identifiers.
//! No token-kind inspection happens here.

use lyra_ast::QualifiedSegment;
use smol_str::SmolStr;

use crate::def_index::QualifiedRoot;

/// A two-segment qualified path: `root::member`.
///
/// Shared structural type used by `NamePath`, `TypeRef`, and resolution APIs.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct QualifiedPath {
    pub root: QualifiedRoot,
    pub member: SmolStr,
}

impl QualifiedPath {
    /// Display string for diagnostics (e.g. `"pkg::member"` or `"$unit::member"`).
    pub fn display_name(&self) -> String {
        format!("{}::{}", self.root.display_name(), self.member)
    }
}

/// Why `lower_qualified_name` could not produce a `QualifiedPath`.
pub enum QualifiedNameLowerError {
    /// Fewer than two segments (malformed AST).
    TooFewSegments,
    /// More than two segments (not yet supported).
    TooManySegments { count: usize },
}

/// Lower a `QualifiedName` AST node into a `QualifiedPath`.
///
/// Uses the typed `classified_segments()` accessor so this module
/// never inspects token kinds directly.
pub fn lower_qualified_name(
    qn: &lyra_ast::QualifiedName,
) -> Result<QualifiedPath, QualifiedNameLowerError> {
    let mut segments = qn.classified_segments();
    let first = segments
        .next()
        .ok_or(QualifiedNameLowerError::TooFewSegments)?;
    let second = segments
        .next()
        .ok_or(QualifiedNameLowerError::TooFewSegments)?;
    if segments.next().is_some() {
        let count = 3 + segments.count();
        return Err(QualifiedNameLowerError::TooManySegments { count });
    }
    let root = match first {
        QualifiedSegment::UnitScope => QualifiedRoot::Unit,
        QualifiedSegment::Named(name) => QualifiedRoot::Package(name),
    };
    let member = match second {
        QualifiedSegment::Named(name) => name,
        QualifiedSegment::UnitScope => return Err(QualifiedNameLowerError::TooFewSegments),
    };
    Ok(QualifiedPath { root, member })
}
