use lyra_source::TextRange;
use smol_str::SmolStr;

/// Structured semantic diagnostic.
///
/// `range` is in expanded-text space within the owning file.
/// The DB layer converts to `Span` via `source_map.map_span()`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemanticDiag {
    pub kind: SemanticDiagKind,
    pub range: TextRange,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SemanticDiagKind {
    UnresolvedName { name: SmolStr },
    DuplicateDefinition { name: SmolStr, original: TextRange },
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
        }
    }
}
