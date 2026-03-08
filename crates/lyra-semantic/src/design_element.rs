use lyra_source::TokenSpan;

use crate::global_index::DefinitionKind;

/// Full extent of a design element (module, package, interface, etc.).
///
/// The `TokenSpan` covers the entire design-element body, trimmed of leading
/// trivia. Computed in the builder (where CST access is allowed) so that
/// consumers can check containment without CST traversal.
///
/// Used for directive-placement legality checks (e.g., LRM 22.3 `resetall`
/// inside a design element).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DesignElement {
    pub kind: DefinitionKind,
    pub extent: TokenSpan,
}
