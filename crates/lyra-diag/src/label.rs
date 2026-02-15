use lyra_source::{FileId, Span, TextRange};
use smol_str::SmolStr;

use crate::message::Message;

/// Whether a label marks the primary location or a related location.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LabelKind {
    Primary,
    Secondary,
}

/// A labeled source location within a diagnostic.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Label {
    pub kind: LabelKind,
    pub span: Span,
    pub message: Message,
}

/// A text replacement targeting original user-editable source.
///
/// Coordinates are in ORIGINAL file space, not expanded.
/// If mapping from expanded -> original fails, do not emit the edit.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TextEdit {
    pub file: FileId,
    pub range: TextRange,
    pub replacement: SmolStr,
}
