use crate::{FileId, Span};
use text_size::TextSize;

/// A precise location in a physical source file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileLoc {
    pub file: FileId,
    pub offset: TextSize,
}

/// The kind of expansion that produced a location.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ExpansionKind {
    Include,
}

/// One frame in the expansion stack, representing a single
/// expansion step (include or macro).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ExpansionFrame {
    pub kind: ExpansionKind,
    /// Where the directive/macro was invoked (in the including file).
    /// Uses Span (file + range) so "included from here" notes can
    /// highlight the entire directive, not just a point.
    pub call_site: Span,
    /// Where the text physically lives (in the included/defining file).
    pub spelling: FileLoc,
}
