pub use text_size::{TextRange, TextSize};

mod expansion;
pub use expansion::{ExpansionFrame, ExpansionKind, FileLoc};

mod line_index;
pub use line_index::{LineCol, LineIndex};

/// Span of a name-introducing identifier token.
///
/// Captured at builder time from `SyntaxToken::text_range()`. Provides
/// O(1) `text_range()` at diagnostic time without CST traversal.
///
/// File context comes from the owning `DefIndex`, `SourceFile`, or
/// `ErasedAstId` -- not stored here.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct NameSpan {
    start: u32,
    len: u32,
}

impl NameSpan {
    pub fn new(range: TextRange) -> Self {
        Self {
            start: u32::from(range.start()),
            len: u32::from(range.end()) - u32::from(range.start()),
        }
    }

    pub fn text_range(self) -> TextRange {
        let start = TextSize::from(self.start);
        TextRange::at(start, TextSize::from(self.len))
    }
}

/// Opaque handle to a source file in the database.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FileId(pub u32);

/// A span within a single file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub file: FileId,
    pub range: TextRange,
}
