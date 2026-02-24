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
    /// Sentinel for missing name tokens (parse error recovery).
    ///
    /// Only the semantic builder (and tests) may produce this value.
    /// When the builder creates INVALID, it also emits an
    /// `InternalError` diagnostic anchored at the relevant syntax
    /// node, so the real diagnostic span is still meaningful.
    pub const INVALID: Self = Self {
        start: u32::MAX,
        len: 0,
    };

    pub fn new(range: TextRange) -> Self {
        Self {
            start: u32::from(range.start()),
            len: u32::from(range.end()) - u32::from(range.start()),
        }
    }

    pub fn is_valid(self) -> bool {
        self.start != u32::MAX
    }

    pub fn text_range(self) -> TextRange {
        if !self.is_valid() {
            return Self::unmappable_range();
        }
        let start = TextSize::from(self.start);
        TextRange::at(start, TextSize::from(self.len))
    }

    /// Return the token range, falling back to `fallback` when INVALID.
    ///
    /// Use this when a meaningful location is needed for diagnostics
    /// and the caller has an AST-node anchor to fall back on.
    pub fn text_range_or(self, fallback: TextRange) -> TextRange {
        if self.is_valid() {
            self.text_range()
        } else {
            fallback
        }
    }

    /// Sentinel range that `map_span` will never resolve.
    ///
    /// Starts at `u32::MAX`, which is past any real file content,
    /// so `SourceMap::map_span` returns `None`.
    #[inline]
    fn unmappable_range() -> TextRange {
        TextRange::at(TextSize::from(u32::MAX), TextSize::from(0))
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
