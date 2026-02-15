pub use text_size::{TextRange, TextSize};

/// Opaque handle to a source file in the database.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileId(pub u32);

/// A span within a single file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub file: FileId,
    pub range: TextRange,
}
