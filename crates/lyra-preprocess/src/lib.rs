use lyra_lexer::Token;
use lyra_source::{FileId, Span, TextRange};

/// Bundled output of preprocessing a single file.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PreprocOutput {
    pub tokens: Vec<Token>,
    pub source_map: SourceMap,
    pub includes: IncludeGraph,
}

/// Maps expanded-token ranges back to original source spans.
///
/// Identity preprocessing maps every range to the same range in `file`.
/// When real macro expansion lands, this will carry per-token origin info.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceMap {
    file: FileId,
}

impl SourceMap {
    /// Map an expanded-token range back to a source span.
    ///
    /// Identity implementation: the range is unchanged, attributed to `self.file`.
    pub(crate) fn new(file: FileId) -> Self {
        Self { file }
    }

    pub fn map_span(&self, range: TextRange) -> Span {
        Span {
            file: self.file,
            range,
        }
    }

    /// The file this source map belongs to.
    pub fn file(&self) -> FileId {
        self.file
    }
}

/// Direct include dependencies for a single file.
///
/// Identity preprocessing leaves this empty. When `` `include `` support
/// lands, this will list the files directly included by this file.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct IncludeGraph {
    deps: Vec<FileId>,
}

impl IncludeGraph {
    /// Direct include dependencies for this file.
    pub fn dependencies(&self) -> &[FileId] {
        &self.deps
    }

    /// Whether this file has no include dependencies.
    pub fn is_empty(&self) -> bool {
        self.deps.is_empty()
    }
}

/// Preprocess a token stream for a single file.
///
/// Currently an identity pass: tokens are cloned unchanged, the source map
/// maps every range back to itself, and the include graph is empty.
pub fn preprocess(file: FileId, tokens: &[Token]) -> PreprocOutput {
    PreprocOutput {
        tokens: tokens.to_vec(),
        source_map: SourceMap::new(file),
        includes: IncludeGraph::default(),
    }
}
