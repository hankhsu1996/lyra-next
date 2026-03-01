mod engine;
mod env;
mod source_map;

use lyra_lexer::{SyntaxKind, Token};
use lyra_source::{FileId, Span, TextRange, TextSize};
use smol_str::SmolStr;

pub use crate::env::{MacroDef, MacroEnv, MacroValue};
pub use crate::source_map::SourceMap;

/// A resolved include file returned by an [`IncludeProvider`].
///
/// Borrows tokens and text from the provider, avoiding copies on the
/// hot path. The DB provider returns references into Salsa-cached
/// data; test providers return references to pre-lexed storage.
#[derive(Debug, Clone, Copy)]
pub struct ResolvedInclude<'a> {
    pub file_id: FileId,
    pub tokens: &'a [Token],
    pub text: &'a str,
}

/// Callback for resolving `` `include `` paths to file contents.
///
/// The DB layer implements this by reading through Salsa queries,
/// which establishes incremental dependencies. The borrowed return
/// type ensures the provider contract does not force allocations.
pub trait IncludeProvider {
    fn resolve(&self, path: &str) -> Option<ResolvedInclude<'_>>;
}

/// A preprocess error (e.g., unresolved include, malformed directive).
///
/// `span` is always in physical/origin space (`FileId` + range in the
/// original source text).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PreprocError {
    pub span: Span,
    pub message: SmolStr,
}

/// A directive encountered during preprocessing that was not handled
/// by the preprocessor itself (e.g., `` `timescale ``, `` `default_nettype ``).
///
/// Preserved as a side-channel so later stages can consume directives
/// they care about, while the output token stream stays directive-free.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DirectiveEvent {
    pub span: Span,
    pub kind: DirectiveEventKind,
}

/// Classification of a directive event.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DirectiveEventKind {
    /// A directive the preprocessor does not handle.
    Unknown(SmolStr),
}

/// Inputs for preprocessing a single file.
pub struct PreprocessInputs<'a> {
    pub file: FileId,
    pub tokens: &'a [Token],
    pub text: &'a str,
    pub provider: &'a dyn IncludeProvider,
    pub starting_env: &'a MacroEnv,
}

/// Bundled output of preprocessing a single file.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PreprocOutput {
    pub tokens: Vec<Token>,
    pub expanded_text: String,
    pub source_map: SourceMap,
    pub includes: IncludeGraph,
    pub errors: Vec<PreprocError>,
    pub final_env: MacroEnv,
    pub directive_events: Vec<DirectiveEvent>,
}

/// Direct include dependencies for a single file.
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

    pub(crate) fn push(&mut self, file: FileId) {
        if !self.deps.contains(&file) {
            self.deps.push(file);
        }
    }
}

/// No-op include provider that never resolves anything.
struct NoOpProvider;

impl IncludeProvider for NoOpProvider {
    fn resolve(&self, _path: &str) -> Option<ResolvedInclude<'_>> {
        None
    }
}

/// Preprocess a token stream for a single file.
///
/// Handles conditional compilation (`` `ifdef ``/`` `ifndef ``/
/// `` `elsif ``/`` `else ``/`` `endif ``), object-like macro
/// definitions (`` `define ``/`` `undef ``), and `` `include ``
/// directives. All directive tokens are stripped from the output.
pub fn preprocess(inputs: &PreprocessInputs<'_>) -> PreprocOutput {
    engine::Preprocessor::new(inputs).run()
}

/// Identity preprocessing: no include resolution, no starting macros.
pub fn preprocess_identity(file: FileId, tokens: &[Token], text: &str) -> PreprocOutput {
    let env = MacroEnv::empty();
    preprocess(&PreprocessInputs {
        file,
        tokens,
        text,
        provider: &NoOpProvider,
        starting_env: &env,
    })
}

/// Lightweight scan for `` `include `` directives. Returns the quoted
/// paths without performing any expansion. Useful for the tool layer
/// to discover include dependencies before setting up resolution.
pub fn scan_includes(tokens: &[Token], text: &str) -> Vec<TextRange> {
    let mut paths = Vec::new();
    let mut cursor: usize = 0;

    for (i, tok) in tokens.iter().enumerate() {
        let tok_len: usize = tok.len.into();
        let tok_text = &text[cursor..cursor + tok_len];

        if tok.kind == SyntaxKind::Directive
            && tok_text == "`include"
            && let Some((inner_range, _, _)) = find_include_path(tokens, i, cursor)
        {
            paths.push(inner_range);
        }
        cursor += tok_len;
    }

    paths
}

/// Find the include path after a `` `include `` directive token.
///
/// Returns `(inner_range, literal_range, token_count_to_skip)` where
/// `inner_range` is the path text without quotes, `literal_range` is
/// the full string literal span, and `token_count_to_skip` includes
/// the directive token itself plus any whitespace and the string
/// literal.
fn find_include_path(
    tokens: &[Token],
    directive_idx: usize,
    directive_cursor: usize,
) -> Option<(TextRange, TextRange, usize)> {
    let mut cursor = directive_cursor;
    let dir_len: usize = tokens[directive_idx].len.into();
    cursor += dir_len;

    let mut j = directive_idx + 1;
    // Skip whitespace tokens
    while j < tokens.len() && tokens[j].kind == SyntaxKind::Whitespace {
        let tok_len: usize = tokens[j].len.into();
        cursor += tok_len;
        j += 1;
    }

    if j >= tokens.len() {
        return None;
    }

    let path_tok = tokens[j];
    let tok_len: usize = path_tok.len.into();

    if path_tok.kind != SyntaxKind::StringLiteral {
        return None;
    }

    if tok_len < 2 {
        return None;
    }

    let inner_range = TextRange::new(
        TextSize::new((cursor + 1) as u32),
        TextSize::new((cursor + tok_len - 1) as u32),
    );
    let literal_range = TextRange::new(
        TextSize::new(cursor as u32),
        TextSize::new((cursor + tok_len) as u32),
    );

    let skip_count = j - directive_idx + 1;
    Some((inner_range, literal_range, skip_count))
}
