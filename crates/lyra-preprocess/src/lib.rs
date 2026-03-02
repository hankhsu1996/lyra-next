mod directive;
mod engine;
mod env;
mod source_map;

use lyra_lexer::{SyntaxKind, Token};
use lyra_source::{FileId, Span, TextRange, TextSize};
use smol_str::SmolStr;

pub use crate::directive::DirectiveKeyword;
pub use crate::env::{MacroDef, MacroEnv, MacroTok, MacroTokenSeq, MacroValue};
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

/// Whether a directive event originated at the top level or inside a
/// macro expansion.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DirectiveEventOrigin {
    TopLevel,
    MacroExpansion,
}

/// A directive encountered during preprocessing that was not handled
/// by the preprocessor itself (e.g., `` `timescale ``, `` `default_nettype ``).
///
/// Preserved as a side-channel so later stages can consume directives
/// they care about, while the output token stream stays directive-free.
///
/// Events are totally ordered by `(expanded_offset, event_seq)`.
/// `expanded_offset` records the exact byte position in the expanded
/// output where the directive would have appeared. `event_seq` is a
/// monotonically increasing counter that breaks ties when multiple
/// events share the same offset (e.g., consecutive stripped
/// directives). This ordering is deterministic within a single
/// preprocess invocation and provides a stable merge key for future
/// parallel include processing.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DirectiveEvent {
    pub span: Span,
    pub kind: DirectiveEventKind,
    pub origin: DirectiveEventOrigin,
    /// Byte offset in the expanded output at the point this event was
    /// produced. For top-level events this is the current expanded
    /// text length after flushing identity; for events inside a macro
    /// body this is the expansion base offset plus bytes emitted so
    /// far within the expansion.
    pub expanded_offset: TextSize,
    /// Monotonically increasing sequence number for deterministic
    /// ordering when multiple events share the same `expanded_offset`.
    pub event_seq: u32,
}

/// Classification of a directive event.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DirectiveEventKind {
    /// A recognized LRM directive keyword encountered outside the
    /// preprocessor's own handling (e.g., `` `timescale ``).
    KnownDirective(DirectiveKeyword),
    /// Use of an undefined macro name.
    UndefinedMacro(SmolStr),
    /// Malformed or unrecognized directive form.
    UnrecognizedDirective(SmolStr),
}

const DEFAULT_MACRO_RECURSION_LIMIT: usize = 64;

/// Inputs for preprocessing a single file.
pub struct PreprocessInputs<'a> {
    pub file: FileId,
    pub tokens: &'a [Token],
    pub text: &'a str,
    pub provider: &'a dyn IncludeProvider,
    pub starting_env: &'a MacroEnv,
    /// Maximum nesting depth for recursive macro expansion.
    /// Defaults to 64.
    pub macro_recursion_limit: usize,
}

impl PreprocessInputs<'_> {
    /// Default macro recursion limit (64).
    pub const DEFAULT_RECURSION_LIMIT: usize = DEFAULT_MACRO_RECURSION_LIMIT;
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
        macro_recursion_limit: DEFAULT_MACRO_RECURSION_LIMIT,
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
