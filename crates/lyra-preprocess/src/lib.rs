use lyra_lexer::{SyntaxKind, Token};
use lyra_source::{ExpansionFrame, ExpansionKind, FileId, FileLoc, Span, TextRange, TextSize};

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

/// A preprocess error (e.g., unresolved include).
///
/// Range is in expanded-output space. Callers use [`SourceMap`] to
/// convert to original source spans.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PreprocError {
    pub range: TextRange,
    pub message: String,
}

/// Bundled output of preprocessing a single file.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PreprocOutput {
    pub tokens: Vec<Token>,
    pub expanded_text: String,
    pub source_map: SourceMap,
    pub includes: IncludeGraph,
    pub errors: Vec<PreprocError>,
}

// SourceMap: segment-based splice mapping

/// A contiguous range in the expanded output that originated from a
/// different file (an included file). Ranges NOT covered by any segment
/// map to the primary file at the same offset (identity).
#[derive(Debug, Clone, PartialEq, Eq)]
struct Segment {
    expanded_range: TextRange,
    origin: Span,
    /// The full span of the include directive in the original source
    /// (e.g., `` `include "x.sv" ``). Used to populate
    /// `ExpansionFrame::call_site` for "included from here" notes.
    call_site: Span,
}

/// Maps expanded-output positions back to original source spans.
///
/// Uses segment-based splice mapping: non-identity regions (included
/// content) are recorded as segments. Gaps between segments map to the
/// primary file at the same offset (identity fallback).
///
/// The core primitive is [`map_point`](SourceMap::map_point) (single
/// offset). Range mapping is derived with strict containment semantics.
/// This API is forward-compatible with future macro expansion -- only
/// the internal representation would change.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceMap {
    file: FileId,
    segments: Vec<Segment>,
    expanded_len: TextSize,
}

impl SourceMap {
    pub(crate) fn new(file: FileId, segments: Vec<Segment>, expanded_len: TextSize) -> Self {
        debug_assert!(
            segments
                .windows(2)
                .all(|w| w[0].expanded_range.end() <= w[1].expanded_range.start()),
            "segments must be sorted and non-overlapping",
        );
        debug_assert!(
            segments
                .iter()
                .all(|s| s.expanded_range.len() == s.origin.range.len()),
            "segment expanded length must match origin length",
        );
        Self {
            file,
            segments,
            expanded_len,
        }
    }

    /// Map a single byte offset in the expanded output to an original
    /// source span (file + offset). This is the core primitive.
    ///
    /// Returns `None` if `offset` is past the end of the expanded text.
    pub fn map_point(&self, offset: TextSize) -> Option<Span> {
        if offset > self.expanded_len {
            return None;
        }
        if let Some(seg) = self.find_segment(offset) {
            let delta = offset - seg.expanded_range.start();
            Some(Span {
                file: seg.origin.file,
                range: TextRange::empty(seg.origin.range.start() + delta),
            })
        } else {
            Some(Span {
                file: self.file,
                range: TextRange::empty(offset),
            })
        }
    }

    /// Map a range in the expanded output to an original source span.
    ///
    /// Returns `Some` only if the entire range falls within one segment
    /// (or entirely within identity-mapped gaps belonging to the same
    /// file). Returns `None` if the range straddles segment boundaries
    /// or extends past the expanded text.
    pub fn map_range(&self, range: TextRange) -> Option<Span> {
        if range.is_empty() {
            return self.map_point(range.start());
        }

        let start_span = self.map_point(range.start())?;
        // end is exclusive, so map the last inclusive byte
        let last = range.end() - TextSize::new(1);
        let end_span = self.map_point(last)?;

        if start_span.file != end_span.file {
            return None;
        }

        // Check that start and end are in the same "zone" (both in the
        // same segment, or both in identity gaps)
        let start_seg = self.find_segment(range.start());
        let end_seg = self.find_segment(last);
        match (start_seg, end_seg) {
            (Some(s), Some(e)) => {
                if s.expanded_range != e.expanded_range {
                    return None;
                }
            }
            (None, None) => {}
            _ => return None,
        }

        let mapped_start = start_span.range.start();
        let mapped_end = end_span.range.start() + TextSize::new(1);
        Some(Span {
            file: start_span.file,
            range: TextRange::new(mapped_start, mapped_end),
        })
    }

    /// Convenience for diagnostics: maps via `map_point(range.start())`.
    ///
    /// When a range straddles segment boundaries (e.g., spans both
    /// included and main-file content), only the start point is mapped.
    /// Callers should not assume the returned span covers the full
    /// original range -- it identifies the origin file and start offset
    /// only.
    ///
    /// Returns `None` if the offset is out of bounds.
    pub fn map_span(&self, range: TextRange) -> Option<Span> {
        self.map_point(range.start())
    }

    /// Resolve an expanded-output offset to its physical file location.
    pub fn resolve_file_loc(&self, offset: TextSize) -> Option<FileLoc> {
        let span = self.map_point(offset)?;
        Some(FileLoc {
            file: span.file,
            offset: span.range.start(),
        })
    }

    /// Return the local expansion frame for an offset.
    ///
    /// Returns `None` for identity-mapped positions (text from the
    /// primary file). Returns `Some` for positions from included files.
    /// Each per-file `SourceMap` produces at most one frame (the
    /// direct include). Transitive chaining is a DB-layer concern
    /// once the preprocessor gains recursive expansion.
    pub fn expansion_frame(&self, offset: TextSize) -> Option<ExpansionFrame> {
        let seg = self.find_segment(offset)?;
        let delta = offset - seg.expanded_range.start();
        Some(ExpansionFrame {
            kind: ExpansionKind::Include,
            call_site: seg.call_site,
            spelling: FileLoc {
                file: seg.origin.file,
                offset: seg.origin.range.start() + delta,
            },
        })
    }

    /// The primary file this source map belongs to.
    pub fn file(&self) -> FileId {
        self.file
    }

    fn find_segment(&self, offset: TextSize) -> Option<&Segment> {
        let idx = self
            .segments
            .partition_point(|s| s.expanded_range.end() <= offset);
        if idx < self.segments.len() && self.segments[idx].expanded_range.contains(offset) {
            Some(&self.segments[idx])
        } else {
            None
        }
    }
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
/// Handles `` `include `` directives by calling the provider to resolve
/// included file content. One level of includes only -- include
/// directives inside included files are not expanded.
///
/// For identity preprocessing (no includes), pass a provider that
/// returns `None` for all paths, or use [`preprocess_identity`].
pub fn preprocess(
    file: FileId,
    tokens: &[Token],
    text: &str,
    provider: &dyn IncludeProvider,
) -> PreprocOutput {
    let mut out_tokens: Vec<Token> = Vec::with_capacity(tokens.len());
    let mut expanded_text = String::with_capacity(text.len());
    let mut segments: Vec<Segment> = Vec::new();
    let mut includes = IncludeGraph::default();
    let mut errors: Vec<PreprocError> = Vec::new();

    // Byte cursor in the original `text`, tracking how far we have consumed.
    let mut src_cursor: usize = 0;
    // Byte cursor in the expanded output, tracking how far we have written.
    let mut exp_cursor: usize = 0;
    // Start of the current pending flush region in `text`.
    let mut flush_start: usize = 0;

    let mut i = 0;
    while i < tokens.len() {
        let tok = tokens[i];

        if tok.kind == SyntaxKind::Eof {
            out_tokens.push(tok);
            break;
        }

        let tok_len: usize = tok.len.into();
        let tok_text = &text[src_cursor..src_cursor + tok_len];

        if tok.kind == SyntaxKind::Directive
            && tok_text == "`include"
            && let Some((path, path_range, skip_count)) =
                find_include_path(tokens, text, i, src_cursor)
        {
            if let Some(resolved) = provider.resolve(&path) {
                // Flush pending main-file text up to the directive
                let pending = &text[flush_start..src_cursor];
                if !pending.is_empty() {
                    expanded_text.push_str(pending);
                    exp_cursor += pending.len();
                }

                // Record the included content as a segment
                let inc_start = TextSize::new(exp_cursor as u32);
                let inc_len = resolved.text.len();

                // Append included tokens (excluding Eof)
                for inc_tok in resolved.tokens {
                    if inc_tok.kind == SyntaxKind::Eof {
                        break;
                    }
                    out_tokens.push(*inc_tok);
                }
                expanded_text.push_str(resolved.text);

                let inc_end = TextSize::new((exp_cursor + inc_len) as u32);

                // Compute the call site: full span of the include directive
                let skip_bytes: usize = tokens[i..i + skip_count]
                    .iter()
                    .map(|t| usize::from(t.len))
                    .sum();
                let call_site = Span {
                    file,
                    range: TextRange::new(
                        TextSize::new(src_cursor as u32),
                        TextSize::new((src_cursor + skip_bytes) as u32),
                    ),
                };

                segments.push(Segment {
                    expanded_range: TextRange::new(inc_start, inc_end),
                    origin: Span {
                        file: resolved.file_id,
                        range: TextRange::new(TextSize::new(0), TextSize::new(inc_len as u32)),
                    },
                    call_site,
                });
                exp_cursor += inc_len;

                includes.deps.push(resolved.file_id);
                src_cursor += skip_bytes;
                flush_start = src_cursor;
                i += skip_count;
                continue;
            }
            // Unresolved include: keep tokens, record error
            let exp_offset = exp_cursor + (src_cursor - flush_start);
            let mapped_range = TextRange::new(
                TextSize::new(exp_offset as u32) + path_range.start()
                    - TextSize::new(src_cursor as u32),
                TextSize::new(exp_offset as u32) + path_range.end()
                    - TextSize::new(src_cursor as u32),
            );
            errors.push(PreprocError {
                range: mapped_range,
                message: format!("unresolved include: \"{path}\""),
            });
            // Fall through to copy token as-is
        }

        // Default: copy token to output
        out_tokens.push(tok);
        src_cursor += tok_len;
        i += 1;
    }

    // Flush remaining main-file text
    let remaining = &text[flush_start..];
    if !remaining.is_empty() {
        expanded_text.push_str(remaining);
    }

    let expanded_len = TextSize::new(expanded_text.len() as u32);
    PreprocOutput {
        tokens: out_tokens,
        source_map: SourceMap::new(file, segments, expanded_len),
        expanded_text,
        includes,
        errors,
    }
}

/// Identity preprocessing: no include resolution.
pub fn preprocess_identity(file: FileId, tokens: &[Token], text: &str) -> PreprocOutput {
    preprocess(file, tokens, text, &NoOpProvider)
}

/// Lightweight scan for `` `include `` directives. Returns the quoted
/// paths without performing any expansion. Useful for the tool layer
/// to discover include dependencies before setting up resolution.
pub fn scan_includes(tokens: &[Token], text: &str) -> Vec<String> {
    let mut paths = Vec::new();
    let mut cursor: usize = 0;

    for (i, tok) in tokens.iter().enumerate() {
        let tok_len: usize = tok.len.into();
        let tok_text = &text[cursor..cursor + tok_len];

        if tok.kind == SyntaxKind::Directive
            && tok_text == "`include"
            && let Some((path, _, _)) = find_include_path(tokens, text, i, cursor)
        {
            paths.push(path);
        }
        cursor += tok_len;
    }

    paths
}

/// Find the include path after a `` `include `` directive token.
///
/// Returns `(path, path_token_range_in_text, token_count_to_skip)` where
/// `token_count_to_skip` includes the directive token itself plus any
/// whitespace and the string literal.
fn find_include_path(
    tokens: &[Token],
    text: &str,
    directive_idx: usize,
    directive_cursor: usize,
) -> Option<(String, TextRange, usize)> {
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

    let path_text = &text[cursor..cursor + tok_len];
    // Strip surrounding quotes
    if path_text.len() < 2 {
        return None;
    }
    let path = path_text[1..path_text.len() - 1].to_string();
    let path_range = TextRange::new(
        TextSize::new(cursor as u32),
        TextSize::new((cursor + tok_len) as u32),
    );

    // skip_count = directive + whitespace + string literal
    let skip_count = j - directive_idx + 1;
    Some((path, path_range, skip_count))
}
