use lyra_source::{ExpansionFrame, ExpansionKind, FileId, FileLoc, Span, TextRange, TextSize};

/// A contiguous range in the expanded output that originated from a
/// specific source location.
///
/// Identity segments map back to the primary file. Expansion segments
/// (includes, future macros) map to a different file and carry a
/// non-empty `call_site`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Segment {
    pub expanded_range: TextRange,
    pub origin: Span,
    /// The call-site span that caused expansion (e.g., the full
    /// `` `include "x.sv" `` directive). Zero-range for identity
    /// segments.
    pub call_site: Span,
}

impl Segment {
    fn is_identity(&self) -> bool {
        self.call_site.range.is_empty()
    }
}

/// Maps expanded-output positions back to original source spans.
///
/// Every byte in the expanded output is covered by an explicit
/// segment. Identity segments map primary-file bytes back to their
/// original offsets. Expansion segments (includes) map to the
/// included file.
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
    /// source span (file + offset).
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
            // No segment covers this offset. This can happen at the
            // exact end of expanded text when it equals a segment
            // boundary, or for empty expanded text.
            Some(Span {
                file: self.file,
                range: TextRange::empty(offset),
            })
        }
    }

    /// Map a range in the expanded output to an original source span.
    ///
    /// Returns `Some` only if the entire range falls within one
    /// segment. Returns `None` if the range straddles segment
    /// boundaries or extends past the expanded text.
    pub fn map_range(&self, range: TextRange) -> Option<Span> {
        if range.is_empty() {
            return self.map_point(range.start());
        }

        let start_span = self.map_point(range.start())?;
        let last = range.end() - TextSize::new(1);
        let end_span = self.map_point(last)?;

        if start_span.file != end_span.file {
            return None;
        }

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
    /// When a range straddles segment boundaries, only the start point
    /// is mapped. Returns `None` if the offset is out of bounds.
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
    pub fn expansion_frame(&self, offset: TextSize) -> Option<ExpansionFrame> {
        let seg = self.find_segment(offset)?;
        if seg.is_identity() {
            return None;
        }
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
