use lyra_source::{ExpansionFrame, ExpansionKind, FileId, FileLoc, Span, TextRange, TextSize};

/// What produced a source-map segment.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum SegmentKind {
    /// Bytes copied verbatim from the primary file.
    Identity,
    /// Bytes from an included file. `call_site` points to the
    /// `` `include `` directive in the parent file (distinct from
    /// `Segment::origin` which points into the included file).
    Include { call_site: Span },
    /// Bytes produced by macro expansion. The call site (`` `FOO ``
    /// token) is stored in `Segment::origin`.
    Macro,
}

/// A contiguous range in the expanded output that originated from a
/// specific source location.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Segment {
    pub expanded_range: TextRange,
    pub origin: Span,
    pub kind: SegmentKind,
}

impl Segment {
    fn is_identity(&self) -> bool {
        matches!(self.kind, SegmentKind::Identity)
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
            segments.iter().all(|s| s.is_identity()
                || matches!(s.kind, SegmentKind::Macro)
                || s.expanded_range.len() == s.origin.range.len()),
            "non-macro expansion segment length must match origin length",
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
            let mapped_delta = if matches!(seg.kind, SegmentKind::Macro) {
                std::cmp::min(
                    delta,
                    seg.origin
                        .range
                        .len()
                        .checked_sub(TextSize::new(1))
                        .unwrap_or(TextSize::new(0)),
                )
            } else {
                delta
            };
            Some(Span {
                file: seg.origin.file,
                range: TextRange::empty(seg.origin.range.start() + mapped_delta),
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
        let delta = offset - seg.expanded_range.start();
        match &seg.kind {
            SegmentKind::Identity => None,
            SegmentKind::Include { call_site } => Some(ExpansionFrame {
                kind: ExpansionKind::Include,
                call_site: *call_site,
                spelling: FileLoc {
                    file: seg.origin.file,
                    offset: seg.origin.range.start() + delta,
                },
            }),
            SegmentKind::Macro => {
                let clamped = std::cmp::min(
                    delta,
                    seg.origin
                        .range
                        .len()
                        .checked_sub(TextSize::new(1))
                        .unwrap_or(TextSize::new(0)),
                );
                Some(ExpansionFrame {
                    kind: ExpansionKind::Macro,
                    call_site: seg.origin,
                    spelling: FileLoc {
                        file: seg.origin.file,
                        offset: seg.origin.range.start() + clamped,
                    },
                })
            }
        }
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
