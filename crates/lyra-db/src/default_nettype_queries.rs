use lyra_preprocess::{DefaultNettypeValue, DirectiveEventKind, DirectiveKeyword};
use lyra_source::{Span, TextSize};

use crate::SourceFile;
use crate::pipeline::preprocess_file;

/// One policy change point in a file from a `` `default_nettype `` directive.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DefaultNettypePolicyEntry {
    pub value: DefaultNettypeValue,
    /// Expanded-file offset at which this directive takes effect.
    pub expanded_offset: TextSize,
    pub directive_span: Span,
    pub value_span: Span,
}

/// Per-file `` `default_nettype `` policy summary.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct FileDefaultNettypeSummary {
    /// Ordered by `expanded_offset`, stable and deterministic.
    pub entries: Vec<DefaultNettypePolicyEntry>,
    /// Expanded offsets of `` `resetall `` directives.
    pub resetall_offsets: Vec<TextSize>,
}

/// Build a per-file summary of `` `default_nettype `` directives and
/// `` `resetall `` positions from the preprocessed directive event stream.
///
/// The preprocessor already parses directive arguments into
/// `DefaultNettypeValue`, so this query is a thin projection from
/// directive events into an ordered policy timeline.
#[salsa::tracked(return_ref)]
pub fn file_default_nettype_summary(
    db: &dyn salsa::Database,
    file: SourceFile,
) -> FileDefaultNettypeSummary {
    let pp = preprocess_file(db, file);
    let mut entries = Vec::new();
    let mut resetall_offsets = Vec::new();

    for event in &pp.directive_events {
        match &event.kind {
            DirectiveEventKind::DefaultNettype(dn) => {
                entries.push(DefaultNettypePolicyEntry {
                    value: dn.value,
                    expanded_offset: event.expanded_offset,
                    directive_span: event.span,
                    value_span: dn.value_span,
                });
            }
            DirectiveEventKind::KnownDirective(DirectiveKeyword::Resetall) => {
                resetall_offsets.push(event.expanded_offset);
            }
            _ => {}
        }
    }

    FileDefaultNettypeSummary {
        entries,
        resetall_offsets,
    }
}

/// Find the active `default_nettype` policy at a given expanded offset,
/// accounting for `` `resetall `` directives. Returns `Wire` when no
/// directive is active (LRM 22.8 default).
pub fn active_default_nettype_at(
    summary: &FileDefaultNettypeSummary,
    offset: TextSize,
) -> DefaultNettypeValue {
    let mut active: Option<DefaultNettypeValue> = None;

    for entry in &summary.entries {
        if entry.expanded_offset > offset {
            break;
        }
        let reset = summary
            .resetall_offsets
            .iter()
            .any(|&r| r > entry.expanded_offset && r <= offset);
        if reset {
            active = None;
        } else {
            active = Some(entry.value);
        }
    }

    active.unwrap_or(DefaultNettypeValue::Wire)
}

/// Whether the active policy at `offset` is `none` (implicit net creation
/// disabled).
pub fn default_nettype_is_none_at(summary: &FileDefaultNettypeSummary, offset: TextSize) -> bool {
    active_default_nettype_at(summary, offset) == DefaultNettypeValue::None
}

#[cfg(test)]
mod tests {
    use lyra_preprocess::DefaultNettypeValue;
    use lyra_source::{FileId, Span, TextRange, TextSize};

    use super::{DefaultNettypePolicyEntry, FileDefaultNettypeSummary, active_default_nettype_at};

    fn entry(value: DefaultNettypeValue, offset: u32) -> DefaultNettypePolicyEntry {
        DefaultNettypePolicyEntry {
            value,
            expanded_offset: TextSize::new(offset),
            directive_span: Span {
                file: FileId(0),
                range: TextRange::empty(TextSize::new(0)),
            },
            value_span: Span {
                file: FileId(0),
                range: TextRange::empty(TextSize::new(0)),
            },
        }
    }

    #[test]
    fn no_entries_returns_wire() {
        let summary = FileDefaultNettypeSummary::default();
        assert_eq!(
            active_default_nettype_at(&summary, TextSize::new(100)),
            DefaultNettypeValue::Wire,
        );
    }

    #[test]
    fn single_entry_before_offset() {
        let summary = FileDefaultNettypeSummary {
            entries: vec![entry(DefaultNettypeValue::Tri, 10)],
            resetall_offsets: vec![],
        };
        assert_eq!(
            active_default_nettype_at(&summary, TextSize::new(50)),
            DefaultNettypeValue::Tri,
        );
    }

    #[test]
    fn entry_after_offset_ignored() {
        let summary = FileDefaultNettypeSummary {
            entries: vec![entry(DefaultNettypeValue::Tri, 100)],
            resetall_offsets: vec![],
        };
        assert_eq!(
            active_default_nettype_at(&summary, TextSize::new(50)),
            DefaultNettypeValue::Wire,
        );
    }

    #[test]
    fn resetall_clears_active() {
        let summary = FileDefaultNettypeSummary {
            entries: vec![entry(DefaultNettypeValue::None, 10)],
            resetall_offsets: vec![TextSize::new(30)],
        };
        assert_eq!(
            active_default_nettype_at(&summary, TextSize::new(50)),
            DefaultNettypeValue::Wire,
        );
    }

    #[test]
    fn resetall_before_entry_does_not_clear() {
        let summary = FileDefaultNettypeSummary {
            entries: vec![entry(DefaultNettypeValue::Uwire, 30)],
            resetall_offsets: vec![TextSize::new(10)],
        };
        assert_eq!(
            active_default_nettype_at(&summary, TextSize::new(50)),
            DefaultNettypeValue::Uwire,
        );
    }

    #[test]
    fn multiple_entries_picks_last() {
        let summary = FileDefaultNettypeSummary {
            entries: vec![
                entry(DefaultNettypeValue::Tri, 10),
                entry(DefaultNettypeValue::Wand, 30),
            ],
            resetall_offsets: vec![],
        };
        assert_eq!(
            active_default_nettype_at(&summary, TextSize::new(50)),
            DefaultNettypeValue::Wand,
        );
    }

    #[test]
    fn none_value_detected() {
        let summary = FileDefaultNettypeSummary {
            entries: vec![entry(DefaultNettypeValue::None, 10)],
            resetall_offsets: vec![],
        };
        assert_eq!(
            active_default_nettype_at(&summary, TextSize::new(50)),
            DefaultNettypeValue::None,
        );
        assert!(super::default_nettype_is_none_at(
            &summary,
            TextSize::new(50)
        ));
    }

    #[test]
    fn alias_spellings_preserved() {
        let summary = FileDefaultNettypeSummary {
            entries: vec![
                entry(DefaultNettypeValue::Triand, 10),
                entry(DefaultNettypeValue::Trior, 30),
            ],
            resetall_offsets: vec![],
        };
        assert_eq!(
            active_default_nettype_at(&summary, TextSize::new(20)),
            DefaultNettypeValue::Triand,
        );
        assert_eq!(
            active_default_nettype_at(&summary, TextSize::new(50)),
            DefaultNettypeValue::Trior,
        );
    }
}
