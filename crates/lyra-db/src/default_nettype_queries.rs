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
    /// Expanded offsets of `` `resetall `` directives, ordered by offset.
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

/// Convert a `DefaultNettypeValue` (preprocess layer) to an `ActiveNetType`
/// (semantic layer).
fn to_active_net_type(v: DefaultNettypeValue) -> lyra_semantic::default_nettype::ActiveNetType {
    use lyra_semantic::default_nettype::ActiveNetType;
    match v {
        DefaultNettypeValue::Wire => ActiveNetType::Wire,
        DefaultNettypeValue::Tri => ActiveNetType::Tri,
        DefaultNettypeValue::Tri0 => ActiveNetType::Tri0,
        DefaultNettypeValue::Tri1 => ActiveNetType::Tri1,
        DefaultNettypeValue::Wand => ActiveNetType::Wand,
        DefaultNettypeValue::Triand => ActiveNetType::Triand,
        DefaultNettypeValue::Wor => ActiveNetType::Wor,
        DefaultNettypeValue::Trior => ActiveNetType::Trior,
        DefaultNettypeValue::Trireg => ActiveNetType::Trireg,
        DefaultNettypeValue::Uwire => ActiveNetType::Uwire,
        DefaultNettypeValue::None => ActiveNetType::None,
    }
}

/// Build a `DefaultNettypePolicy` from the preprocessor summary.
///
/// Translates each directive change point and `resetall` position into
/// a sorted `(TextSize, ActiveNetType)` timeline that the semantic
/// resolve layer queries by site anchor.
pub fn build_default_nettype_policy(
    summary: &FileDefaultNettypeSummary,
) -> lyra_semantic::default_nettype::DefaultNettypePolicy {
    use lyra_semantic::default_nettype::ActiveNetType;

    let mut changes: Vec<(TextSize, ActiveNetType)> = Vec::new();

    // Merge directive entries and resetall offsets into a sorted timeline.
    // Resetall resets to Wire (LRM 22.8 default).
    let mut dir_iter = summary.entries.iter().peekable();
    let mut reset_iter = summary.resetall_offsets.iter().peekable();

    loop {
        let reset_offset = reset_iter.peek().copied().copied();

        match (dir_iter.peek(), reset_offset) {
            (Some(entry), Some(r)) if r <= entry.expanded_offset => {
                changes.push((r, ActiveNetType::Wire));
                reset_iter.next();
            }
            (Some(_), _) => {
                if let Some(entry) = dir_iter.next() {
                    changes.push((entry.expanded_offset, to_active_net_type(entry.value)));
                }
            }
            (None, Some(r)) => {
                changes.push((r, ActiveNetType::Wire));
                reset_iter.next();
            }
            (None, None) => break,
        }
    }

    lyra_semantic::default_nettype::DefaultNettypePolicy::new(changes)
}

#[cfg(test)]
mod tests {
    use lyra_preprocess::DefaultNettypeValue;
    use lyra_source::{FileId, Span, TextRange, TextSize};

    use super::{
        DefaultNettypePolicyEntry, FileDefaultNettypeSummary, active_default_nettype_at,
        build_default_nettype_policy,
    };
    use lyra_semantic::default_nettype::ActiveNetType;

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
        assert_eq!(
            super::active_default_nettype_at(&summary, TextSize::new(50)),
            DefaultNettypeValue::None,
        );
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

    // build_default_nettype_policy conversion tests

    #[test]
    fn policy_empty_summary() {
        let summary = FileDefaultNettypeSummary::default();
        let policy = build_default_nettype_policy(&summary);
        assert_eq!(
            policy.active_at_offset(TextSize::new(100)),
            ActiveNetType::Wire
        );
    }

    #[test]
    fn policy_directive_at_exact_offset() {
        let summary = FileDefaultNettypeSummary {
            entries: vec![entry(DefaultNettypeValue::None, 10)],
            resetall_offsets: vec![],
        };
        let policy = build_default_nettype_policy(&summary);
        assert_eq!(
            policy.active_at_offset(TextSize::new(10)),
            ActiveNetType::None
        );
    }

    #[test]
    fn policy_interleaved_directive_and_resetall() {
        let summary = FileDefaultNettypeSummary {
            entries: vec![
                entry(DefaultNettypeValue::None, 10),
                entry(DefaultNettypeValue::Tri, 50),
            ],
            resetall_offsets: vec![TextSize::new(30)],
        };
        let policy = build_default_nettype_policy(&summary);
        // Before any directive
        assert_eq!(
            policy.active_at_offset(TextSize::new(5)),
            ActiveNetType::Wire
        );
        // After first directive
        assert_eq!(
            policy.active_at_offset(TextSize::new(20)),
            ActiveNetType::None
        );
        // After resetall at 30
        assert_eq!(
            policy.active_at_offset(TextSize::new(40)),
            ActiveNetType::Wire
        );
        // After second directive at 50
        assert_eq!(
            policy.active_at_offset(TextSize::new(60)),
            ActiveNetType::Tri
        );
    }

    #[test]
    fn policy_resetall_restores_wire() {
        let summary = FileDefaultNettypeSummary {
            entries: vec![entry(DefaultNettypeValue::Wand, 10)],
            resetall_offsets: vec![TextSize::new(30)],
        };
        let policy = build_default_nettype_policy(&summary);
        assert_eq!(
            policy.active_at_offset(TextSize::new(20)),
            ActiveNetType::Wand
        );
        assert_eq!(
            policy.active_at_offset(TextSize::new(50)),
            ActiveNetType::Wire
        );
    }

    #[test]
    fn policy_later_directive_overrides_earlier() {
        let summary = FileDefaultNettypeSummary {
            entries: vec![
                entry(DefaultNettypeValue::Tri, 10),
                entry(DefaultNettypeValue::None, 30),
            ],
            resetall_offsets: vec![],
        };
        let policy = build_default_nettype_policy(&summary);
        assert_eq!(
            policy.active_at_offset(TextSize::new(20)),
            ActiveNetType::Tri
        );
        assert_eq!(
            policy.active_at_offset(TextSize::new(50)),
            ActiveNetType::None
        );
    }

    #[test]
    fn policy_none_survives_until_next_change() {
        let summary = FileDefaultNettypeSummary {
            entries: vec![
                entry(DefaultNettypeValue::None, 10),
                entry(DefaultNettypeValue::Wire, 100),
            ],
            resetall_offsets: vec![],
        };
        let policy = build_default_nettype_policy(&summary);
        assert!(policy.active_at_offset(TextSize::new(50)).is_none());
        assert!(policy.active_at_offset(TextSize::new(99)).is_none());
        assert!(!policy.active_at_offset(TextSize::new(100)).is_none());
    }
}
