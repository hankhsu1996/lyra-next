use lyra_preprocess::DirectiveEventKind;
use lyra_semantic::def_entry::DefScope;
use lyra_semantic::scopes::ScopeId;
use lyra_semantic::time_scale::{
    EffectiveTimeParams, FileTimescaleEntry, FileTimescaleSummary, TimeScaleValue, TimescaleDiag,
    TimescaleDiagKind,
};
use lyra_source::TextSize;

use crate::pipeline::preprocess_file;
use crate::semantic::def_index_file;
use crate::{CompilationUnit, SourceFile};

/// Build a per-file summary of `` `timescale `` directives and `` `resetall ``
/// positions from the preprocessed directive event stream.
///
/// Converts structured `TimescaleDirective` payloads into canonical
/// `TimeScaleValue` pairs. Entries with unparseable semantic values are
/// silently dropped (the preprocessor already diagnosed malformed syntax).
#[salsa::tracked(return_ref)]
pub fn file_timescale_summary(db: &dyn salsa::Database, file: SourceFile) -> FileTimescaleSummary {
    let pp = preprocess_file(db, file);
    let mut entries = Vec::new();
    let mut resetall_offsets = Vec::new();
    let mut diagnostics = Vec::new();

    for event in &pp.directive_events {
        match &event.kind {
            DirectiveEventKind::Timescale(ts) => {
                let unit = TimeScaleValue::parse(&ts.unit_text);
                let precision = TimeScaleValue::parse(&ts.precision_text);

                for (text, parsed) in [(&ts.unit_text, &unit), (&ts.precision_text, &precision)] {
                    if parsed.is_none() {
                        diagnostics.push(TimescaleDiag {
                            span: ts.full_span,
                            kind: TimescaleDiagKind::InvalidValue(text.clone()),
                        });
                    }
                }

                if let (Some(u), Some(p)) = (unit, precision) {
                    if p.rank() > u.rank() {
                        diagnostics.push(TimescaleDiag {
                            span: ts.full_span,
                            kind: TimescaleDiagKind::PrecisionExceedsUnit {
                                unit: u,
                                precision: p,
                            },
                        });
                    }
                    entries.push(FileTimescaleEntry {
                        unit: u,
                        precision: p,
                        expanded_offset: event.expanded_offset,
                        span: ts.full_span,
                    });
                }
            }
            DirectiveEventKind::KnownDirective(lyra_preprocess::DirectiveKeyword::Resetall) => {
                resetall_offsets.push(event.expanded_offset);
            }
            _ => {}
        }
    }

    FileTimescaleSummary {
        entries,
        resetall_offsets,
        diagnostics,
    }
}

/// Compute effective time parameters per design-element scope.
///
/// Encodes LRM 3.14.2.3 precedence:
/// 1. Explicit `timeunit`/`timeprecision` declarations win.
/// 2. Otherwise, the last active `` `timescale `` preceding the design
///    element's declaration position in expanded text applies.
/// 3. `` `resetall `` clears active directive state.
/// 4. Absence of either source leaves the field unset.
#[salsa::tracked(return_ref)]
pub fn effective_time_params(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
) -> Box<[(ScopeId, EffectiveTimeParams)]> {
    let summary = file_timescale_summary(db, file);
    let def = def_index_file(db, file);
    let _ = unit;

    let mut result = Vec::new();

    for def_entry in &*def.def_entries {
        let DefScope::Owned(scope_id) = def_entry.scope else {
            continue;
        };

        let decl_offset = def_entry.decl_site.text_range().start();
        let directive_ts = active_timescale_at(summary, decl_offset);

        let explicit = def.scope_time_units.get(&scope_id);
        let mut params = EffectiveTimeParams::default();

        if let Some(stu) = explicit {
            for decl in &stu.decls {
                match decl {
                    lyra_semantic::time_scale::TimeUnitsDecl::Timeunit {
                        unit, precision, ..
                    } => {
                        if let Some(v) = TimeScaleValue::parse(&unit.raw) {
                            params.unit = Some(v);
                        }
                        if let Some(prec) = precision
                            && let Some(v) = TimeScaleValue::parse(&prec.raw)
                        {
                            params.precision = Some(v);
                        }
                    }
                    lyra_semantic::time_scale::TimeUnitsDecl::Timeprecision {
                        precision, ..
                    } => {
                        if let Some(v) = TimeScaleValue::parse(&precision.raw) {
                            params.precision = Some(v);
                        }
                    }
                }
            }
        }

        if params.unit.is_none()
            && let Some(ts) = &directive_ts
        {
            params.unit = Some(ts.unit);
        }
        if params.precision.is_none()
            && let Some(ts) = &directive_ts
        {
            params.precision = Some(ts.precision);
        }

        result.push((scope_id, params));
    }

    result.into_boxed_slice()
}

/// Find the last active `` `timescale `` preceding `offset`, accounting
/// for `` `resetall `` directives that clear the active state.
fn active_timescale_at(
    summary: &FileTimescaleSummary,
    offset: TextSize,
) -> Option<FileTimescaleEntry> {
    let mut active: Option<&FileTimescaleEntry> = None;

    for entry in &summary.entries {
        if entry.expanded_offset > offset {
            break;
        }
        // Directives at the same or earlier expanded offset as the design
        // element apply: they were stripped from the output but preceded it
        // in source order.
        let reset = summary
            .resetall_offsets
            .iter()
            .any(|&r| r > entry.expanded_offset && r <= offset);
        if reset {
            active = None;
        } else {
            active = Some(entry);
        }
    }

    active.cloned()
}

#[cfg(test)]
mod tests {
    use lyra_semantic::time_scale::{
        FileTimescaleEntry, FileTimescaleSummary, TimeMagnitude, TimeScaleValue, TimeUnitSuffix,
    };
    use lyra_source::{FileId, Span, TextRange, TextSize};

    use super::active_timescale_at;

    fn tsv(mag: TimeMagnitude, unit: TimeUnitSuffix) -> TimeScaleValue {
        TimeScaleValue {
            magnitude: mag,
            unit,
        }
    }

    fn entry(unit: TimeScaleValue, precision: TimeScaleValue, offset: u32) -> FileTimescaleEntry {
        FileTimescaleEntry {
            unit,
            precision,
            expanded_offset: TextSize::new(offset),
            span: Span {
                file: FileId(0),
                range: TextRange::empty(TextSize::new(0)),
            },
        }
    }

    #[test]
    fn no_entries_returns_none() {
        let summary = FileTimescaleSummary::default();
        assert!(active_timescale_at(&summary, TextSize::new(100)).is_none());
    }

    #[test]
    fn single_entry_before_offset() {
        let ns1 = tsv(TimeMagnitude::One, TimeUnitSuffix::Ns);
        let ps1 = tsv(TimeMagnitude::One, TimeUnitSuffix::Ps);
        let summary = FileTimescaleSummary {
            entries: vec![entry(ns1, ps1, 10)],
            resetall_offsets: vec![],
            diagnostics: vec![],
        };
        let result = active_timescale_at(&summary, TextSize::new(50));
        assert_eq!(result.as_ref().map(|e| e.unit), Some(ns1));
    }

    #[test]
    fn entry_after_offset_ignored() {
        let ns1 = tsv(TimeMagnitude::One, TimeUnitSuffix::Ns);
        let ps1 = tsv(TimeMagnitude::One, TimeUnitSuffix::Ps);
        let summary = FileTimescaleSummary {
            entries: vec![entry(ns1, ps1, 100)],
            resetall_offsets: vec![],
            diagnostics: vec![],
        };
        assert!(active_timescale_at(&summary, TextSize::new(50)).is_none());
    }

    #[test]
    fn resetall_clears_active() {
        let ns1 = tsv(TimeMagnitude::One, TimeUnitSuffix::Ns);
        let ps1 = tsv(TimeMagnitude::One, TimeUnitSuffix::Ps);
        let summary = FileTimescaleSummary {
            entries: vec![entry(ns1, ps1, 10)],
            resetall_offsets: vec![TextSize::new(30)],
            diagnostics: vec![],
        };
        assert!(active_timescale_at(&summary, TextSize::new(50)).is_none());
    }

    #[test]
    fn resetall_before_entry_does_not_clear() {
        let ns1 = tsv(TimeMagnitude::One, TimeUnitSuffix::Ns);
        let ps1 = tsv(TimeMagnitude::One, TimeUnitSuffix::Ps);
        let summary = FileTimescaleSummary {
            entries: vec![entry(ns1, ps1, 30)],
            resetall_offsets: vec![TextSize::new(10)],
            diagnostics: vec![],
        };
        let result = active_timescale_at(&summary, TextSize::new(50));
        assert_eq!(result.as_ref().map(|e| e.unit), Some(ns1));
    }

    #[test]
    fn multiple_entries_picks_last() {
        let ns1 = tsv(TimeMagnitude::One, TimeUnitSuffix::Ns);
        let us1 = tsv(TimeMagnitude::One, TimeUnitSuffix::Us);
        let ps1 = tsv(TimeMagnitude::One, TimeUnitSuffix::Ps);
        let summary = FileTimescaleSummary {
            entries: vec![entry(ns1, ps1, 10), entry(us1, ps1, 30)],
            resetall_offsets: vec![],
            diagnostics: vec![],
        };
        let result = active_timescale_at(&summary, TextSize::new(50));
        assert_eq!(result.as_ref().map(|e| e.unit), Some(us1));
    }
}
