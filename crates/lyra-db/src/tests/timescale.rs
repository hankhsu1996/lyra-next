use lyra_semantic::time_scale::{TimeMagnitude, TimeScaleValue, TimeUnitSuffix};

use super::*;

fn tsv(mag: TimeMagnitude, unit: TimeUnitSuffix) -> TimeScaleValue {
    TimeScaleValue {
        magnitude: mag,
        unit,
    }
}

#[test]
fn file_summary_basic() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "`timescale 1ns / 1ps\nmodule m; endmodule\n");
    let summary = crate::timescale_queries::file_timescale_summary(&db, file);
    assert_eq!(summary.entries.len(), 1);
    assert_eq!(
        summary.entries[0].unit,
        tsv(TimeMagnitude::One, TimeUnitSuffix::Ns)
    );
    assert_eq!(
        summary.entries[0].precision,
        tsv(TimeMagnitude::One, TimeUnitSuffix::Ps)
    );
}

#[test]
fn file_summary_multiple() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "`timescale 1ns / 1ps\n`timescale 10us / 100ns\nmodule m; endmodule\n",
    );
    let summary = crate::timescale_queries::file_timescale_summary(&db, file);
    assert_eq!(summary.entries.len(), 2);
    assert_eq!(
        summary.entries[0].unit,
        tsv(TimeMagnitude::One, TimeUnitSuffix::Ns)
    );
    assert_eq!(
        summary.entries[1].unit,
        tsv(TimeMagnitude::Ten, TimeUnitSuffix::Us)
    );
}

#[test]
fn file_summary_malformed_ignored() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "`timescale\nmodule m; endmodule\n");
    let summary = crate::timescale_queries::file_timescale_summary(&db, file);
    assert!(summary.entries.is_empty());
}

#[test]
fn effective_params_from_directive() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "`timescale 1ns / 1ps\nmodule m;\nendmodule\n");
    let unit = single_file_unit(&db, file);
    let params = crate::timescale_queries::effective_time_params(&db, file, unit);
    assert!(!params.is_empty());
    let (_, tp) = &params[0];
    assert_eq!(tp.unit, Some(tsv(TimeMagnitude::One, TimeUnitSuffix::Ns)));
    assert_eq!(
        tp.precision,
        Some(tsv(TimeMagnitude::One, TimeUnitSuffix::Ps))
    );
}

#[test]
fn explicit_timeunit_overrides_directive() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "`timescale 1ns / 1ps\nmodule m;\ntimeunit 10us;\nendmodule\n",
    );
    let unit = single_file_unit(&db, file);
    let params = crate::timescale_queries::effective_time_params(&db, file, unit);
    assert!(!params.is_empty());
    let (_, tp) = &params[0];
    assert_eq!(tp.unit, Some(tsv(TimeMagnitude::Ten, TimeUnitSuffix::Us)));
    assert_eq!(
        tp.precision,
        Some(tsv(TimeMagnitude::One, TimeUnitSuffix::Ps))
    );
}

#[test]
fn precision_exceeds_unit_diagnostic() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "`timescale 1ps / 1ns\nmodule m;\nendmodule\n");
    let unit = single_file_unit(&db, file);
    let diags = file_diagnostics(&db, file, unit);
    assert!(
        diags
            .iter()
            .any(|d| d.code == lyra_diag::code::TIMESCALE_PRECISION_EXCEEDS_UNIT),
        "diags: {diags:?}"
    );
}

#[test]
fn no_diagnostic_when_precision_finer() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "`timescale 1ns / 1ps\nmodule m;\nendmodule\n");
    let unit = single_file_unit(&db, file);
    let diags = file_diagnostics(&db, file, unit);
    assert!(
        !diags
            .iter()
            .any(|d| d.code == lyra_diag::code::TIMESCALE_PRECISION_EXCEEDS_UNIT),
        "diags: {diags:?}"
    );
}
