use crate::Site;
use smol_str::SmolStr;

/// An owned representation of a single time literal (e.g. `100ps`, `1ns`).
///
/// Stores the canonical raw text as written in source. Future PRs may add
/// parsed numeric value, exponent, or token-level site identity when the
/// architecture supports token-level `Site`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TimeLiteral {
    pub raw: SmolStr,
}

/// A single `timeunit` or `timeprecision` declaration as written in source.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TimeUnitsDecl {
    Timeunit {
        decl_site: Site,
        unit: TimeLiteral,
        precision: Option<TimeLiteral>,
    },
    Timeprecision {
        decl_site: Site,
        precision: TimeLiteral,
    },
}

/// Per-scope collection of time-unit declarations, in source order.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ScopeTimeUnits {
    pub decls: Vec<TimeUnitsDecl>,
}
