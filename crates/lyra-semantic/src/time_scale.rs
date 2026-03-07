use std::fmt;

use lyra_source::{Span, TextSize};
use smol_str::SmolStr;

use crate::Site;

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

/// The numeric magnitude prefix of a time value (`1`, `10`, `100`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TimeMagnitude {
    One,
    Ten,
    Hundred,
}

impl fmt::Display for TimeMagnitude {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::One => f.write_str("1"),
            Self::Ten => f.write_str("10"),
            Self::Hundred => f.write_str("100"),
        }
    }
}

/// The unit suffix of a time value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TimeUnitSuffix {
    S,
    Ms,
    Us,
    Ns,
    Ps,
    Fs,
}

impl fmt::Display for TimeUnitSuffix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::S => f.write_str("s"),
            Self::Ms => f.write_str("ms"),
            Self::Us => f.write_str("us"),
            Self::Ns => f.write_str("ns"),
            Self::Ps => f.write_str("ps"),
            Self::Fs => f.write_str("fs"),
        }
    }
}

/// A canonical parsed time-scale value (e.g. `1ns`, `100ps`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TimeScaleValue {
    pub magnitude: TimeMagnitude,
    pub unit: TimeUnitSuffix,
}

impl TimeScaleValue {
    /// Parse a time-scale value from its textual form.
    pub fn parse(s: &str) -> Option<Self> {
        let (magnitude, rest) = if let Some(rest) = s.strip_prefix("100") {
            (TimeMagnitude::Hundred, rest)
        } else if let Some(rest) = s.strip_prefix("10") {
            (TimeMagnitude::Ten, rest)
        } else if let Some(rest) = s.strip_prefix('1') {
            (TimeMagnitude::One, rest)
        } else {
            return None;
        };

        let unit = match rest {
            "s" => TimeUnitSuffix::S,
            "ms" => TimeUnitSuffix::Ms,
            "us" => TimeUnitSuffix::Us,
            "ns" => TimeUnitSuffix::Ns,
            "ps" => TimeUnitSuffix::Ps,
            "fs" => TimeUnitSuffix::Fs,
            _ => return None,
        };

        Some(Self { magnitude, unit })
    }

    /// Numeric rank for ordering and comparison. Smaller rank = finer
    /// precision. Values range from 0 (`1fs`) to 17 (`100s`).
    pub fn rank(self) -> i8 {
        let unit_rank: i8 = match self.unit {
            TimeUnitSuffix::Fs => 0,
            TimeUnitSuffix::Ps => 3,
            TimeUnitSuffix::Ns => 6,
            TimeUnitSuffix::Us => 9,
            TimeUnitSuffix::Ms => 12,
            TimeUnitSuffix::S => 15,
        };
        let mag_rank: i8 = match self.magnitude {
            TimeMagnitude::One => 0,
            TimeMagnitude::Ten => 1,
            TimeMagnitude::Hundred => 2,
        };
        unit_rank + mag_rank
    }
}

impl fmt::Display for TimeScaleValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.magnitude, self.unit)
    }
}

/// A single `` `timescale `` entry from a file's directive stream.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileTimescaleEntry {
    pub unit: TimeScaleValue,
    pub precision: TimeScaleValue,
    pub expanded_offset: TextSize,
    pub span: Span,
}

/// A diagnostic produced during canonical timescale lowering.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TimescaleDiagKind {
    /// A time value token was not a valid `TimeScaleValue`.
    InvalidValue(SmolStr),
    /// Precision is finer than unit (LRM 3.14.2.1).
    PrecisionExceedsUnit {
        unit: TimeScaleValue,
        precision: TimeScaleValue,
    },
}

/// A diagnostic produced during canonical timescale lowering.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TimescaleDiag {
    pub span: Span,
    pub kind: TimescaleDiagKind,
}

/// Per-file summary of `` `timescale `` directives and `` `resetall `` positions.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct FileTimescaleSummary {
    pub entries: Vec<FileTimescaleEntry>,
    pub resetall_offsets: Vec<TextSize>,
    pub diagnostics: Vec<TimescaleDiag>,
}

/// The effective time parameters for a scope, derived from the precedence
/// rules in LRM 3.14.2.3.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct EffectiveTimeParams {
    pub unit: Option<TimeScaleValue>,
    pub precision: Option<TimeScaleValue>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_valid_values() {
        let cases = [
            ("1s", TimeMagnitude::One, TimeUnitSuffix::S),
            ("10ms", TimeMagnitude::Ten, TimeUnitSuffix::Ms),
            ("100us", TimeMagnitude::Hundred, TimeUnitSuffix::Us),
            ("1ns", TimeMagnitude::One, TimeUnitSuffix::Ns),
            ("10ps", TimeMagnitude::Ten, TimeUnitSuffix::Ps),
            ("100fs", TimeMagnitude::Hundred, TimeUnitSuffix::Fs),
        ];
        for (text, mag, unit) in cases {
            let v = TimeScaleValue::parse(text);
            assert_eq!(
                v,
                Some(TimeScaleValue {
                    magnitude: mag,
                    unit,
                }),
                "failed to parse: {text}"
            );
        }
    }

    #[test]
    fn parse_invalid_values() {
        assert_eq!(TimeScaleValue::parse("2ns"), None);
        assert_eq!(TimeScaleValue::parse("1000ns"), None);
        assert_eq!(TimeScaleValue::parse("1xs"), None);
        assert_eq!(TimeScaleValue::parse(""), None);
        assert_eq!(TimeScaleValue::parse("ns"), None);
    }

    #[test]
    fn rank_ordering() {
        let fs1 = TimeScaleValue::parse("1fs").map(|v| v.rank());
        let ps1 = TimeScaleValue::parse("1ps").map(|v| v.rank());
        let ns1 = TimeScaleValue::parse("1ns").map(|v| v.rank());
        let ns10 = TimeScaleValue::parse("10ns").map(|v| v.rank());
        let s100 = TimeScaleValue::parse("100s").map(|v| v.rank());
        assert!(fs1 < ps1);
        assert!(ps1 < ns1);
        assert!(ns1 < ns10);
        assert!(ns10 < s100);
    }

    #[test]
    fn display_roundtrip() {
        for text in &["1ns", "10ps", "100us", "1s", "10ms", "100fs"] {
            let v = TimeScaleValue::parse(text).unwrap_or_else(|| panic!("parse {text}"));
            assert_eq!(&v.to_string(), *text);
        }
    }
}
