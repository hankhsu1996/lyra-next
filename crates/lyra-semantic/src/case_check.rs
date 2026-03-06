// Case statement legality check data types (LRM 12.5.4).
//
// Plain data types. No Salsa annotations, no db access.
// The orchestrator in `lyra-db` produces these items.

use lyra_ast::CaseKind;
use lyra_source::TokenSpan;

/// A single case-legality finding.
///
/// Each item anchors the violation by `TokenSpan` (the case keyword)
/// so lowering is a trivial span mapping.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CaseCheckItem {
    /// `inside` used with `casex` or `casez` (LRM 12.5.4).
    IllegalInsideCaseKind { kind: CaseKind, kw_span: TokenSpan },
}

/// Per-file case-legality product.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CaseCheckIndex {
    pub items: Box<[CaseCheckItem]>,
}
