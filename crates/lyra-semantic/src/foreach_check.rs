// Foreach loop legality check data types (LRM 12.7.3).
//
// Plain data types and pure helper functions. No Salsa annotations,
// no db access. The orchestrator in `lyra-db` produces these items.

use lyra_source::TokenSpan;

/// A single foreach-legality finding.
///
/// Each item anchors a specific violation by `TokenSpan` (keyword or
/// name token range) so lowering is a trivial span mapping. Token text
/// is recovered from the expanded source at lowering time.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ForeachCheckItem {
    /// Assignment target resolves to a foreach loop variable (LRM 12.7.3).
    AssignToForeachVar { lhs_name_span: TokenSpan },
    /// Loop variable shares name with iterated array root name.
    VarSameNameAsArray { var_name_span: TokenSpan },
    /// More loop variables than iterable dimensions.
    TooManyVars {
        excess_var_span: TokenSpan,
        dim_count: u32,
        var_count: u32,
    },
    /// Foreach loop iterates a wildcard associative array (LRM 7.8.1).
    WildcardAssocArray { foreach_kw_span: TokenSpan },
}

/// Per-file foreach-legality product.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForeachCheckIndex {
    pub items: Box<[ForeachCheckItem]>,
}
