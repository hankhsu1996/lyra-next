use std::collections::HashMap;

use crate::Site;
use lyra_source::TokenSpan;

use crate::modport_def::PortDirection;

/// Pre-computed semantic fact for a single field access on a modport-qualified interface.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldAccessFact {
    pub member_name_span: TokenSpan,
    pub port_id: Site,
    pub direction: PortDirection,
    pub target: FieldAccessTarget,
}

/// What the modport port target is, for access checking.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FieldAccessTarget {
    /// Bare ident: standard direction rules.
    Member,
    /// Expression port: carries expr AST ID for lvalue query.
    Expr(Site),
    /// `.P()`: reject all access.
    Empty,
}

/// Map of pre-computed facts keyed by `FieldExpr` AST node identity.
/// Lookup-only: keyed point lookup by `Site`; do not iterate for
/// observable output.
pub type FieldAccessFacts = HashMap<Site, FieldAccessFact>;
