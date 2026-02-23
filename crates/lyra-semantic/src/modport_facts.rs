use std::collections::HashMap;

use lyra_ast::ErasedAstId;
use lyra_source::TextRange;

use crate::modport_def::PortDirection;

/// Pre-computed semantic fact for a single field access on a modport-qualified interface.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldAccessFact {
    pub member_range: TextRange,
    pub port_id: ErasedAstId,
    pub direction: PortDirection,
    pub target: FieldAccessTarget,
}

/// What the modport port target is, for access checking.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FieldAccessTarget {
    /// Bare ident: standard direction rules.
    Member,
    /// Expression port: carries expr AST ID for lvalue query.
    Expr(ErasedAstId),
    /// `.P()`: reject all access.
    Empty,
}

/// Map of pre-computed facts keyed by `FieldExpr` AST node identity.
pub type FieldAccessFacts = HashMap<ErasedAstId, FieldAccessFact>;
