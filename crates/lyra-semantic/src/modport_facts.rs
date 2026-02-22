use std::collections::HashMap;

use lyra_ast::ErasedAstId;
use lyra_source::TextRange;

use crate::modport_def::PortDirection;

/// Pre-computed semantic fact for a single field access on a modport-qualified interface.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldAccessFact {
    pub member_range: TextRange,
    pub direction: PortDirection,
}

/// Map of pre-computed facts keyed by `FieldExpr` AST node identity.
pub type FieldAccessFacts = HashMap<ErasedAstId, FieldAccessFact>;
