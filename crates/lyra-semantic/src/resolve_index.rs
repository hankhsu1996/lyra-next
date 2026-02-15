use std::collections::HashMap;

use lyra_ast::ErasedAstId;
use lyra_source::FileId;

use crate::diagnostic::SemanticDiag;
use crate::symbols::SymbolId;

/// Per-file resolution results. Depends on `DefIndex`.
///
/// Maps use-site `ErasedAstId` to resolved `SymbolId`.
/// Uses `HashMap` because the access pattern is point lookup
/// by `AstId` (from cursor queries).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolveIndex {
    pub file: FileId,
    pub resolutions: HashMap<ErasedAstId, SymbolId>,
    pub diagnostics: Box<[SemanticDiag]>,
}
