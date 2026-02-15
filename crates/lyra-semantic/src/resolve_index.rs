use std::collections::HashMap;

use lyra_ast::ErasedAstId;
use lyra_source::FileId;

use crate::diagnostic::SemanticDiag;
use crate::symbols::{Namespace, SymbolId};

/// A resolved name: the target symbol and the namespace it was found in.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Resolution {
    pub symbol: SymbolId,
    pub namespace: Namespace,
}

/// Per-file resolution results. Depends on `DefIndex`.
///
/// Maps use-site `ErasedAstId` to resolved `Resolution`.
/// Uses `HashMap` because the access pattern is point lookup
/// by `AstId` (from cursor queries).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolveIndex {
    pub file: FileId,
    pub resolutions: HashMap<ErasedAstId, Resolution>,
    pub diagnostics: Box<[SemanticDiag]>,
}
