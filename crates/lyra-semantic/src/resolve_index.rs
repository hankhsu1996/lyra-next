use std::collections::HashMap;

use lyra_ast::ErasedAstId;
use lyra_source::FileId;

use crate::diagnostic::SemanticDiag;
use crate::symbols::{GlobalSymbolId, Namespace, SymbolId};

/// Offset-independent resolution result from `build_resolve_core`.
///
/// Local resolutions carry a `SymbolId` (per-file); global resolutions
/// carry an `ErasedAstId` (cross-file, topology-stable). The mapping
/// to `GlobalSymbolId` happens in `build_resolve_index`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CoreResolution {
    /// Resolved within the same file's lexical scopes.
    Local {
        symbol: SymbolId,
        namespace: Namespace,
    },
    /// Resolved via the global definitions name space.
    Global {
        decl: ErasedAstId,
        namespace: Namespace,
    },
}

/// A resolved name: the target symbol and the namespace it was found in.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Resolution {
    pub symbol: GlobalSymbolId,
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
