use lyra_ast::ErasedAstId;
use lyra_source::{FileId, TextRange};
use smol_str::SmolStr;

use crate::diagnostic::SemanticDiag;
use crate::scopes::{ScopeId, ScopeTree};
use crate::symbols::{SymbolId, SymbolTable};

/// Per-file definition index: symbols, scopes, exports.
///
/// Depends only on the parse result. Does NOT resolve uses.
/// All ranges are in expanded-text coordinate space within `file`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DefIndex {
    pub file: FileId,
    pub symbols: SymbolTable,
    pub scopes: ScopeTree,
    pub exports: Exports,
    pub use_sites: Box<[UseSite]>,
    pub diagnostics: Box<[SemanticDiag]>,
}

/// A recorded name-use site awaiting resolution.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UseSite {
    pub name: SmolStr,
    pub range: TextRange,
    pub scope: ScopeId,
    pub ast_id: ErasedAstId,
}

/// Module/package names exported to compilation-unit namespace.
///
/// Separate from lexical `ScopeTree`. Entries sorted by
/// `symbols[id].name` for binary-search lookup.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Exports {
    pub modules: Box<[SymbolId]>,
}
