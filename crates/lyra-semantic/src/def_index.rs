use lyra_ast::ErasedAstId;
use lyra_source::{FileId, TextRange};
use smol_str::SmolStr;

use crate::diagnostic::SemanticDiag;
use crate::scopes::{ScopeId, ScopeTree};
use crate::symbols::{Namespace, SymbolId, SymbolTable};

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

/// A name path: simple identifier or (future) qualified/hierarchical path.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NamePath {
    /// Simple identifier: `foo`
    Simple(SmolStr),
    // Future variants:
    // Qualified { package: SmolStr, name: SmolStr },  // pkg::sym
    // Hierarchical(Box<[SmolStr]>),                   // a.b.c
}

impl NamePath {
    pub fn as_simple(&self) -> Option<&str> {
        match self {
            Self::Simple(s) => Some(s.as_str()),
        }
    }
}

/// A recorded name-use site awaiting resolution.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UseSite {
    pub path: NamePath,
    pub expected_ns: Namespace,
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
