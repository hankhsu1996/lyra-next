use smol_str::SmolStr;

use crate::def_index::{DefIndex, NamePath};
use crate::scopes::{ScopeId, ScopeTree, SymbolNameLookup};
use crate::symbols::{Namespace, SymbolId, SymbolKind};

/// Offset-independent projection of `DefIndex` for resolution.
///
/// Contains only the facts needed to resolve names: symbol names/kinds,
/// scope tree, and use-site semantic keys. No `TextRange`, no `ErasedAstId`.
/// `PartialEq` compares equal across whitespace-only edits, enabling Salsa
/// to backdate and skip re-running expensive resolution logic downstream.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NameGraph {
    pub(crate) symbol_names: Box<[SmolStr]>,
    pub(crate) symbol_kinds: Box<[SymbolKind]>,
    pub(crate) scopes: ScopeTree,
    pub(crate) use_entries: Box<[UseEntry]>,
}

/// Offset-independent use-site key.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct UseEntry {
    pub(crate) path: NamePath,
    pub(crate) expected_ns: Namespace,
    pub(crate) scope: ScopeId,
}

impl SymbolNameLookup for NameGraph {
    fn name(&self, id: SymbolId) -> &str {
        self.symbol_names[id.0 as usize].as_str()
    }
}

impl NameGraph {
    /// Project offset-independent facts from a `DefIndex`.
    pub fn from_def_index(def: &DefIndex) -> Self {
        let symbol_names: Box<[SmolStr]> = def
            .symbols
            .iter()
            .map(|(_, sym)| sym.name.clone())
            .collect();

        let symbol_kinds: Box<[SymbolKind]> = def.symbols.iter().map(|(_, sym)| sym.kind).collect();

        let use_entries: Box<[UseEntry]> = def
            .use_sites
            .iter()
            .map(|site| UseEntry {
                path: site.path.clone(),
                expected_ns: site.expected_ns,
                scope: site.scope,
            })
            .collect();

        Self {
            symbol_names,
            symbol_kinds,
            scopes: def.scopes.clone(),
            use_entries,
        }
    }
}
