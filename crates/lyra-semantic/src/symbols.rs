use lyra_source::{FileId, TextRange};
use smol_str::SmolStr;

use crate::scopes::ScopeId;

/// Per-file symbol index.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolId(pub(crate) u32);

/// Global identity across files. All public APIs use this.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GlobalSymbolId {
    pub file: FileId,
    pub local: SymbolId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    Module,
    Port,
    Net,
    Variable,
    Parameter,
}

/// A resolved symbol entry.
///
/// `def_range` is in expanded-text coordinate space within the owning
/// file. The `FileId` lives on `DefIndex`, not duplicated here.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Symbol {
    pub name: SmolStr,
    pub kind: SymbolKind,
    pub def_range: TextRange,
    pub scope: ScopeId,
}

/// Per-file symbol store, indexed by `SymbolId`.
///
/// Frozen after construction -- `Box<[Symbol]>` for cheap clone.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SymbolTable {
    symbols: Box<[Symbol]>,
}

/// Builder for accumulating symbols before freezing.
pub(crate) struct SymbolTableBuilder {
    symbols: Vec<Symbol>,
}

impl SymbolTableBuilder {
    pub(crate) fn new() -> Self {
        Self {
            symbols: Vec::new(),
        }
    }

    pub(crate) fn push(&mut self, sym: Symbol) -> SymbolId {
        let id = SymbolId(self.symbols.len() as u32);
        self.symbols.push(sym);
        id
    }

    pub(crate) fn freeze(self) -> SymbolTable {
        SymbolTable {
            symbols: self.symbols.into_boxed_slice(),
        }
    }
}

impl SymbolTable {
    pub fn get(&self, id: SymbolId) -> &Symbol {
        &self.symbols[id.0 as usize]
    }

    pub fn len(&self) -> usize {
        self.symbols.len()
    }

    pub fn is_empty(&self) -> bool {
        self.symbols.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = (SymbolId, &Symbol)> {
        self.symbols
            .iter()
            .enumerate()
            .map(|(i, s)| (SymbolId(i as u32), s))
    }
}
