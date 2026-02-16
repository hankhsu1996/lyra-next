use lyra_ast::ErasedAstId;
use lyra_source::{FileId, TextRange};
use smol_str::SmolStr;

use crate::scopes::ScopeId;

/// Which namespace a symbol or use site belongs to.
///
/// SV has separate namespaces for values (nets, variables, ports,
/// parameters, functions, tasks) and types (typedefs, classes, enum types,
/// struct types). A scope can hold one value and one type with the same
/// identifier without conflict.
///
/// The Definition namespace (IEEE 1800-2023 section 3.13(a)) holds
/// non-nested module, primitive, program, and interface identifiers.
/// These are global within a compilation unit and are NOT resolved
/// through lexical scope chains.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Namespace {
    Value,
    Type,
    Definition,
}

/// Per-file symbol index.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolId(pub(crate) u32);

/// Global identity across files. All public APIs use this.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GlobalSymbolId {
    pub file: FileId,
    pub local: SymbolId,
}

/// Cross-file definition identity.
///
/// Wraps `ErasedAstId` to make the "this is a global definition reference"
/// intent explicit. All cross-file references use this instead of bare
/// `ErasedAstId`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct GlobalDefId(ErasedAstId);

impl GlobalDefId {
    pub fn new(ast_id: ErasedAstId) -> Self {
        Self(ast_id)
    }

    pub fn file(self) -> FileId {
        self.0.file()
    }

    pub fn ast_id(self) -> ErasedAstId {
        self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    Module,
    Package,
    Port,
    Net,
    Variable,
    Parameter,
    Typedef,
}

impl SymbolKind {
    /// The namespace this symbol kind belongs to.
    ///
    /// Modules and packages live in the Definition namespace
    /// (IEEE 1800 section 3.13(a)), resolved via `GlobalDefIndex`,
    /// not lexical scopes.
    pub(crate) fn namespace(self) -> Namespace {
        match self {
            Self::Module | Self::Package => Namespace::Definition,
            Self::Port | Self::Net | Self::Variable | Self::Parameter => Namespace::Value,
            Self::Typedef => Namespace::Type,
        }
    }
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
