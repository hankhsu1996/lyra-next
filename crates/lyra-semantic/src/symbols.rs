use lyra_ast::ErasedAstId;
use lyra_source::{FileId, TextRange};
use smol_str::SmolStr;

use crate::record::SymbolOrigin;
use crate::scopes::ScopeId;

/// Bitmask for namespace overlap checking.
///
/// Used by import conflict detection (LRM 26.5) to track which
/// namespaces overlap between an import and a local declaration
/// or another import.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NsMask(u8);

impl NsMask {
    pub const EMPTY: Self = Self(0);
    pub const VALUE: Self = Self(1);
    pub const TYPE: Self = Self(2);

    #[must_use]
    pub fn union(self, other: Self) -> Self {
        Self(self.0 | other.0)
    }

    #[must_use]
    pub fn intersect(self, other: Self) -> Self {
        Self(self.0 & other.0)
    }

    pub fn intersects(self, other: Self) -> bool {
        self.0 & other.0 != 0
    }

    pub fn is_empty(self) -> bool {
        self.0 == 0
    }

    pub fn from_namespace(ns: Namespace) -> Self {
        match ns {
            Namespace::Value => Self::VALUE,
            Namespace::Type => Self::TYPE,
            Namespace::Definition => Self::EMPTY,
        }
    }
}

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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Namespace {
    Value,
    Type,
    Definition,
}

/// Per-file symbol index.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SymbolId(pub(crate) u32);

impl SymbolId {
    /// Raw index for array-based lookups.
    pub fn index(self) -> usize {
        self.0 as usize
    }
}

/// Global identity across files. All public APIs use this.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GlobalSymbolId {
    pub file: FileId,
    pub local: SymbolId,
}

/// Cross-file definition identity.
///
/// Cross-file identity for definition-namespace constructs (module, package,
/// interface, program, primitive, config). Wraps `ErasedAstId` with
/// `def_ast` semantics. `symbol_global_def()` enforces the restriction.
/// `PackageScope` uses `ErasedAstId` directly for value/type-namespace
/// member anchors.
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
    /// ANSI port from module/interface/program `PortList`.
    PortAnsi,
    /// Task/function port from `TfPortDecl`.
    PortTf,
    Net,
    Variable,
    Parameter,
    Typedef,
    Interface,
    Program,
    Primitive,
    Config,
    Function,
    Task,
    /// Enum variant injected into the enclosing scope as a value-namespace
    /// constant with the parent enum's type.
    EnumMember,
    /// Navigation/diagnostics only. Not added to scope bindings; resolved
    /// exclusively via `DefIndex.modport_name_map` when the LHS is an
    /// interface type.
    Modport,
    /// Module or interface instance (e.g., `my_bus sb();`). Not added to
    /// lexical scope bindings; resolved via `DefIndex.instance_decls`
    /// fallback when the name is unresolved in value context.
    Instance,
}

impl SymbolKind {
    /// The namespace this symbol kind belongs to.
    ///
    /// Modules and packages live in the Definition namespace
    /// (IEEE 1800 section 3.13(a)), resolved via `GlobalDefIndex`,
    /// not lexical scopes.
    pub(crate) fn namespace(self) -> Namespace {
        match self {
            Self::Module
            | Self::Package
            | Self::Interface
            | Self::Program
            | Self::Primitive
            | Self::Config => Namespace::Definition,
            Self::PortAnsi
            | Self::PortTf
            | Self::Net
            | Self::Variable
            | Self::Parameter
            | Self::Function
            | Self::Task
            | Self::EnumMember
            | Self::Modport
            | Self::Instance => Namespace::Value,
            Self::Typedef => Namespace::Type,
        }
    }
}

/// A resolved symbol entry.
///
/// `def_ast` anchors this symbol to the declaration item node that
/// introduces it (e.g. `VarDecl` for variables, `ModuleDecl` for modules).
/// Multiple symbols may share the same `def_ast` when declared in the same
/// declaration item (e.g. `logic x, y;`).
///
/// `name_ast` is the unique name-introducing site (1:1 with the symbol).
/// For multi-declarator items this is the `Declarator` node; for single-name
/// declarations it equals `def_ast`.
///
/// `type_ast` is the `TypeSpec` node spelling the type, when present.
///
/// `def_range` is in expanded-text coordinate space within the owning
/// file. The `FileId` lives on `DefIndex`, not duplicated here.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Symbol {
    pub name: SmolStr,
    pub kind: SymbolKind,
    pub def_ast: ErasedAstId,
    pub name_ast: ErasedAstId,
    pub type_ast: Option<ErasedAstId>,
    pub def_range: TextRange,
    pub scope: ScopeId,
    pub origin: SymbolOrigin,
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

    pub(crate) fn get(&self, id: SymbolId) -> &Symbol {
        &self.symbols[id.0 as usize]
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
