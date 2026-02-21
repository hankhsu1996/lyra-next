use std::collections::HashMap;

use lyra_ast::ErasedAstId;
use lyra_source::{FileId, TextRange};
use smol_str::SmolStr;

use crate::diagnostic::SemanticDiag;
use crate::enum_def::{EnumDef, EnumDefIdx, EnumId, EnumVariantId};
use crate::record::{ModportDef, ModportDefId, RecordDef, RecordDefIdx, RecordId};
use crate::scopes::{ScopeId, ScopeTree};
use crate::symbols::{GlobalDefId, Namespace, SymbolId, SymbolTable};

/// Lookup strategy for a use-site.
///
/// Determines which namespace(s) to search and in what order.
/// Lives on use-sites and resolution requests only -- never on
/// symbol storage or scope bindings (those stay `Namespace`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ExpectedNs {
    /// Look up in exactly one namespace.
    Exact(Namespace),
    /// Type position: try Type first, fall back to Value.
    /// Used for user-defined type names in `TypeSpec`.
    TypeThenValue,
}

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
    pub imports: Box<[Import]>,
    pub local_decls: Box<[LocalDecl]>,
    /// Reverse map from exported declaration `ErasedAstId` to `SymbolId`.
    /// Used by cross-file resolution to convert `ErasedAstId` (from
    /// `GlobalDefIndex`) back to a `SymbolId` in this file.
    pub decl_to_symbol: HashMap<ErasedAstId, SymbolId>,
    /// Dense reverse map from `SymbolId` index to its `Declarator` `ErasedAstId`.
    /// `None` for symbols without a `Declarator` node (e.g. module/package names).
    pub symbol_to_decl: Box<[Option<ErasedAstId>]>,
    /// Maps parameter `Declarator` `ErasedAstId` to its initializer `Expression`
    /// `ErasedAstId`. Key present with `None` = parameter with no default value.
    /// Key absent = not a parameter declarator (not tracked).
    pub decl_to_init_expr: HashMap<ErasedAstId, Option<ErasedAstId>>,
    pub enum_defs: Box<[EnumDef]>,
    pub record_defs: Box<[RecordDef]>,
    pub modport_defs: HashMap<ModportDefId, ModportDef>,
    pub modport_name_map: HashMap<(crate::record::InterfaceDefId, SmolStr), ModportDefId>,
    pub export_decls: Box<[ExportDecl]>,
    pub diagnostics: Box<[SemanticDiag]>,
}

impl DefIndex {
    pub fn enum_def(&self, idx: EnumDefIdx) -> &EnumDef {
        &self.enum_defs[idx.0 as usize]
    }

    pub fn record_def(&self, idx: RecordDefIdx) -> &RecordDef {
        &self.record_defs[idx.0 as usize]
    }

    pub fn enum_id(&self, idx: EnumDefIdx) -> EnumId {
        let def = self.enum_def(idx);
        EnumId {
            file: self.file,
            owner: def.owner.clone(),
            ordinal: def.ordinal,
        }
    }

    pub fn enum_variant_id(&self, enum_idx: EnumDefIdx, variant_ordinal: u32) -> EnumVariantId {
        EnumVariantId {
            enum_id: self.enum_id(enum_idx),
            variant_ordinal,
        }
    }

    pub fn modport_by_name(
        &self,
        iface: crate::record::InterfaceDefId,
        name: &str,
    ) -> Option<&ModportDef> {
        let id = self.modport_name_map.get(&(iface, SmolStr::new(name)))?;
        self.modport_defs.get(id)
    }

    /// Map a local symbol to its cross-file definition identity.
    pub fn symbol_global_def(&self, sym: SymbolId) -> Option<GlobalDefId> {
        debug_assert!(sym.index() < self.symbol_to_decl.len());
        let ast_id = self.symbol_to_decl.get(sym.index()).and_then(|o| *o)?;
        Some(GlobalDefId::new(ast_id))
    }

    pub fn record_id(&self, idx: RecordDefIdx) -> RecordId {
        let def = self.record_def(idx);
        RecordId {
            file: self.file,
            owner: def.owner.clone(),
            ordinal: def.ordinal,
        }
    }
}

/// A name path: simple identifier or qualified path.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NamePath {
    /// Simple identifier: `foo`
    Simple(SmolStr),
    /// Qualified path: `a::b::c`. Segments are the identifiers.
    Qualified { segments: Box<[SmolStr]> },
}

impl NamePath {
    pub fn as_simple(&self) -> Option<&str> {
        match self {
            Self::Simple(s) => Some(s.as_str()),
            Self::Qualified { .. } => None,
        }
    }

    pub fn as_qualified(&self) -> Option<&[SmolStr]> {
        match self {
            Self::Simple(_) => None,
            Self::Qualified { segments } => Some(segments),
        }
    }

    pub fn display_name(&self) -> String {
        match self {
            Self::Simple(s) => s.to_string(),
            Self::Qualified { segments } => segments.join("::"),
        }
    }
}

/// A recorded name-use site awaiting resolution.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UseSite {
    pub path: NamePath,
    pub expected_ns: ExpectedNs,
    pub range: TextRange,
    pub scope: ScopeId,
    pub ast_id: ErasedAstId,
    /// File-local monotonic rank from preorder syntax traversal.
    /// Used by the resolver for LRM 26.3 positional visibility.
    pub order_key: u32,
}

/// A local declaration that may conflict with wildcard imports (LRM 26.3).
///
/// Value- and type-namespace declarations only. Definition-namespace
/// items (module, package, interface, etc.) are excluded.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LocalDecl {
    pub id: LocalDeclId,
    pub symbol_id: SymbolId,
    pub name: SmolStr,
    pub namespace: Namespace,
    pub ast_id: ErasedAstId,
    /// File-local monotonic rank from preorder syntax traversal.
    pub order_key: u32,
    pub range: TextRange,
}

/// An import declaration record.
///
/// Carries no namespace: the namespace is determined at the use-site
/// when the import is consulted during resolution. This matches LRM
/// semantics where `import pkg::x` makes `x` visible for whatever
/// namespace the use-site needs.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Import {
    pub id: ImportDeclId,
    pub package: SmolStr,
    pub name: ImportName,
    pub scope: ScopeId,
    pub range: TextRange,
    pub ast_id: ErasedAstId,
    /// File-local monotonic rank from preorder syntax traversal.
    /// Used by the resolver for LRM 26.3 positional visibility.
    pub order_key: u32,
}

/// Whether an import is explicit (`pkg::sym`) or wildcard (`pkg::*`).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ImportName {
    Explicit(SmolStr),
    Wildcard,
}

/// File-local identity for a local declaration that may conflict with imports.
/// Scoped to a single `DefIndex`/`NameGraph` (same as `ScopeId`, `SymbolId`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct LocalDeclId {
    pub scope: ScopeId,
    pub ordinal: u32,
}

/// File-local identity for an import declaration.
/// Scoped to a single `DefIndex`/`NameGraph` (same as `ScopeId`, `SymbolId`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ImportDeclId {
    pub scope: ScopeId,
    pub ordinal: u32,
}

/// File-local identity for an export declaration.
/// Scoped to a single `DefIndex`/`NameGraph` (same as `ScopeId`, `SymbolId`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ExportDeclId {
    pub scope: ScopeId,
    pub ordinal: u32,
}

/// What an export declaration names (LRM 26.6).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExportKey {
    /// `export Pkg::name;`
    Explicit { package: SmolStr, name: SmolStr },
    /// `export Pkg::*;`
    PackageWildcard { package: SmolStr },
    /// `export *::*;`
    AllWildcard,
}

/// A single export declaration with identity and source range.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExportDecl {
    pub id: ExportDeclId,
    pub key: ExportKey,
    pub range: TextRange,
}

/// Compilation-unit-level environment: implicit imports visible in all files.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompilationUnitEnv {
    pub implicit_imports: Box<[ImplicitImport]>,
}

/// An implicit import added at the compilation-unit level.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImplicitImport {
    pub package: SmolStr,
    pub name: ImportName,
}

/// Definition-namespace and package names exported to compilation-unit namespace.
///
/// Separate from lexical `ScopeTree`. Entries sorted by
/// `symbols[id].name` for binary-search lookup.
///
/// `definitions` holds all definition-namespace constructs (module, interface,
/// program, primitive, config) per LRM 3.13(a). Packages stay separate since
/// they are in a distinct namespace (LRM 3.13(b)) and are used differently
/// (package scope index).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Exports {
    pub definitions: Box<[SymbolId]>,
    pub packages: Box<[SymbolId]>,
}
