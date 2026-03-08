use std::collections::HashMap;

use crate::Site;
use lyra_source::{FileId, NameSpan};
use smol_str::SmolStr;

use crate::def_entry::{DefEntry, DefScope};
use crate::diagnostic::SemanticDiag;
use crate::enum_def::{EnumDef, EnumDefIdx, EnumId};
use crate::global_index::DefinitionKind;
use crate::instance_decl::{InstanceDecl, InstanceDeclIdx};
use crate::interface_id::InterfaceDefId;
use crate::modport_def::{ModportDef, ModportDefId};
use crate::nettype_def::{NettypeDef, NettypeDefIdx};
use crate::record::{RecordDef, RecordDefIdx, RecordId};
use crate::scopes::{ScopeId, ScopeKind, ScopeTree};
#[cfg(test)]
use crate::symbols::SymbolKind;
use crate::symbols::{GlobalDefId, Namespace, SymbolId, SymbolTable};
use crate::time_scale::ScopeTimeUnits;

/// Semantic role of a scope-owning declaration.
///
/// Used for role-qualified scope lookup in test helpers. Covers
/// both definition-namespace owners (module, package, ...) and
/// symbol-namespace owners (function, task).
#[cfg(test)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum ScopeOwnerKind {
    Module,
    Package,
    Interface,
    Program,
    Primitive,
    Config,
    Function,
    Task,
}

#[cfg(test)]
impl ScopeOwnerKind {
    pub(crate) fn from_definition_kind(dk: DefinitionKind) -> Self {
        match dk {
            DefinitionKind::Module => Self::Module,
            DefinitionKind::Package => Self::Package,
            DefinitionKind::Interface => Self::Interface,
            DefinitionKind::Program => Self::Program,
            DefinitionKind::Primitive => Self::Primitive,
            DefinitionKind::Config => Self::Config,
        }
    }

    pub(crate) fn from_symbol_kind(sk: SymbolKind) -> Option<Self> {
        match sk {
            SymbolKind::Function => Some(Self::Function),
            SymbolKind::Task => Some(Self::Task),
            _ => None,
        }
    }
}

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
    /// Type operator `type(name)`: try Type first, fall back to Value.
    /// Unlike `TypeThenValue`, resolving to Value does not emit a
    /// "not a type" diagnostic (LRM 6.23).
    TypeOrValue,
}

/// Per-file definition index: symbols, scopes, definition entries.
///
/// Depends only on the parse result. Does NOT resolve uses.
/// All ranges are in expanded-text coordinate space within `file`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DefIndex {
    pub file: FileId,
    pub symbols: SymbolTable,
    pub scopes: ScopeTree,
    /// Definition-namespace entries keyed by insertion order.
    /// Use `def_entry()` to look up by `GlobalDefId`.
    pub def_entries: Box<[DefEntry]>,
    /// Lookup-only: reverse map from definition name-site to `GlobalDefId`.
    /// Keyed point lookup; do not iterate.
    pub name_site_to_def: HashMap<Site, GlobalDefId>,
    /// All `GlobalDefId`s in this file, sorted by `(name, kind, name_site)` for
    /// deterministic iteration. Used by `global_def_index` and `package_scope_index`.
    pub defs_by_name: Box<[GlobalDefId]>,
    pub use_sites: Box<[UseSite]>,
    pub imports: Box<[Import]>,
    pub local_decls: Box<[LocalDecl]>,
    /// Lookup-only: reverse map from name-site `Site` to `SymbolId`.
    /// Used by cross-file resolution to convert `Site` (from
    /// `PackageScopeIndex`) back to a `SymbolId` in this file.
    /// Keyed point lookup; do not iterate.
    pub name_site_to_symbol: HashMap<Site, SymbolId>,
    /// Lookup-only: maps parameter name-site `Site` to its initializer
    /// `Expression` `Site`. Key present with `None` = parameter with no
    /// default value. Key absent = not a parameter (not tracked).
    /// Keyed point lookup; do not iterate.
    pub name_site_to_init_expr: HashMap<Site, Option<Site>>,
    pub enum_defs: Box<[EnumDef]>,
    /// Lookup-only: keyed point lookup; do not iterate.
    pub enum_by_site: HashMap<Site, EnumDefIdx>,
    pub record_defs: Box<[RecordDef]>,
    /// Lookup-only: keyed point lookup; do not iterate.
    pub record_by_site: HashMap<Site, RecordDefIdx>,
    pub instance_decls: Box<[InstanceDecl]>,
    pub nettype_defs: Box<[NettypeDef]>,
    /// Modport definitions: ordered storage + keyed lookup.
    /// Access through `modport_def()`, `modport_defs_in_order()`,
    /// `modport_by_name()`, or `modport_name_to_id()`.
    pub(crate) modports: ModportStorage,
    pub export_decls: Box<[ExportDecl]>,
    /// Lookup-only: keyed point lookup by `SymbolId`; do not iterate.
    pub foreach_var_defs: HashMap<SymbolId, ForeachVarDef>,
    /// Lookup-only: keyed point lookup by `ScopeId`; do not iterate.
    pub scope_time_units: HashMap<ScopeId, ScopeTimeUnits>,
    /// Maps a scope to the `decl_site` of the declaration that owns it.
    /// Present for named scopes (modules, packages, callables); absent
    /// for anonymous scopes (blocks, generates, file).
    pub scope_owners: HashMap<ScopeId, Site>,
    /// Reverse index: maps an owner `decl_site` to the `ScopeId` it owns.
    /// Populated atomically with `scope_owners` through
    /// `DefContext::register_scope_owner`.
    pub owner_to_scope: HashMap<Site, ScopeId>,
    pub diagnostics: Box<[SemanticDiag]>,
    pub internal_errors: Box<[(Option<Site>, SmolStr)]>,
}

/// Encapsulates modport storage: declaration-order slice for traversal,
/// position index for O(1) keyed lookup, name map for name-based lookup.
///
/// Storage shape is private. Consumers use `DefIndex` accessors.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModportStorage {
    defs_ordered: Box<[ModportDef]>,
    index: HashMap<ModportDefId, u32>,
    name_map: HashMap<(InterfaceDefId, SmolStr), ModportDefId>,
}

impl ModportStorage {
    pub(crate) fn empty() -> Self {
        Self {
            defs_ordered: Box::new([]),
            index: HashMap::new(),
            name_map: HashMap::new(),
        }
    }

    pub(crate) fn new(
        defs_ordered: Box<[ModportDef]>,
        index: HashMap<ModportDefId, u32>,
        name_map: HashMap<(InterfaceDefId, SmolStr), ModportDefId>,
    ) -> Self {
        Self {
            defs_ordered,
            index,
            name_map,
        }
    }
}

impl DefIndex {
    /// Look up a definition entry by `GlobalDefId`.
    pub fn def_entry(&self, id: GlobalDefId) -> Option<&DefEntry> {
        let def_id = self.name_site_to_def.get(&id.ast_id())?;
        debug_assert_eq!(*def_id, id);
        self.def_entries.iter().find(|e| e.decl_site == id.ast_id())
    }

    /// Find the scope owned by a specific declaration site.
    pub fn find_scope_by_owner(&self, owner: Site) -> Option<ScopeId> {
        self.owner_to_scope.get(&owner).copied()
    }

    pub fn enum_def(&self, idx: EnumDefIdx) -> &EnumDef {
        &self.enum_defs[idx.0 as usize]
    }

    pub fn record_def(&self, idx: RecordDefIdx) -> &RecordDef {
        &self.record_defs[idx.0 as usize]
    }

    pub fn enum_id(&self, idx: EnumDefIdx) -> EnumId {
        EnumId::new(self.enum_defs[idx.0 as usize].enum_type_site)
    }

    pub fn enum_def_by_id(&self, id: EnumId) -> Option<&EnumDef> {
        let idx = self.enum_by_site.get(&id.as_erased())?;
        Some(&self.enum_defs[idx.0 as usize])
    }

    pub fn record_def_by_id(&self, id: RecordId) -> Option<&RecordDef> {
        let idx = self.record_by_site.get(&id.as_erased())?;
        Some(&self.record_defs[idx.0 as usize])
    }

    pub fn instance_decl(&self, idx: InstanceDeclIdx) -> &InstanceDecl {
        &self.instance_decls[idx.0 as usize]
    }

    pub fn nettype_def(&self, idx: NettypeDefIdx) -> &NettypeDef {
        &self.nettype_defs[idx.0 as usize]
    }

    pub fn modport_def(&self, id: ModportDefId) -> Option<&ModportDef> {
        let &idx = self.modports.index.get(&id)?;
        Some(&self.modports.defs_ordered[idx as usize])
    }

    /// Declaration-order modport definitions. Total by construction:
    /// the ordered slice IS the storage, not a projection over a separate map.
    pub fn modport_defs_in_order(&self) -> &[ModportDef] {
        &self.modports.defs_ordered
    }

    pub fn modport_by_name(&self, iface: InterfaceDefId, name: &str) -> Option<&ModportDef> {
        let id = self.modports.name_map.get(&(iface, SmolStr::new(name)))?;
        self.modport_def(*id)
    }

    pub fn modport_name_to_id(&self, iface: InterfaceDefId, name: &str) -> Option<ModportDefId> {
        self.modports
            .name_map
            .get(&(iface, SmolStr::new(name)))
            .copied()
    }

    pub fn package_scope(&self, name: &str) -> Option<ScopeId> {
        for &def_id in &*self.defs_by_name {
            if let Some(entry) = self.def_entry(def_id)
                && entry.kind == DefinitionKind::Package
                && entry.name == name
                && let DefScope::Owned(scope_id) = entry.scope
            {
                let scope_data = self.scopes.get(scope_id);
                if scope_data.kind == ScopeKind::Package {
                    return Some(scope_id);
                }
            }
        }
        None
    }

    pub fn record_id(&self, idx: RecordDefIdx) -> RecordId {
        RecordId::new(self.record_defs[idx.0 as usize].record_type_site)
    }

    /// File scope is always `ScopeId(0)` with `ScopeKind::File`.
    pub fn file_scope(&self) -> Option<ScopeId> {
        let scope = ScopeId(0);
        let data = self.scopes.get(scope);
        (data.kind == ScopeKind::File).then_some(scope)
    }
}

/// Root of a qualified name path.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum QualifiedRoot {
    /// `Pkg::member` -- package-qualified name.
    Package(SmolStr),
    /// `$unit::member` -- compilation-unit-qualified name.
    Unit,
}

impl QualifiedRoot {
    /// Display name for diagnostics.
    pub fn display_name(&self) -> &str {
        match self {
            Self::Package(pkg) => pkg.as_str(),
            Self::Unit => "$unit",
        }
    }
}

/// A name path: simple identifier or qualified path.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NamePath {
    /// Simple identifier: `foo`
    Simple(SmolStr),
    /// Qualified path: `root::member`.
    Qualified(crate::name_lowering::QualifiedPath),
}

impl NamePath {
    pub fn as_simple(&self) -> Option<&str> {
        match self {
            Self::Simple(s) => Some(s.as_str()),
            Self::Qualified(_) => None,
        }
    }

    pub fn display_name(&self) -> String {
        match self {
            Self::Simple(s) => s.to_string(),
            Self::Qualified(qp) => qp.display_name(),
        }
    }
}

/// A recorded name-use site awaiting resolution.
// Gap-1 contract: UseSite stores anchors only; no TextRange fields.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UseSite {
    pub path: NamePath,
    pub expected_ns: ExpectedNs,
    pub scope: ScopeId,
    pub name_ref_site: Site,
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
    pub decl_site: Site,
    pub name_span: NameSpan,
    /// File-local monotonic rank from preorder syntax traversal.
    pub order_key: u32,
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
    pub import_stmt_site: Site,
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

/// A single export declaration with identity and AST anchor.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExportDecl {
    pub id: ExportDeclId,
    pub key: ExportKey,
    pub export_stmt_site: Site,
}

/// Compilation-unit-level environment: implicit imports visible in all files.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompilationUnitEnv {
    pub implicit_imports: Box<[ImplicitImport]>,
}

/// Metadata for a foreach loop variable (LRM 12.7.3).
///
/// Stored in `DefIndex::foreach_var_defs` keyed by the variable's `SymbolId`.
/// The type system uses this to derive the loop variable's type from the
/// iterated array's dimension at the given slot.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ForeachVarDef {
    /// `ErasedAstId` of the containing `ForeachStmt` node.
    pub foreach_stmt: Site,
    /// 0-indexed dimension slot (counting all slots, including skipped).
    pub slot: u32,
}

/// An implicit import added at the compilation-unit level.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImplicitImport {
    pub package: SmolStr,
    pub name: ImportName,
}
