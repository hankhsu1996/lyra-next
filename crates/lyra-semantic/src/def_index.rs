use std::collections::HashMap;

use crate::Site;
use lyra_source::{DeclSpan, FileId};
use smol_str::SmolStr;

use crate::def_entry::DefEntry;
use crate::design_element::DesignElement;
use crate::diagnostic::SemanticDiag;
use crate::enum_def::{EnumDef, EnumDefIdx, EnumId};
use crate::global_index::DefinitionKind;
use crate::instance_decl::{InstanceDecl, InstanceDeclIdx};
use crate::interface_id::InterfaceDefId;
use crate::modport_def::{ModportDef, ModportDefId};
use crate::nettype_def::{NettypeDef, NettypeDefIdx};
use crate::record::{RecordDef, RecordDefIdx, RecordId};
use crate::scopes::{ScopeId, ScopeKind, ScopeTree};
use crate::symbols::{GlobalDefId, Namespace, SymbolId, SymbolTable};
use crate::time_scale::ScopeTimeUnits;

/// Typed semantic identity of a scope owner.
///
/// Covers both definition-namespace owners (module, package, ...) and
/// symbol-namespace owners (function, task). Production data, not
/// test-only.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ScopeOwner {
    /// A definition-namespace item owns this scope.
    Def(GlobalDefId),
    /// A symbol-namespace item owns this scope.
    Symbol(SymbolId),
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
    /// Reverse map from definition `decl_site` to `GlobalDefId`.
    /// Retained for source-anchor-based recovery paths (e.g. `resolve_at`
    /// package name lookup). Not the primary semantic lookup path -- use
    /// `def_entry(id)` for canonical `GlobalDefId -> DefEntry` resolution.
    pub decl_site_to_def: HashMap<Site, GlobalDefId>,
    /// All `GlobalDefId`s in this file, sorted by `(name, kind, ordinal)` for
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
    /// Maps a scope to its typed semantic owner.
    /// Present for named scopes (modules, packages, callables); absent
    /// for anonymous scopes (blocks, generates, file).
    pub scope_owners: HashMap<ScopeId, ScopeOwner>,
    /// Reverse index: maps a typed semantic owner to the `ScopeId` it owns.
    /// Populated atomically with `scope_owners` through
    /// `DefContext::register_scope_owner`.
    pub owner_to_scope: HashMap<ScopeOwner, ScopeId>,
    pub diagnostics: Box<[SemanticDiag]>,
    pub internal_errors: Box<[(Option<Site>, SmolStr)]>,
    /// Design-element extents, source-ordered by construction.
    /// Each entry stores the full trimmed extent computed in the builder.
    pub design_elements: Box<[DesignElement]>,
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
    /// Iterate over definition entries with their canonical `GlobalDefId`s.
    ///
    /// Yields `(GlobalDefId, &DefEntry)` in insertion order. Use this
    /// instead of `def_entries.iter().enumerate()` with manual ID
    /// reconstruction.
    pub fn iter_defs(&self) -> impl Iterator<Item = (GlobalDefId, &DefEntry)> {
        self.def_entries
            .iter()
            .enumerate()
            .map(|(i, entry)| (GlobalDefId::new(self.file, i as u32), entry))
    }

    /// Look up a definition entry by `GlobalDefId`.
    ///
    /// Direct O(1) ordinal indexing into `def_entries`. Returns `None`
    /// if the ID belongs to a different file or the ordinal is out of range.
    pub fn def_entry(&self, id: GlobalDefId) -> Option<&DefEntry> {
        if id.file() != self.file {
            return None;
        }
        self.def_entries.get(id.ordinal() as usize)
    }

    /// Find the scope owned by a definition-namespace item.
    pub fn scope_of_def(&self, def_id: GlobalDefId) -> Option<ScopeId> {
        self.owner_to_scope.get(&ScopeOwner::Def(def_id)).copied()
    }

    /// Find the scope owned by a symbol-namespace item.
    pub fn scope_of_symbol(&self, sym_id: SymbolId) -> Option<ScopeId> {
        self.owner_to_scope
            .get(&ScopeOwner::Symbol(sym_id))
            .copied()
    }

    /// Find the scope owned by a typed semantic owner.
    pub fn scope_of_owner(&self, owner: ScopeOwner) -> Option<ScopeId> {
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
                && let Some(scope_id) = self.scope_of_def(def_id)
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

/// Semantic classification for unresolved-name sites whose behavior depends
/// on implicit-net policy (LRM 6.10).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ImplicitNetSiteKind {
    /// LHS of a continuous assignment (`assign y = ...`).
    ContinuousAssignLhs,
    /// Port connection in a module/interface/program instance.
    PortConnection,
    /// Port expression in a non-ANSI port declaration (LRM 23.2.2.1).
    PortExprDecl,
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
    /// When `Some`, this use-site is an implicit-net candidate site.
    /// Unresolved-name handling depends on the active `default_nettype`
    /// policy at this site's offset.
    pub implicit_net_site: Option<ImplicitNetSiteKind>,
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
    pub name_span: DeclSpan,
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
