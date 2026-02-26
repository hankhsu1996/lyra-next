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
use crate::record::{RecordDef, RecordDefIdx, RecordId};
use crate::scopes::{ScopeId, ScopeKind, ScopeTree};
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
    /// Reverse map from definition name-site to `GlobalDefId`.
    pub name_site_to_def: HashMap<Site, GlobalDefId>,
    /// All `GlobalDefId`s in this file, sorted by `(name, kind, name_site)` for
    /// deterministic iteration. Used by `global_def_index` and `package_scope_index`.
    pub defs_by_name: Box<[GlobalDefId]>,
    pub use_sites: Box<[UseSite]>,
    pub imports: Box<[Import]>,
    pub local_decls: Box<[LocalDecl]>,
    /// Reverse map from name-site `Site` to `SymbolId`.
    /// Used by cross-file resolution to convert `Site` (from
    /// `PackageScopeIndex`) back to a `SymbolId` in this file.
    pub name_site_to_symbol: HashMap<Site, SymbolId>,
    /// Maps parameter name-site `Site` to its initializer `Expression`
    /// `Site`. Key present with `None` = parameter with no default value.
    /// Key absent = not a parameter (not tracked).
    pub name_site_to_init_expr: HashMap<Site, Option<Site>>,
    pub enum_defs: Box<[EnumDef]>,
    pub enum_by_site: HashMap<Site, EnumDefIdx>,
    pub record_defs: Box<[RecordDef]>,
    pub record_by_site: HashMap<Site, RecordDefIdx>,
    pub instance_decls: Box<[InstanceDecl]>,
    pub modport_defs: HashMap<ModportDefId, ModportDef>,
    pub modport_name_map: HashMap<(InterfaceDefId, SmolStr), ModportDefId>,
    pub export_decls: Box<[ExportDecl]>,
    pub diagnostics: Box<[SemanticDiag]>,
    pub internal_errors: Box<[(Option<Site>, SmolStr)]>,
}

impl DefIndex {
    /// Look up a definition entry by `GlobalDefId`.
    pub fn def_entry(&self, id: GlobalDefId) -> Option<&DefEntry> {
        let def_id = self.name_site_to_def.get(&id.ast_id())?;
        debug_assert_eq!(*def_id, id);
        self.def_entries.iter().find(|e| e.decl_site == id.ast_id())
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

    pub fn modport_by_name(&self, iface: InterfaceDefId, name: &str) -> Option<&ModportDef> {
        let id = self.modport_name_map.get(&(iface, SmolStr::new(name)))?;
        self.modport_defs.get(id)
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

/// An implicit import added at the compilation-unit level.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImplicitImport {
    pub package: SmolStr,
    pub name: ImportName,
}
