use lyra_source::FileId;
use smol_str::SmolStr;

use crate::def_index::{DefIndex, ExpectedNs, ExportDeclId, ExportKey, ImportName, NamePath};
use crate::scopes::{ScopeId, ScopeTree, SymbolNameLookup};
use crate::symbols::{SymbolId, SymbolKind};

/// Offset-independent projection of `DefIndex` for resolution.
///
/// Contains only the facts needed to resolve names: symbol names/kinds,
/// scope tree, and use-site semantic keys. No `TextRange`, no `ErasedAstId`.
/// `PartialEq` compares equal across whitespace-only edits, enabling Salsa
/// to backdate and skip re-running expensive resolution logic downstream.
/// `FileId` is offset-independent and does not affect backdating.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NameGraph {
    pub(crate) file: FileId,
    pub(crate) symbol_names: Box<[SmolStr]>,
    pub(crate) symbol_kinds: Box<[SymbolKind]>,
    pub(crate) scopes: ScopeTree,
    pub(crate) use_entries: Box<[UseEntry]>,
    pub(crate) imports: Box<[ImportEntry]>,
    pub(crate) export_decls: Box<[ExportEntry]>,
}

/// Offset-independent export entry in `NameGraph`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExportEntry {
    pub id: ExportDeclId,
    pub key: ExportKey,
}

/// Offset-independent use-site key.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct UseEntry {
    pub(crate) path: NamePath,
    pub(crate) expected_ns: ExpectedNs,
    pub(crate) scope: ScopeId,
    pub(crate) order_key: u32,
}

/// Offset-independent import entry.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportEntry {
    pub package: SmolStr,
    pub name: ImportName,
    pub scope: ScopeId,
    pub order_key: u32,
}

impl SymbolNameLookup for NameGraph {
    fn name(&self, id: SymbolId) -> &str {
        self.symbol_names[id.0 as usize].as_str()
    }
}

impl NameGraph {
    /// Access the import entries.
    pub fn imports(&self) -> &[ImportEntry] {
        &self.imports
    }

    /// Access the export declarations.
    pub fn export_decls(&self) -> &[ExportEntry] {
        &self.export_decls
    }

    /// Exports owned by `scope`, O(log n) via binary search into sorted slice.
    pub fn exports_for_scope(&self, scope: ScopeId) -> &[ExportEntry] {
        let start = self.export_decls.partition_point(|e| e.id.scope < scope);
        let end = self.export_decls[start..].partition_point(|e| e.id.scope == scope) + start;
        &self.export_decls[start..end]
    }

    /// Imports owned by `scope`, O(log n) via binary search into sorted slice.
    pub fn imports_for_scope(&self, scope: ScopeId) -> &[ImportEntry] {
        let start = self.imports.partition_point(|i| i.scope < scope);
        let end = self.imports[start..].partition_point(|i| i.scope == scope) + start;
        &self.imports[start..end]
    }

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
                order_key: site.order_key,
            })
            .collect();

        let mut imports: Vec<ImportEntry> = def
            .imports
            .iter()
            .map(|imp| ImportEntry {
                package: imp.package.clone(),
                name: imp.name.clone(),
                scope: imp.scope,
                order_key: imp.order_key,
            })
            .collect();
        imports.sort_by_key(|i| i.scope);

        let mut export_decls: Vec<ExportEntry> = def
            .export_decls
            .iter()
            .map(|e| ExportEntry {
                id: e.id,
                key: e.key.clone(),
            })
            .collect();
        export_decls.sort_by(|a, b| (a.id.scope, a.id.ordinal).cmp(&(b.id.scope, b.id.ordinal)));

        Self {
            file: def.file,
            symbol_names,
            symbol_kinds,
            scopes: def.scopes.clone(),
            use_entries,
            imports: imports.into_boxed_slice(),
            export_decls: export_decls.into_boxed_slice(),
        }
    }
}
