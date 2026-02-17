mod const_eval;
mod diagnostics;
mod expr_queries;
mod lower_diag;
pub mod pipeline;
mod resolve_at;
pub mod semantic;
pub mod type_queries;

// Re-export pipeline queries
pub use pipeline::{
    ast_id_map, ast_root, full_expansion_stack, include_graph, lex_file, line_index, parse_file,
    preprocess_file, source_map, update_file_text,
};

// Re-export semantic queries
pub use semantic::{
    def_index_file, global_def_index, name_graph_file, package_scope_index, resolve_core_file,
    resolve_index_file,
};

// Re-export const-eval
pub use const_eval::{ConstExprRef, eval_const_int};

// Re-export type queries
pub use type_queries::{SymbolRef, type_of_symbol, type_of_symbol_raw};

// Re-export expression queries
pub use expr_queries::{ExprRef, type_of_expr};

// Re-export resolve-at helpers
pub use resolve_at::{TypeAtResult, resolve_at, symbol_global, type_at};

// Re-export diagnostics
pub use diagnostics::{file_diagnostics, type_diagnostics, unit_diagnostics};

/// Sorted include-path lookup for deterministic O(log n) resolution.
///
/// Stored as a sorted `Vec` to satisfy Salsa input constraints
/// (`Clone + Eq + Hash`). Construction sorts entries by path;
/// lookup uses binary search.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct IncludeMap {
    entries: Vec<(String, SourceFile)>,
}

impl IncludeMap {
    /// Build an `IncludeMap` from unsorted entries. Sorts by path.
    pub fn new(mut entries: Vec<(String, SourceFile)>) -> Self {
        entries.sort_by(|(a, _), (b, _)| a.cmp(b));
        Self { entries }
    }

    /// Look up a path in O(log n).
    pub fn lookup(&self, path: &str) -> Option<SourceFile> {
        let idx = self
            .entries
            .binary_search_by(|(p, _)| p.as_str().cmp(path))
            .ok()?;
        Some(self.entries[idx].1)
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = &(String, SourceFile)> {
        self.entries.iter()
    }
}

/// A source file input for the Salsa database.
///
/// The `include_map` field carries resolution metadata: which include
/// paths map to which files. Only the tool layer should set this field
/// (via `set_include_map`). Long-term, this could move to a separate
/// tracked query to decouple content changes from resolution changes.
#[salsa::input]
pub struct SourceFile {
    pub file_id: lyra_source::FileId,
    #[return_ref]
    pub text: String,
    #[return_ref]
    pub include_map: IncludeMap,
}

/// A compilation unit: the set of source files compiled together.
///
/// IEEE 1800-2023 section 3.12.1: a compilation unit is a tool-defined
/// collection of source files. Module names in the definitions name space
/// (section 3.13(a)) are global within a compilation unit.
///
/// `files` must be sorted by `FileId` and deduplicated.
#[salsa::input]
pub struct CompilationUnit {
    #[return_ref]
    pub files: Vec<SourceFile>,
}

/// Create a `CompilationUnit` from a set of files, sorting and deduplicating.
pub fn new_compilation_unit(
    db: &dyn salsa::Database,
    mut files: Vec<SourceFile>,
) -> CompilationUnit {
    files.sort_by_key(|f| f.file_id(db));
    files.dedup_by_key(|f| f.file_id(db));
    CompilationUnit::new(db, files)
}

/// O(log n) lookup of `SourceFile` by `FileId` within a compilation unit.
pub fn source_file_by_id(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    file_id: lyra_source::FileId,
) -> Option<SourceFile> {
    let files = unit.files(db);
    let idx = files
        .binary_search_by_key(&file_id, |f| f.file_id(db))
        .ok()?;
    Some(files[idx])
}

/// The central Salsa database for Lyra.
#[salsa::db]
#[derive(Default, Clone)]
pub struct LyraDatabase {
    storage: salsa::Storage<Self>,
}

#[salsa::db]
impl salsa::Database for LyraDatabase {
    fn salsa_event(&self, _event: &dyn Fn() -> salsa::Event) {}
}

#[cfg(test)]
mod tests;
