mod lower_diag;

use lyra_preprocess::{IncludeProvider, ResolvedInclude};
use lyra_semantic::def_index::DefIndex;
use lyra_semantic::global_index::{
    DefinitionKind, GlobalDefIndex, PackageScope, PackageScopeIndex,
};
use lyra_semantic::name_graph::NameGraph;
use lyra_semantic::resolve_index::{CoreResolveOutput, ResolveIndex};
use lyra_semantic::scopes::ScopeKind;
use lyra_semantic::symbols::{GlobalDefId, GlobalSymbolId};
use salsa::Setter;
use smol_str::SmolStr;

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

/// Lex a source file into tokens (including trivia and EOF).
#[salsa::tracked(return_ref)]
pub fn lex_file(db: &dyn salsa::Database, file: SourceFile) -> Vec<lyra_lexer::Token> {
    lyra_lexer::lex(file.text(db))
}

/// Include provider that resolves paths via Salsa queries.
struct DbIncludeProvider<'a> {
    db: &'a dyn salsa::Database,
    include_map: &'a IncludeMap,
}

impl IncludeProvider for DbIncludeProvider<'_> {
    fn resolve(&self, path: &str) -> Option<ResolvedInclude<'_>> {
        let file = self.include_map.lookup(path)?;
        Some(ResolvedInclude {
            file_id: file.file_id(self.db),
            tokens: lex_file(self.db, file),
            text: file.text(self.db),
        })
    }
}

/// Run the preprocessor over lexed tokens.
#[salsa::tracked(return_ref)]
pub fn preprocess_file(
    db: &dyn salsa::Database,
    file: SourceFile,
) -> lyra_preprocess::PreprocOutput {
    let include_map = file.include_map(db);
    let provider = DbIncludeProvider { db, include_map };
    lyra_preprocess::preprocess(
        file.file_id(db),
        lex_file(db, file),
        file.text(db),
        &provider,
    )
}

/// Parse a source file into a lossless green tree with diagnostics.
#[salsa::tracked(return_ref)]
pub fn parse_file(db: &dyn salsa::Database, file: SourceFile) -> lyra_parser::Parse {
    let pp = preprocess_file(db, file);
    lyra_parser::parse(&pp.tokens, &pp.expanded_text)
}

/// Build the line index for a source file (Salsa-cached).
#[salsa::tracked(return_ref)]
pub fn line_index(db: &dyn salsa::Database, file: SourceFile) -> lyra_source::LineIndex {
    lyra_source::LineIndex::new(file.text(db))
}

/// Update the text of an existing source file, triggering Salsa
/// invalidation for all queries that depend on it.
pub fn update_file_text(db: &mut dyn salsa::Database, file: SourceFile, text: String) {
    file.set_text(db).to(text);
}

/// Access the source map for a preprocessed file.
pub fn source_map(db: &dyn salsa::Database, file: SourceFile) -> &lyra_preprocess::SourceMap {
    &preprocess_file(db, file).source_map
}

/// Access the include graph for a preprocessed file.
pub fn include_graph(db: &dyn salsa::Database, file: SourceFile) -> &lyra_preprocess::IncludeGraph {
    &preprocess_file(db, file).includes
}

/// Return the expansion frame for an offset in a file's expanded output.
///
/// Returns a single-element `Vec` if the offset falls in an included
/// region, or an empty `Vec` for identity-mapped (non-included)
/// positions. The `Vec` return type is forward-compatible with
/// recursive include expansion, which will produce multi-frame
/// stacks.
///
/// Currently, `preprocess()` only performs one level of include
/// expansion (splicing raw file text, not recursively expanded
/// output). Chasing `spelling` offsets across file boundaries would
/// compare raw-text offsets against expanded-output source maps,
/// producing incorrect provenance. Transitive chaining will be
/// added when the preprocessor gains recursive expansion.
pub fn full_expansion_stack(
    db: &dyn salsa::Database,
    file: SourceFile,
    offset: lyra_source::TextSize,
) -> Vec<lyra_source::ExpansionFrame> {
    let sm = source_map(db, file);
    sm.expansion_frame(offset).into_iter().collect()
}

/// Return a typed `SourceFile` AST root for the given file.
///
/// Returns `None` if the root node cannot be cast to `SourceFile` (should
/// not happen with a correct parser, but no panic in library code).
pub fn ast_root(db: &dyn salsa::Database, file: SourceFile) -> Option<lyra_ast::SourceFile> {
    let parse = parse_file(db, file);
    lyra_ast::AstNode::cast(parse.syntax())
}

/// Build the per-file `AstIdMap` (Salsa-cached).
#[salsa::tracked(return_ref)]
pub fn ast_id_map(db: &dyn salsa::Database, file: SourceFile) -> lyra_ast::AstIdMap {
    let parse = parse_file(db, file);
    lyra_ast::AstIdMap::from_root(file.file_id(db), &parse.syntax())
}

/// Build per-file definition index (Salsa-cached).
///
/// Collects declarations, scopes, exports, and use-sites from the parse tree.
/// Does NOT resolve name uses -- see `resolve_index_file`.
#[salsa::tracked(return_ref)]
pub fn def_index_file(db: &dyn salsa::Database, file: SourceFile) -> DefIndex {
    let parse = parse_file(db, file);
    let map = ast_id_map(db, file);
    lyra_semantic::build_def_index(file.file_id(db), parse, map)
}

/// Extract offset-independent name graph from the definition index (Salsa-cached).
///
/// On whitespace-only edits, this query re-executes but produces an equal
/// result (no ranges), so Salsa backdates it and skips re-running the
/// expensive `resolve_core_file` downstream.
#[salsa::tracked(return_ref)]
pub fn name_graph_file(db: &dyn salsa::Database, file: SourceFile) -> NameGraph {
    NameGraph::from_def_index(def_index_file(db, file))
}

/// Build the global definition index for a compilation unit (Salsa-cached).
///
/// Aggregates module and package names with `GlobalDefId`s from all files'
/// `DefIndex` exports. On whitespace-only edits, `ErasedAstId` values are
/// topology-stable, so the result backdates correctly.
#[salsa::tracked(return_ref)]
pub fn global_def_index(db: &dyn salsa::Database, unit: CompilationUnit) -> GlobalDefIndex {
    let mut entries: Vec<(SmolStr, GlobalDefId, DefinitionKind)> = Vec::new();
    for file in unit.files(db) {
        let def = def_index_file(db, *file);
        // Collect modules
        for &sym_id in &*def.exports.modules {
            let sym = def.symbols.get(sym_id);
            for (ast_id, &sid) in &def.decl_to_symbol {
                if sid == sym_id {
                    entries.push((
                        sym.name.clone(),
                        GlobalDefId::new(*ast_id),
                        DefinitionKind::Module,
                    ));
                    break;
                }
            }
        }
        // Collect packages
        for &sym_id in &*def.exports.packages {
            let sym = def.symbols.get(sym_id);
            for (ast_id, &sid) in &def.decl_to_symbol {
                if sid == sym_id {
                    entries.push((
                        sym.name.clone(),
                        GlobalDefId::new(*ast_id),
                        DefinitionKind::Package,
                    ));
                    break;
                }
            }
        }
    }
    lyra_semantic::global_index::build_global_def_index(&entries)
}

/// Build the package scope index for a compilation unit (Salsa-cached).
///
/// Extracts symbols from package scopes in all files, split by namespace.
#[salsa::tracked(return_ref)]
pub fn package_scope_index(db: &dyn salsa::Database, unit: CompilationUnit) -> PackageScopeIndex {
    let mut packages = Vec::new();
    for file in unit.files(db) {
        let def = def_index_file(db, *file);
        // Find package symbols and their scopes
        for &sym_id in &*def.exports.packages {
            let pkg_sym = def.symbols.get(sym_id);
            let pkg_scope = pkg_sym.scope;
            let scope_data = def.scopes.get(pkg_scope);
            if scope_data.kind != ScopeKind::Package {
                continue;
            }

            let mut value_ns: Vec<(SmolStr, GlobalDefId)> = Vec::new();
            let mut type_ns: Vec<(SmolStr, GlobalDefId)> = Vec::new();

            // Collect Value-namespace symbols from this package scope
            for &child_sym_id in &*scope_data.value_ns {
                let child_sym = def.symbols.get(child_sym_id);
                // Look up the GlobalDefId for this symbol
                for (ast_id, &sid) in &def.decl_to_symbol {
                    if sid == child_sym_id {
                        value_ns.push((child_sym.name.clone(), GlobalDefId::new(*ast_id)));
                        break;
                    }
                }
            }

            // Collect Type-namespace symbols
            for &child_sym_id in &*scope_data.type_ns {
                let child_sym = def.symbols.get(child_sym_id);
                for (ast_id, &sid) in &def.decl_to_symbol {
                    if sid == child_sym_id {
                        type_ns.push((child_sym.name.clone(), GlobalDefId::new(*ast_id)));
                        break;
                    }
                }
            }

            value_ns.sort_by(|(a, _), (b, _)| a.cmp(b));
            type_ns.sort_by(|(a, _), (b, _)| a.cmp(b));

            packages.push(PackageScope {
                name: pkg_sym.name.clone(),
                value_ns: value_ns.into_boxed_slice(),
                type_ns: type_ns.into_boxed_slice(),
            });
        }
    }
    lyra_semantic::global_index::build_package_scope_index(packages)
}

/// Resolve all use-sites using only offset-independent data (Salsa-cached).
///
/// Depends on `name_graph_file`, `global_def_index`, and
/// `package_scope_index`. When all are backdated (e.g. whitespace edit),
/// this query is NOT re-executed.
#[salsa::tracked(return_ref)]
pub fn resolve_core_file(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
) -> CoreResolveOutput {
    let graph = name_graph_file(db, file);
    let global = global_def_index(db, unit);
    let pkg_scope = package_scope_index(db, unit);
    lyra_semantic::build_resolve_core(graph, global, pkg_scope)
}

/// Build per-file resolution index (Salsa-cached).
///
/// Combines offset-independent resolve results from `resolve_core_file`
/// with offset-dependent data from `def_index_file` (`ast_ids`, ranges)
/// to produce the final `HashMap` and diagnostics. Trivially cheap.
#[salsa::tracked(return_ref)]
pub fn resolve_index_file(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
) -> ResolveIndex {
    let def = def_index_file(db, file);
    let core = resolve_core_file(db, file, unit);
    let lookup_decl = |def_id: GlobalDefId| -> Option<lyra_semantic::symbols::SymbolId> {
        let target_file_id = def_id.file();
        let target_file = source_file_by_id(db, unit, target_file_id)?;
        let target_def = def_index_file(db, target_file);
        target_def.decl_to_symbol.get(&def_id.ast_id()).copied()
    };
    lyra_semantic::build_resolve_index(def, core, &lookup_decl)
}

/// Resolve the name at a cursor position.
///
/// Finds the nearest `NameRef` at `offset`, looks up its `AstId`,
/// and returns the resolved `GlobalSymbolId` if found.
/// Also handles module instantiation type names (e.g. `adder` in
/// `adder u1(...)`) and qualified names (`pkg::sym`).
pub fn resolve_at(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
    offset: lyra_source::TextSize,
) -> Option<GlobalSymbolId> {
    let parse = parse_file(db, file);
    let ast_map = ast_id_map(db, file);
    let resolve = resolve_index_file(db, file, unit);

    // Try NameRef first (common case)
    if let Some(name_ref) = find_name_ref_at(&parse.syntax(), offset)
        && let Some(ast_id) = ast_map.ast_id(&name_ref)
        && let Some(resolution) = resolve.resolutions.get(&ast_id.erase())
    {
        return Some(resolution.symbol);
    }

    // Try qualified name (pkg::sym)
    if let Some(result) = find_qualified_name_at(db, file, unit, &parse.syntax(), offset) {
        return Some(result);
    }

    // Fallback: module instantiation type name
    if let Some(inst) = find_module_instantiation_name_at(&parse.syntax(), offset)
        && let Some(ast_id) = ast_map.ast_id(&inst)
        && let Some(resolution) = resolve.resolutions.get(&ast_id.erase())
    {
        return Some(resolution.symbol);
    }

    None
}

/// Look up a symbol by its global id within a compilation unit.
pub fn symbol_global(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    id: GlobalSymbolId,
) -> Option<&lyra_semantic::symbols::Symbol> {
    let file = source_file_by_id(db, unit, id.file)?;
    Some(def_index_file(db, file).symbols.get(id.local))
}

/// Find a `NameRef` node at or near the given offset.
fn find_name_ref_at(
    root: &lyra_parser::SyntaxNode,
    offset: lyra_source::TextSize,
) -> Option<lyra_ast::NameRef> {
    use lyra_ast::AstNode;
    let token = root.token_at_offset(offset).right_biased()?;
    token.parent_ancestors().find_map(lyra_ast::NameRef::cast)
}

/// Find a qualified name at the cursor position and resolve it.
///
/// When on the package name part: resolve to the package declaration.
/// When on the member name part: look up the `QualifiedName`'s `AstId`
/// in the resolve index.
fn find_qualified_name_at(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
    root: &lyra_parser::SyntaxNode,
    offset: lyra_source::TextSize,
) -> Option<GlobalSymbolId> {
    use lyra_ast::AstNode;
    let token = root.token_at_offset(offset).right_biased()?;
    let qn = token
        .parent_ancestors()
        .find_map(lyra_ast::QualifiedName::cast)?;

    let segments: Vec<_> = qn.segments().collect();
    if segments.len() < 2 {
        return None;
    }

    // Determine if cursor is on the package name or member name
    let on_package = segments[0].text_range().contains(offset);

    if on_package {
        // Resolve to the package declaration
        let pkg_name = segments[0].text();
        let global = global_def_index(db, unit);
        let def_id = global.resolve_package(pkg_name.as_ref())?;
        let target_file = source_file_by_id(db, unit, def_id.file())?;
        let target_def = def_index_file(db, target_file);
        let local = target_def.decl_to_symbol.get(&def_id.ast_id()).copied()?;
        Some(GlobalSymbolId {
            file: def_id.file(),
            local,
        })
    } else {
        // Resolve via the QualifiedName's AstId in the resolve index
        let ast_map = ast_id_map(db, file);
        let resolve = resolve_index_file(db, file, unit);
        let ast_id = ast_map.ast_id(&qn)?;
        let resolution = resolve.resolutions.get(&ast_id.erase())?;
        Some(resolution.symbol)
    }
}

/// Find a `ModuleInstantiation` node where the cursor is on the module
/// type name (not the instance name).
fn find_module_instantiation_name_at(
    root: &lyra_parser::SyntaxNode,
    offset: lyra_source::TextSize,
) -> Option<lyra_ast::ModuleInstantiation> {
    use lyra_ast::AstNode;
    let token = root.token_at_offset(offset).right_biased()?;
    let inst = token
        .parent_ancestors()
        .find_map(lyra_ast::ModuleInstantiation::cast)?;
    // Only match if offset is within the module name token
    let name_token = inst.module_name()?;
    if name_token.text_range().contains(offset) {
        Some(inst)
    } else {
        None
    }
}

/// Convert parse, preprocess, and semantic errors into structured diagnostics (Salsa-cached).
#[salsa::tracked(return_ref)]
pub fn file_diagnostics(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
) -> Vec<lyra_diag::Diagnostic> {
    let pp = preprocess_file(db, file);
    let parse = parse_file(db, file);
    let def = def_index_file(db, file);
    let resolve = resolve_index_file(db, file, unit);
    lower_diag::lower_file_diagnostics(file.file_id(db), pp, parse, def, resolve)
}

/// Unit-level diagnostics: duplicate definitions in the definitions namespace.
///
/// Walks `GlobalDefIndex.definitions()`, finds adjacent entries with the
/// same name, and emits one diagnostic per duplicate. Catches module/module,
/// package/package, and module/package name collisions.
#[salsa::tracked(return_ref)]
pub fn unit_diagnostics(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
) -> Box<[lyra_diag::Diagnostic]> {
    let global = global_def_index(db, unit);
    let defs = global.definitions();
    let mut diags = Vec::new();

    let mut i = 0;
    while i < defs.len() {
        let (name, _, _) = &defs[i];
        let mut j = i + 1;
        while j < defs.len() && defs[j].0 == *name {
            j += 1;
        }
        if j - i > 1 {
            // Duplicate group: emit diagnostics for entries [i+1..j]
            for (_, dup_def_id, _) in &defs[(i + 1)..j] {
                let dup_file_id = dup_def_id.file();
                if let Some(dup_file) = source_file_by_id(db, unit, dup_file_id) {
                    let dup_def = def_index_file(db, dup_file);
                    if let Some(&sym_id) = dup_def.decl_to_symbol.get(&dup_def_id.ast_id()) {
                        let sym = dup_def.symbols.get(sym_id);
                        let pp = preprocess_file(db, dup_file);
                        if let Some(span) = pp.source_map.map_span(sym.def_range) {
                            diags.push(
                                lyra_diag::Diagnostic::new(
                                    lyra_diag::Severity::Error,
                                    lyra_diag::DiagnosticCode::DUPLICATE_DEFINITION,
                                    lyra_diag::Message::new(
                                        lyra_diag::MessageId::DuplicateDefinitionInUnit,
                                        vec![lyra_diag::Arg::Name(name.clone())],
                                    ),
                                )
                                .with_label(lyra_diag::Label {
                                    kind: lyra_diag::LabelKind::Primary,
                                    span,
                                    message: lyra_diag::Message::simple(
                                        lyra_diag::MessageId::RedefinedHere,
                                    ),
                                }),
                            );
                        }
                    }
                }
            }
        }
        i = j;
    }

    diags.into_boxed_slice()
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
mod tests {
    use lyra_ast::AstNode;

    use super::*;

    fn new_file(db: &dyn salsa::Database, id: u32, text: &str) -> SourceFile {
        SourceFile::new(
            db,
            lyra_source::FileId(id),
            text.to_string(),
            IncludeMap::default(),
        )
    }

    /// Create a single-file compilation unit (convenience for tests).
    fn single_file_unit(db: &dyn salsa::Database, file: SourceFile) -> CompilationUnit {
        new_compilation_unit(db, vec![file])
    }

    #[test]
    fn smoke_db_roundtrip() {
        let db = LyraDatabase::default();
        let file = new_file(&db, 0, "module top; endmodule");
        assert_eq!(file.text(&db), "module top; endmodule");
    }

    #[test]
    fn end_to_end_parse() {
        let db = LyraDatabase::default();
        let file = new_file(&db, 0, "module top; endmodule");
        let parse = parse_file(&db, file);
        assert!(parse.errors.is_empty());
        assert_eq!(parse.syntax().text().to_string(), "module top; endmodule");
    }

    #[test]
    fn ast_root_returns_typed_source_file() {
        let db = LyraDatabase::default();
        let file = new_file(&db, 0, "module top; endmodule");
        let root = ast_root(&db, file).expect("parser produces SourceFile root");
        let modules: Vec<_> = root.modules().collect();
        assert_eq!(modules.len(), 1);
        assert_eq!(
            modules[0].name().map(|n| n.text().to_string()),
            Some("top".to_string()),
        );
    }

    #[test]
    fn ast_id_map_roundtrip() {
        let db = LyraDatabase::default();
        let file = new_file(&db, 0, "module top; endmodule");
        let root = ast_root(&db, file).expect("parser produces SourceFile root");
        let map = ast_id_map(&db, file);
        let parse = parse_file(&db, file);

        for module in root.modules() {
            let id = map.ast_id(&module).expect("module should have an id");
            let recovered = map
                .get(&parse.syntax(), id)
                .expect("should recover module from id");
            assert_eq!(module.text_range(), recovered.text_range());
        }
    }

    #[test]
    fn cross_file_get_returns_none() {
        let db = LyraDatabase::default();
        let file_a = new_file(&db, 0, "module a; endmodule");
        let file_b = new_file(&db, 1, "module b; endmodule");
        let root_b = ast_root(&db, file_b).expect("parser produces SourceFile root");
        let map_a = ast_id_map(&db, file_a);
        let map_b = ast_id_map(&db, file_b);
        let parse_a = parse_file(&db, file_a);

        let module_b = root_b.modules().next().expect("file b has a module");
        let id_b = map_b.ast_id(&module_b).expect("module should have an id");
        assert!(map_a.get(&parse_a.syntax(), id_b).is_none());
    }

    #[test]
    fn source_map_identity() {
        let db = LyraDatabase::default();
        let fid = lyra_source::FileId(5);
        let file = SourceFile::new(
            &db,
            fid,
            "module m; endmodule".to_string(),
            IncludeMap::default(),
        );
        let sm = source_map(&db, file);
        let range = lyra_source::TextRange::new(
            lyra_source::TextSize::new(0),
            lyra_source::TextSize::new(6),
        );
        let span = sm.map_span(range).expect("in-bounds should map");
        assert_eq!(span.file, fid);
    }

    #[test]
    fn include_graph_empty() {
        let db = LyraDatabase::default();
        let file = new_file(&db, 0, "module m; endmodule");
        let ig = include_graph(&db, file);
        assert!(ig.is_empty());
        assert!(ig.dependencies().is_empty());
    }

    #[test]
    fn file_diagnostics_maps_parse_errors() {
        let db = LyraDatabase::default();
        let file = SourceFile::new(
            &db,
            lyra_source::FileId(42),
            "module top;".to_string(),
            IncludeMap::default(),
        );
        let unit = single_file_unit(&db, file);
        let diags = file_diagnostics(&db, file, unit);
        assert!(!diags.is_empty());
        for d in diags {
            assert_eq!(d.span().expect("has span").file, lyra_source::FileId(42));
            assert_eq!(d.severity, lyra_diag::Severity::Error);
        }
    }

    #[test]
    fn roundtrip_expanded_text() {
        let db = LyraDatabase::default();
        let file = new_file(&db, 0, "module top; endmodule");
        let pp = preprocess_file(&db, file);
        let parse = parse_file(&db, file);
        assert_eq!(parse.syntax().text().to_string(), pp.expanded_text);
    }

    #[test]
    fn roundtrip_expanded_text_with_include() {
        let db = LyraDatabase::default();
        let file_b = SourceFile::new(
            &db,
            lyra_source::FileId(1),
            "wire w;".to_string(),
            IncludeMap::default(),
        );
        let file_a = SourceFile::new(
            &db,
            lyra_source::FileId(0),
            "module top; `include \"b.sv\"\nendmodule".to_string(),
            IncludeMap::new(vec![("b.sv".to_string(), file_b)]),
        );
        let pp = preprocess_file(&db, file_a);
        let parse = parse_file(&db, file_a);
        assert_eq!(parse.syntax().text().to_string(), pp.expanded_text);
    }

    // Event-logging database for cache-hit assertions.

    #[salsa::db]
    #[derive(Clone)]
    struct EventDb {
        storage: salsa::Storage<Self>,
        log: std::sync::Arc<std::sync::Mutex<Vec<String>>>,
    }

    impl EventDb {
        fn new() -> Self {
            Self {
                storage: Default::default(),
                log: Default::default(),
            }
        }

        fn take_log(&self) -> Vec<String> {
            std::mem::take(&mut self.log.lock().expect("lock poisoned"))
        }
    }

    #[salsa::db]
    impl salsa::Database for EventDb {
        fn salsa_event(&self, event: &dyn Fn() -> salsa::Event) {
            let event = event();
            if let salsa::EventKind::WillExecute { .. } = event.kind {
                self.log
                    .lock()
                    .expect("lock poisoned")
                    .push(format!("{:?}", event.kind));
            }
        }
    }

    fn has_will_execute(log: &[String]) -> bool {
        log.iter().any(|e| e.contains("WillExecute"))
    }

    #[test]
    fn edit_file_a_does_not_reparse_file_b() {
        let mut db = EventDb::new();
        let file_a = SourceFile::new(
            &db,
            lyra_source::FileId(0),
            "module a; endmodule".to_string(),
            IncludeMap::default(),
        );
        let file_b = SourceFile::new(
            &db,
            lyra_source::FileId(1),
            "module b; endmodule".to_string(),
            IncludeMap::default(),
        );

        let _ = parse_file(&db, file_a);
        let _ = parse_file(&db, file_b);
        db.take_log();

        update_file_text(&mut db, file_a, "module a2; endmodule".to_string());

        let _ = parse_file(&db, file_b);
        let log = db.take_log();
        assert!(
            !has_will_execute(&log),
            "file_b should not re-execute after file_a edit: {log:?}",
        );

        let _ = parse_file(&db, file_a);
        let log = db.take_log();
        assert!(
            has_will_execute(&log),
            "file_a should re-execute after its text changed: {log:?}",
        );
    }

    #[test]
    fn line_index_invalidated_on_text_change() {
        let mut db = EventDb::new();
        let file = SourceFile::new(
            &db,
            lyra_source::FileId(0),
            "line one\nline two".to_string(),
            IncludeMap::default(),
        );
        let idx = line_index(&db, file);
        assert_eq!(idx.line_count(), 2);

        update_file_text(&mut db, file, "line one\nline two\nline three".to_string());
        let idx = line_index(&db, file);
        assert_eq!(idx.line_count(), 3);
    }

    #[test]
    fn unchanged_text_is_cached() {
        let db = EventDb::new();
        let file = SourceFile::new(
            &db,
            lyra_source::FileId(0),
            "module m; endmodule".to_string(),
            IncludeMap::default(),
        );

        let _ = parse_file(&db, file);
        db.take_log();

        let _ = parse_file(&db, file);
        let log = db.take_log();
        assert!(
            !has_will_execute(&log),
            "no re-execution expected when text unchanged: {log:?}",
        );
    }

    #[test]
    fn edit_included_file_invalidates_includer() {
        let mut db = EventDb::new();
        let file_b = SourceFile::new(
            &db,
            lyra_source::FileId(1),
            "wire w;".to_string(),
            IncludeMap::default(),
        );
        let file_a = SourceFile::new(
            &db,
            lyra_source::FileId(0),
            "module top; `include \"b.sv\"\nendmodule".to_string(),
            IncludeMap::new(vec![("b.sv".to_string(), file_b)]),
        );

        let _ = parse_file(&db, file_a);
        db.take_log();

        update_file_text(&mut db, file_b, "wire x;".to_string());

        let _ = parse_file(&db, file_a);
        let log = db.take_log();
        assert!(
            has_will_execute(&log),
            "file_a should re-execute after included file_b changed: {log:?}",
        );
    }

    #[test]
    fn full_expansion_stack_identity() {
        let db = LyraDatabase::default();
        let file = new_file(&db, 0, "module top; endmodule");
        let stack = full_expansion_stack(&db, file, lyra_source::TextSize::new(0));
        assert!(stack.is_empty(), "no includes -> empty stack");
    }

    #[test]
    fn full_expansion_stack_single_include() {
        let db = LyraDatabase::default();
        let file_b = SourceFile::new(
            &db,
            lyra_source::FileId(1),
            "wire w;".to_string(),
            IncludeMap::default(),
        );
        let file_a = SourceFile::new(
            &db,
            lyra_source::FileId(0),
            "module top; `include \"b.sv\"\nendmodule".to_string(),
            IncludeMap::new(vec![("b.sv".to_string(), file_b)]),
        );

        // Offset 12 is in the included range
        let stack = full_expansion_stack(&db, file_a, lyra_source::TextSize::new(12));
        assert_eq!(stack.len(), 1);
        assert_eq!(stack[0].kind, lyra_source::ExpansionKind::Include);
        assert_eq!(stack[0].call_site.file, lyra_source::FileId(0));
        assert_eq!(stack[0].spelling.file, lyra_source::FileId(1));
        assert_eq!(stack[0].spelling.offset, lyra_source::TextSize::new(0));
    }

    #[test]
    fn full_expansion_stack_nested_is_single_frame() {
        let db = LyraDatabase::default();
        // C has plain content
        let file_c = SourceFile::new(
            &db,
            lyra_source::FileId(2),
            "wire c;".to_string(),
            IncludeMap::default(),
        );
        // B includes C
        let file_b = SourceFile::new(
            &db,
            lyra_source::FileId(1),
            "`include \"c.sv\"".to_string(),
            IncludeMap::new(vec![("c.sv".to_string(), file_c)]),
        );
        // A includes B
        let file_a = SourceFile::new(
            &db,
            lyra_source::FileId(0),
            "module top; `include \"b.sv\"\nendmodule".to_string(),
            IncludeMap::new(vec![("b.sv".to_string(), file_b)]),
        );

        // preprocess() only does one level of expansion, so A splices
        // B's raw text (which contains `include "c.sv"). The offset
        // maps to B, not transitively to C.
        let stack = full_expansion_stack(&db, file_a, lyra_source::TextSize::new(12));
        assert_eq!(stack.len(), 1, "one-level expansion -> 1 frame");
        assert_eq!(stack[0].call_site.file, lyra_source::FileId(0));
        assert_eq!(stack[0].spelling.file, lyra_source::FileId(1));
    }

    #[test]
    fn edit_included_file_does_not_invalidate_unrelated() {
        let mut db = EventDb::new();
        let file_b = SourceFile::new(
            &db,
            lyra_source::FileId(1),
            "wire w;".to_string(),
            IncludeMap::default(),
        );
        let file_a = SourceFile::new(
            &db,
            lyra_source::FileId(0),
            "module top; `include \"b.sv\"\nendmodule".to_string(),
            IncludeMap::new(vec![("b.sv".to_string(), file_b)]),
        );
        let file_c = SourceFile::new(
            &db,
            lyra_source::FileId(2),
            "module c; endmodule".to_string(),
            IncludeMap::default(),
        );

        let _ = parse_file(&db, file_a);
        let _ = parse_file(&db, file_c);
        db.take_log();

        update_file_text(&mut db, file_b, "wire x;".to_string());

        let _ = parse_file(&db, file_c);
        let log = db.take_log();
        assert!(
            !has_will_execute(&log),
            "file_c should not re-execute after file_b changed: {log:?}",
        );

        let _ = parse_file(&db, file_a);
        let log = db.take_log();
        assert!(
            has_will_execute(&log),
            "file_a should re-execute after included file_b changed: {log:?}",
        );
    }

    // Semantic resolve tests

    #[test]
    fn resolve_at_port() {
        let db = LyraDatabase::default();
        let file = new_file(&db, 0, "module m(input logic a); assign x = a; endmodule");
        let unit = single_file_unit(&db, file);
        // 'a' in 'assign x = a' is at offset 36
        let result = resolve_at(&db, file, unit, lyra_source::TextSize::new(36));
        assert!(result.is_some(), "port 'a' should resolve");
        let sym =
            symbol_global(&db, unit, result.expect("checked above")).expect("symbol should exist");
        assert_eq!(sym.name.as_str(), "a");
        assert_eq!(sym.kind, lyra_semantic::symbols::SymbolKind::Port);
    }

    #[test]
    fn resolve_at_net() {
        let db = LyraDatabase::default();
        let file = new_file(&db, 0, "module m; wire w; assign w = 1; endmodule");
        let unit = single_file_unit(&db, file);
        // 'w' in 'assign w = 1' -- find offset of the second 'w'
        let text = file.text(&db);
        let w_pos = text.rfind("w =").expect("should find 'w ='");
        let result = resolve_at(&db, file, unit, lyra_source::TextSize::new(w_pos as u32));
        assert!(result.is_some(), "net 'w' should resolve");
        let sym =
            symbol_global(&db, unit, result.expect("checked above")).expect("symbol should exist");
        assert_eq!(sym.name.as_str(), "w");
        assert_eq!(sym.kind, lyra_semantic::symbols::SymbolKind::Net);
    }

    #[test]
    fn resolve_at_var() {
        let db = LyraDatabase::default();
        let file = new_file(&db, 0, "module m; logic x; assign x = 0; endmodule");
        let unit = single_file_unit(&db, file);
        let text = file.text(&db);
        let x_pos = text.rfind("x =").expect("should find 'x ='");
        let result = resolve_at(&db, file, unit, lyra_source::TextSize::new(x_pos as u32));
        assert!(result.is_some(), "var 'x' should resolve");
        let sym =
            symbol_global(&db, unit, result.expect("checked above")).expect("symbol should exist");
        assert_eq!(sym.name.as_str(), "x");
        assert_eq!(sym.kind, lyra_semantic::symbols::SymbolKind::Variable);
    }

    #[test]
    fn unresolved_diag() {
        let db = LyraDatabase::default();
        let file = new_file(&db, 0, "module m; assign y = unknown_name; endmodule");
        let unit = single_file_unit(&db, file);
        let diags = file_diagnostics(&db, file, unit);
        let semantic_diags: Vec<_> = diags
            .iter()
            .filter(|d| d.render_message().contains("unresolved"))
            .collect();
        assert!(
            !semantic_diags.is_empty(),
            "should have unresolved name diagnostic"
        );
        assert!(
            semantic_diags
                .iter()
                .any(|d| d.render_message().contains("unknown_name")),
            "diagnostic should mention 'unknown_name': {semantic_diags:?}"
        );
    }

    #[test]
    fn duplicate_diag() {
        let db = LyraDatabase::default();
        let file = new_file(&db, 0, "module m; logic x; logic x; endmodule");
        let unit = single_file_unit(&db, file);
        let diags = file_diagnostics(&db, file, unit);
        let dup_diags: Vec<_> = diags
            .iter()
            .filter(|d| d.render_message().contains("duplicate"))
            .collect();
        assert!(
            !dup_diags.is_empty(),
            "should have duplicate definition diagnostic"
        );
    }

    #[test]
    fn block_scope() {
        let db = LyraDatabase::default();
        // Variable declared inside begin/end should not be visible outside
        let file = new_file(
            &db,
            0,
            "module m; always_comb begin logic inner; inner = 1; end assign y = inner; endmodule",
        );
        let unit = single_file_unit(&db, file);
        let diags = file_diagnostics(&db, file, unit);
        let unresolved: Vec<_> = diags
            .iter()
            .filter(|d| {
                let msg = d.render_message();
                msg.contains("unresolved") && msg.contains("inner")
            })
            .collect();
        assert!(
            !unresolved.is_empty(),
            "inner should not be visible outside the block: {diags:?}"
        );
    }

    #[test]
    fn shadowing() {
        let db = LyraDatabase::default();
        let src = "module m(input logic a); always_comb begin logic a; a = 1; end endmodule";
        let file = new_file(&db, 0, src);
        let unit = single_file_unit(&db, file);
        // The 'a' inside the block should resolve to the block-local declaration
        let text = file.text(&db);
        let a_in_block = text.find("a = 1").expect("should find 'a = 1'");
        let result = resolve_at(
            &db,
            file,
            unit,
            lyra_source::TextSize::new(a_in_block as u32),
        );
        assert!(result.is_some(), "inner 'a' should resolve");
        let sym =
            symbol_global(&db, unit, result.expect("checked above")).expect("symbol should exist");
        assert_eq!(sym.name.as_str(), "a");
        assert_eq!(sym.kind, lyra_semantic::symbols::SymbolKind::Variable);
        // Inner 'a' is a Variable, not a Port
    }

    #[test]
    fn multi_declarator() {
        let db = LyraDatabase::default();
        let file = new_file(&db, 0, "module m; wire a, b; assign a = b; endmodule");
        let unit = single_file_unit(&db, file);
        let text = file.text(&db);
        // Resolve 'a' in assign
        let a_pos = text.find("a = b").expect("should find 'a = b'");
        let result_a = resolve_at(&db, file, unit, lyra_source::TextSize::new(a_pos as u32));
        assert!(result_a.is_some(), "'a' should resolve");
        // Resolve 'b' in assign
        let b_pos = text.rfind('b').expect("should find 'b'");
        let result_b = resolve_at(&db, file, unit, lyra_source::TextSize::new(b_pos as u32));
        assert!(result_b.is_some(), "'b' should resolve");
    }

    #[test]
    fn module_not_in_lexical_scope() {
        let db = LyraDatabase::default();
        // Module name should be in exports, not resolvable as a lexical name
        let file = new_file(&db, 0, "module m; assign x = m; endmodule");
        let unit = single_file_unit(&db, file);
        let diags = file_diagnostics(&db, file, unit);
        let unresolved: Vec<_> = diags
            .iter()
            .filter(|d| {
                let msg = d.render_message();
                msg.contains("unresolved") && msg.contains("`m`")
            })
            .collect();
        assert!(
            !unresolved.is_empty(),
            "module name 'm' should not resolve as a lexical name: {diags:?}"
        );
        // But exports should contain it
        let def = def_index_file(&db, file);
        assert!(
            !def.exports.modules.is_empty(),
            "module 'm' should be in exports"
        );
    }

    // Incremental invalidation tests

    #[test]
    fn def_index_cached_when_text_unchanged() {
        let db = EventDb::new();
        let file = SourceFile::new(
            &db,
            lyra_source::FileId(0),
            "module m; logic x; endmodule".to_string(),
            IncludeMap::default(),
        );

        let _ = def_index_file(&db, file);
        db.take_log();

        let _ = def_index_file(&db, file);
        let log = db.take_log();
        assert!(
            !has_will_execute(&log),
            "def_index_file should be cached: {log:?}"
        );
    }

    #[test]
    fn def_index_recomputed_on_text_change() {
        let mut db = EventDb::new();
        let file = SourceFile::new(
            &db,
            lyra_source::FileId(0),
            "module m; logic x; endmodule".to_string(),
            IncludeMap::default(),
        );

        let _ = def_index_file(&db, file);
        db.take_log();

        update_file_text(&mut db, file, "module m; logic y; endmodule".to_string());
        let _ = def_index_file(&db, file);
        let log = db.take_log();
        assert!(
            has_will_execute(&log),
            "def_index_file should re-execute after text change: {log:?}"
        );
    }

    #[test]
    fn resolve_index_recomputed_on_text_change() {
        let mut db = EventDb::new();
        let file = SourceFile::new(
            &db,
            lyra_source::FileId(0),
            "module m; logic x; assign x = 0; endmodule".to_string(),
            IncludeMap::default(),
        );
        let unit = single_file_unit(&db, file);

        let _ = resolve_index_file(&db, file, unit);
        db.take_log();

        update_file_text(
            &mut db,
            file,
            "module m; logic y; assign y = 0; endmodule".to_string(),
        );
        let _ = resolve_index_file(&db, file, unit);
        let log = db.take_log();
        assert!(
            has_will_execute(&log),
            "resolve_index_file should re-execute after text change: {log:?}"
        );
    }

    #[test]
    fn whitespace_edit_skips_resolve_core() {
        let mut db = EventDb::new();
        let file = SourceFile::new(
            &db,
            lyra_source::FileId(0),
            "module m; logic x; assign x = 0; endmodule".to_string(),
            IncludeMap::default(),
        );
        let unit = single_file_unit(&db, file);

        // Prime cache and extract stable scalars before mutable borrow
        let res_count_before = resolve_index_file(&db, file, unit).resolutions.len();
        let diag_count_before = file_diagnostics(&db, file, unit).len();
        // Spot-check: 'x' in 'assign x = 0' at offset 25
        let x_sym_before = resolve_at(&db, file, unit, lyra_source::TextSize::new(25));
        db.take_log();

        // Whitespace-only edit: changes offsets but not names/scopes
        update_file_text(
            &mut db,
            file,
            "module  m;  logic  x;  assign  x  =  0;  endmodule".to_string(),
        );
        let res_count_after = resolve_index_file(&db, file, unit).resolutions.len();
        let log = db.take_log();

        // def_index_file re-executes (parse changed, offsets differ)
        assert!(
            log.iter().any(|e| e.contains("def_index_file")),
            "def_index_file should re-execute: {log:?}"
        );
        // name_graph_file re-executes but produces equal result -> backdated
        assert!(
            log.iter().any(|e| e.contains("name_graph_file")),
            "name_graph_file should re-execute: {log:?}"
        );
        // resolve_core_file should NOT re-execute (backdated input)
        assert!(
            !log.iter().any(|e| e.contains("resolve_core_file")),
            "resolve_core_file should be skipped (backdated): {log:?}"
        );

        // Functional equivalence via stable projections
        assert_eq!(
            res_count_before, res_count_after,
            "resolution count should match"
        );
        assert_eq!(
            diag_count_before,
            file_diagnostics(&db, file, unit).len(),
            "diagnostic count should match"
        );

        // Spot-check: 'x' in 'assign  x  =  0' resolves to same SymbolId
        let text = file.text(&db);
        let x_pos = text.rfind("x  =").expect("should find 'x  ='");
        let x_sym_after = resolve_at(&db, file, unit, lyra_source::TextSize::new(x_pos as u32));
        assert_eq!(
            x_sym_before.map(|g| g.local),
            x_sym_after.map(|g| g.local),
            "same SymbolId before and after whitespace edit"
        );
    }

    #[test]
    fn edit_unrelated_file_does_not_recompute_def_index() {
        let mut db = EventDb::new();
        let file_a = SourceFile::new(
            &db,
            lyra_source::FileId(0),
            "module a; logic x; endmodule".to_string(),
            IncludeMap::default(),
        );
        let file_b = SourceFile::new(
            &db,
            lyra_source::FileId(1),
            "module b; logic y; endmodule".to_string(),
            IncludeMap::default(),
        );

        let _ = def_index_file(&db, file_a);
        let _ = def_index_file(&db, file_b);
        db.take_log();

        update_file_text(&mut db, file_b, "module b; logic z; endmodule".to_string());

        let _ = def_index_file(&db, file_a);
        let log = db.take_log();
        assert!(
            !has_will_execute(&log),
            "editing file_b should not recompute file_a's def_index: {log:?}"
        );
    }

    // Structured diagnostic tests

    #[test]
    fn duplicate_diag_has_secondary_label() {
        let db = LyraDatabase::default();
        let file = new_file(&db, 0, "module m; logic x; logic x; endmodule");
        let unit = single_file_unit(&db, file);
        let diags = file_diagnostics(&db, file, unit);
        let dup = diags
            .iter()
            .find(|d| d.code == lyra_diag::DiagnosticCode::DUPLICATE_DEFINITION)
            .expect("should have duplicate diagnostic");
        assert_eq!(dup.labels.len(), 2, "primary + secondary labels");
        assert_eq!(dup.labels[0].kind, lyra_diag::LabelKind::Primary);
        assert_eq!(dup.labels[1].kind, lyra_diag::LabelKind::Secondary);
    }

    #[test]
    fn unresolved_diag_has_code() {
        let db = LyraDatabase::default();
        let file = new_file(&db, 0, "module m; assign y = unknown_name; endmodule");
        let unit = single_file_unit(&db, file);
        let diags = file_diagnostics(&db, file, unit);
        let unresolved = diags
            .iter()
            .find(|d| {
                d.code == lyra_diag::DiagnosticCode::UNRESOLVED_NAME
                    && d.render_message().contains("unknown_name")
            })
            .expect("should have unresolved name diagnostic for unknown_name");
        assert_eq!(unresolved.severity, lyra_diag::Severity::Error);
    }

    #[test]
    fn parse_error_has_code() {
        let db = LyraDatabase::default();
        let file = new_file(&db, 0, "module top;");
        let unit = single_file_unit(&db, file);
        let diags = file_diagnostics(&db, file, unit);
        assert!(
            diags
                .iter()
                .any(|d| d.code == lyra_diag::DiagnosticCode::PARSE_ERROR),
            "should have parse error diagnostic with PARSE_ERROR code"
        );
    }

    // Cross-file resolution tests

    #[test]
    fn cross_file_module_instantiation() {
        let db = LyraDatabase::default();
        let file_a = new_file(
            &db,
            0,
            "module adder(input logic a, input logic b, output logic sum); assign sum = a + b; endmodule",
        );
        let file_b = new_file(
            &db,
            1,
            "module top; logic x, y, s; adder u1(.a(x), .b(y), .sum(s)); endmodule",
        );
        let unit = new_compilation_unit(&db, vec![file_a, file_b]);

        // Cursor on 'adder' in file_b's instantiation
        let text_b = file_b.text(&db);
        let adder_pos = text_b.find("adder").expect("should find 'adder'");
        let result = resolve_at(
            &db,
            file_b,
            unit,
            lyra_source::TextSize::new(adder_pos as u32),
        );
        assert!(result.is_some(), "'adder' should resolve cross-file");
        let sym_id = result.expect("checked above");
        assert_eq!(
            sym_id.file,
            lyra_source::FileId(0),
            "should resolve to file_a"
        );
        let sym = symbol_global(&db, unit, sym_id).expect("symbol should exist");
        assert_eq!(sym.name.as_str(), "adder");
        assert_eq!(sym.kind, lyra_semantic::symbols::SymbolKind::Module);
    }

    #[test]
    fn unresolved_module_instantiation() {
        let db = LyraDatabase::default();
        let file = new_file(
            &db,
            0,
            "module top; logic x; nonexistent u1(.a(x)); endmodule",
        );
        let unit = single_file_unit(&db, file);
        let diags = file_diagnostics(&db, file, unit);
        let unresolved: Vec<_> = diags
            .iter()
            .filter(|d| {
                d.render_message().contains("unresolved")
                    && d.render_message().contains("nonexistent")
            })
            .collect();
        assert!(
            !unresolved.is_empty(),
            "should have unresolved name diagnostic for 'nonexistent': {diags:?}"
        );
    }

    #[test]
    fn no_lexical_fallback_for_definition_ns() {
        let db = LyraDatabase::default();
        // Local variable named 'adder' should NOT shadow a module instantiation
        let file = new_file(&db, 0, "module top; logic adder; adder u1(); endmodule");
        let unit = single_file_unit(&db, file);
        let diags = file_diagnostics(&db, file, unit);
        // The instantiation 'adder u1()' should be unresolved (no module 'adder' in unit)
        let unresolved: Vec<_> = diags
            .iter()
            .filter(|d| {
                let msg = d.render_message();
                msg.contains("unresolved") && msg.contains("adder")
            })
            .collect();
        assert!(
            !unresolved.is_empty(),
            "module instantiation should not fall back to lexical scope: {diags:?}"
        );
    }

    #[test]
    fn duplicate_module_definition() {
        let db = LyraDatabase::default();
        let file_a = new_file(&db, 0, "module foo; endmodule");
        let file_b = new_file(&db, 1, "module foo; endmodule");
        let unit = new_compilation_unit(&db, vec![file_a, file_b]);
        let diags = unit_diagnostics(&db, unit);
        let dup_diags: Vec<_> = diags
            .iter()
            .filter(|d| d.render_message().contains("duplicate definition"))
            .collect();
        assert!(
            !dup_diags.is_empty(),
            "should have duplicate definition diagnostic: {diags:?}"
        );
    }

    #[test]
    fn file_addition_triggers_resolution() {
        let db = LyraDatabase::default();
        // Initially, file_b has unresolved 'adder'
        let file_b = new_file(&db, 1, "module top; logic x; adder u1(.a(x)); endmodule");
        let unit = new_compilation_unit(&db, vec![file_b]);
        let diags = file_diagnostics(&db, file_b, unit);
        assert!(
            diags
                .iter()
                .any(|d| d.render_message().contains("unresolved")),
            "adder should be unresolved initially"
        );

        // Add file_a with module adder
        let file_a = new_file(&db, 0, "module adder(input logic a); endmodule");
        let unit2 = new_compilation_unit(&db, vec![file_a, file_b]);
        let diags2 = file_diagnostics(&db, file_b, unit2);
        let unresolved: Vec<_> = diags2
            .iter()
            .filter(|d| {
                let msg = d.render_message();
                msg.contains("unresolved") && msg.contains("adder")
            })
            .collect();
        assert!(
            unresolved.is_empty(),
            "adder should resolve after adding file_a: {diags2:?}"
        );
    }

    // Package resolution tests

    #[test]
    fn import_explicit_resolves_cross_file() {
        let db = LyraDatabase::default();
        let file_a = new_file(&db, 0, "package pkg; logic x; endpackage");
        let file_b = new_file(&db, 1, "module m; import pkg::x; assign y = x; endmodule");
        let unit = new_compilation_unit(&db, vec![file_a, file_b]);
        let text = file_b.text(&db);
        let x_pos = text.rfind("x;").expect("should find 'x;'");
        let result = resolve_at(&db, file_b, unit, lyra_source::TextSize::new(x_pos as u32));
        assert!(result.is_some(), "'x' should resolve via explicit import");
        let sym_id = result.expect("checked above");
        assert_eq!(
            sym_id.file,
            lyra_source::FileId(0),
            "should resolve to pkg file"
        );
        let sym = symbol_global(&db, unit, sym_id).expect("symbol should exist");
        assert_eq!(sym.name.as_str(), "x");
    }

    #[test]
    fn qualified_name_resolves_cross_file() {
        let db = LyraDatabase::default();
        let file_a = new_file(&db, 0, "package pkg; logic val; endpackage");
        let file_b = new_file(&db, 1, "module m; assign y = pkg::val; endmodule");
        let unit = new_compilation_unit(&db, vec![file_a, file_b]);
        // Cursor on 'val' in 'pkg::val'
        let text = file_b.text(&db);
        let val_pos = text.find("val").expect("should find 'val'");
        let result = resolve_at(
            &db,
            file_b,
            unit,
            lyra_source::TextSize::new(val_pos as u32),
        );
        assert!(result.is_some(), "'val' in pkg::val should resolve");
        let sym_id = result.expect("checked above");
        assert_eq!(sym_id.file, lyra_source::FileId(0));
        let sym = symbol_global(&db, unit, sym_id).expect("symbol should exist");
        assert_eq!(sym.name.as_str(), "val");
    }

    #[test]
    fn wildcard_import_resolves() {
        let db = LyraDatabase::default();
        let file_a = new_file(&db, 0, "package pkg; logic x; endpackage");
        let file_b = new_file(&db, 1, "module m; import pkg::*; assign y = x; endmodule");
        let unit = new_compilation_unit(&db, vec![file_a, file_b]);
        let text = file_b.text(&db);
        let x_pos = text.rfind("x;").expect("should find 'x;'");
        let result = resolve_at(&db, file_b, unit, lyra_source::TextSize::new(x_pos as u32));
        assert!(result.is_some(), "'x' should resolve via wildcard import");
        let sym_id = result.expect("checked above");
        assert_eq!(sym_id.file, lyra_source::FileId(0));
    }

    #[test]
    fn local_shadows_import() {
        let db = LyraDatabase::default();
        let file_a = new_file(&db, 0, "package pkg; logic x; endpackage");
        let file_b = new_file(
            &db,
            1,
            "module m; import pkg::*; logic x; assign y = x; endmodule",
        );
        let unit = new_compilation_unit(&db, vec![file_a, file_b]);
        let text = file_b.text(&db);
        let x_pos = text.rfind("x;").expect("should find 'x;'");
        let result = resolve_at(&db, file_b, unit, lyra_source::TextSize::new(x_pos as u32));
        assert!(result.is_some(), "'x' should resolve to local declaration");
        let sym_id = result.expect("checked above");
        // Local declaration is in file_b
        assert_eq!(
            sym_id.file,
            lyra_source::FileId(1),
            "local should shadow import"
        );
    }

    #[test]
    fn explicit_import_shadows_wildcard() {
        let db = LyraDatabase::default();
        let file_a = new_file(&db, 0, "package p1; logic x; endpackage");
        let file_b = new_file(&db, 1, "package p2; logic x; endpackage");
        let file_c = new_file(
            &db,
            2,
            "module m; import p1::*; import p2::x; assign y = x; endmodule",
        );
        let unit = new_compilation_unit(&db, vec![file_a, file_b, file_c]);
        let text = file_c.text(&db);
        let x_pos = text.rfind("x;").expect("should find 'x;'");
        let result = resolve_at(&db, file_c, unit, lyra_source::TextSize::new(x_pos as u32));
        assert!(result.is_some(), "'x' should resolve via explicit import");
        let sym_id = result.expect("checked above");
        assert_eq!(
            sym_id.file,
            lyra_source::FileId(1),
            "explicit import from p2 should shadow wildcard from p1"
        );
    }

    #[test]
    fn unresolved_import_package_not_found() {
        let db = LyraDatabase::default();
        let file = new_file(&db, 0, "module m; import nonexistent::x; endmodule");
        let unit = single_file_unit(&db, file);
        let diags = file_diagnostics(&db, file, unit);
        assert!(
            diags.iter().any(|d| {
                let msg = d.render_message();
                msg.contains("nonexistent") && msg.contains("not found")
            }),
            "should have package not found diagnostic: {diags:?}"
        );
    }

    #[test]
    fn import_member_not_found() {
        let db = LyraDatabase::default();
        let file_a = new_file(&db, 0, "package pkg; logic x; endpackage");
        let file_b = new_file(&db, 1, "module m; import pkg::z; endmodule");
        let unit = new_compilation_unit(&db, vec![file_a, file_b]);
        let diags = file_diagnostics(&db, file_b, unit);
        assert!(
            diags.iter().any(|d| {
                let msg = d.render_message();
                msg.contains("z") && msg.contains("not found")
            }),
            "should have member not found diagnostic: {diags:?}"
        );
    }

    #[test]
    fn wildcard_ambiguity() {
        let db = LyraDatabase::default();
        let file_a = new_file(&db, 0, "package p1; logic x; endpackage");
        let file_b = new_file(&db, 1, "package p2; logic x; endpackage");
        let file_c = new_file(
            &db,
            2,
            "module m; import p1::*; import p2::*; assign y = x; endmodule",
        );
        let unit = new_compilation_unit(&db, vec![file_a, file_b, file_c]);
        let diags = file_diagnostics(&db, file_c, unit);
        assert!(
            diags.iter().any(|d| {
                let msg = d.render_message();
                msg.contains("ambiguous") || msg.contains("x")
            }),
            "should have ambiguous import diagnostic: {diags:?}"
        );
    }

    #[test]
    fn module_package_name_collision() {
        let db = LyraDatabase::default();
        let file_a = new_file(&db, 0, "module foo; endmodule");
        let file_b = new_file(&db, 1, "package foo; endpackage");
        let unit = new_compilation_unit(&db, vec![file_a, file_b]);
        let diags = unit_diagnostics(&db, unit);
        assert!(
            diags
                .iter()
                .any(|d| d.render_message().contains("duplicate definition")),
            "module and package 'foo' should collide: {diags:?}"
        );
    }

    #[test]
    fn qualified_name_cursor_on_package() {
        let db = LyraDatabase::default();
        let file_a = new_file(&db, 0, "package pkg; logic val; endpackage");
        let file_b = new_file(&db, 1, "module m; assign y = pkg::val; endmodule");
        let unit = new_compilation_unit(&db, vec![file_a, file_b]);
        // Cursor on 'pkg' in 'pkg::val'
        let text = file_b.text(&db);
        let pkg_pos = text.find("pkg::").expect("should find 'pkg::'");
        let result = resolve_at(
            &db,
            file_b,
            unit,
            lyra_source::TextSize::new(pkg_pos as u32),
        );
        assert!(
            result.is_some(),
            "cursor on 'pkg' should resolve to package"
        );
        let sym_id = result.expect("checked above");
        assert_eq!(sym_id.file, lyra_source::FileId(0));
        let sym = symbol_global(&db, unit, sym_id).expect("symbol should exist");
        assert_eq!(sym.name.as_str(), "pkg");
        assert_eq!(sym.kind, lyra_semantic::symbols::SymbolKind::Package);
    }

    #[test]
    fn package_symbols_in_exports() {
        let db = LyraDatabase::default();
        let file = new_file(&db, 0, "package my_pkg; logic x; endpackage");
        let def = def_index_file(&db, file);
        assert!(
            !def.exports.packages.is_empty(),
            "package should be in exports"
        );
    }

    #[test]
    fn qualified_name_resolves_parameter() {
        let db = LyraDatabase::default();
        let file_a = new_file(&db, 0, "package pkg; parameter WIDTH = 8; endpackage");
        let file_b = new_file(&db, 1, "module m; assign y = pkg::WIDTH; endmodule");
        let unit = new_compilation_unit(&db, vec![file_a, file_b]);
        let text = file_b.text(&db);
        let pos = text.find("WIDTH").expect("should find 'WIDTH'");
        let result = resolve_at(&db, file_b, unit, lyra_source::TextSize::new(pos as u32));
        assert!(result.is_some(), "'WIDTH' in pkg::WIDTH should resolve");
        let sym_id = result.expect("checked above");
        assert_eq!(sym_id.file, lyra_source::FileId(0));
        let sym = symbol_global(&db, unit, sym_id).expect("symbol should exist");
        assert_eq!(sym.name.as_str(), "WIDTH");
    }

    // Typedef tests

    #[test]
    fn typedef_resolves_locally() {
        let db = LyraDatabase::default();
        let file = new_file(
            &db,
            0,
            "module m; typedef logic [7:0] byte_t; byte_t x; endmodule",
        );
        let unit = single_file_unit(&db, file);
        let diags = file_diagnostics(&db, file, unit);
        let unresolved: Vec<_> = diags
            .iter()
            .filter(|d| d.render_message().contains("unresolved"))
            .collect();
        assert!(
            unresolved.is_empty(),
            "byte_t should resolve locally: {diags:?}"
        );
    }

    #[test]
    fn typedef_value_type_coexist() {
        let db = LyraDatabase::default();
        let file = new_file(&db, 0, "module m; logic x; typedef int x; endmodule");
        let unit = single_file_unit(&db, file);
        let diags = file_diagnostics(&db, file, unit);
        let dup_diags: Vec<_> = diags
            .iter()
            .filter(|d| d.render_message().contains("duplicate"))
            .collect();
        assert!(
            dup_diags.is_empty(),
            "value 'x' and type 'x' should not conflict: {diags:?}"
        );
    }

    #[test]
    fn typedef_same_namespace_duplicate() {
        let db = LyraDatabase::default();
        let file = new_file(
            &db,
            0,
            "module m; typedef int t; typedef logic t; endmodule",
        );
        let unit = single_file_unit(&db, file);
        let diags = file_diagnostics(&db, file, unit);
        let dup_diags: Vec<_> = diags
            .iter()
            .filter(|d| d.render_message().contains("duplicate"))
            .collect();
        assert!(
            !dup_diags.is_empty(),
            "two typedefs with same name should be duplicate: {diags:?}"
        );
    }

    #[test]
    fn package_typedef_via_qualified_name() {
        let db = LyraDatabase::default();
        let file_a = new_file(
            &db,
            0,
            "package pkg; typedef logic [7:0] my_type; endpackage",
        );
        let file_b = new_file(&db, 1, "module m; typedef pkg::my_type local_t; endmodule");
        let unit = new_compilation_unit(&db, vec![file_a, file_b]);
        let diags = file_diagnostics(&db, file_b, unit);
        let unresolved: Vec<_> = diags
            .iter()
            .filter(|d| d.render_message().contains("unresolved"))
            .collect();
        assert!(
            unresolved.is_empty(),
            "pkg::my_type should resolve: {diags:?}"
        );
    }

    #[test]
    fn package_typedef_via_explicit_import() {
        let db = LyraDatabase::default();
        let file_a = new_file(
            &db,
            0,
            "package pkg; typedef logic [7:0] my_type; endpackage",
        );
        let file_b = new_file(
            &db,
            1,
            "module m; import pkg::my_type; my_type x; endmodule",
        );
        let unit = new_compilation_unit(&db, vec![file_a, file_b]);
        let diags = file_diagnostics(&db, file_b, unit);
        let unresolved: Vec<_> = diags
            .iter()
            .filter(|d| d.render_message().contains("unresolved"))
            .collect();
        assert!(
            unresolved.is_empty(),
            "my_type via explicit import should resolve: {diags:?}"
        );
    }

    #[test]
    fn package_typedef_via_wildcard_import() {
        let db = LyraDatabase::default();
        let file_a = new_file(
            &db,
            0,
            "package pkg; typedef logic [7:0] my_type; endpackage",
        );
        let file_b = new_file(&db, 1, "module m; import pkg::*; my_type x; endmodule");
        let unit = new_compilation_unit(&db, vec![file_a, file_b]);
        let diags = file_diagnostics(&db, file_b, unit);
        let unresolved: Vec<_> = diags
            .iter()
            .filter(|d| d.render_message().contains("unresolved"))
            .collect();
        assert!(
            unresolved.is_empty(),
            "my_type via wildcard import should resolve: {diags:?}"
        );
    }

    #[test]
    fn local_typedef_shadows_imported() {
        let db = LyraDatabase::default();
        let file_a = new_file(
            &db,
            0,
            "package pkg; typedef logic [7:0] my_type; endpackage",
        );
        let file_b = new_file(
            &db,
            1,
            "module m; import pkg::*; typedef int my_type; my_type x; endmodule",
        );
        let unit = new_compilation_unit(&db, vec![file_a, file_b]);
        // 'my_type' in 'my_type x' should resolve to file_b's local typedef
        let text = file_b.text(&db);
        let pos = text.rfind("my_type x").expect("should find 'my_type x'");
        let result = resolve_at(&db, file_b, unit, lyra_source::TextSize::new(pos as u32));
        assert!(result.is_some(), "my_type should resolve");
        let sym_id = result.expect("checked");
        assert_eq!(
            sym_id.file,
            lyra_source::FileId(1),
            "local typedef should shadow import"
        );
    }
}
