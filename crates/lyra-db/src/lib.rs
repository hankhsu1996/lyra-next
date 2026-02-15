mod lower_diag;

use lyra_preprocess::{IncludeProvider, ResolvedInclude};
use lyra_semantic::def_index::DefIndex;
use lyra_semantic::name_graph::NameGraph;
use lyra_semantic::resolve_index::{Resolution, ResolveIndex};
use lyra_semantic::symbols::GlobalSymbolId;
use salsa::Setter;

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

/// Resolve all use-sites using only offset-independent data (Salsa-cached).
///
/// Depends on `name_graph_file`. When the name graph is backdated (e.g.
/// whitespace edit), this query is NOT re-executed.
#[salsa::tracked(return_ref)]
pub fn resolve_core_file(db: &dyn salsa::Database, file: SourceFile) -> Box<[Option<Resolution>]> {
    lyra_semantic::build_resolve_core(name_graph_file(db, file))
}

/// Build per-file resolution index (Salsa-cached).
///
/// Combines offset-independent resolve results from `resolve_core_file`
/// with offset-dependent data from `def_index_file` (`ast_ids`, ranges)
/// to produce the final `HashMap` and diagnostics. Trivially cheap.
#[salsa::tracked(return_ref)]
pub fn resolve_index_file(db: &dyn salsa::Database, file: SourceFile) -> ResolveIndex {
    let def = def_index_file(db, file);
    let core = resolve_core_file(db, file);
    lyra_semantic::build_resolve_index(def, core)
}

/// Resolve the name at a cursor position.
///
/// Finds the nearest `NameRef` at `offset`, looks up its `AstId`,
/// and returns the resolved `GlobalSymbolId` if found.
pub fn resolve_at(
    db: &dyn salsa::Database,
    file: SourceFile,
    offset: lyra_source::TextSize,
) -> Option<GlobalSymbolId> {
    let parse = parse_file(db, file);
    let ast_map = ast_id_map(db, file);
    let name_ref = find_name_ref_at(&parse.syntax(), offset)?;
    let ast_id = ast_map.ast_id(&name_ref)?.erase();
    let resolve = resolve_index_file(db, file);
    let resolution = resolve.resolutions.get(&ast_id)?;
    Some(GlobalSymbolId {
        file: file.file_id(db),
        local: resolution.symbol,
    })
}

/// Look up a symbol by its global id.
pub fn symbol_by_id(
    db: &dyn salsa::Database,
    file: SourceFile,
    id: GlobalSymbolId,
) -> &lyra_semantic::symbols::Symbol {
    def_index_file(db, file).symbols.get(id.local)
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

/// Convert parse, preprocess, and semantic errors into structured diagnostics (Salsa-cached).
#[salsa::tracked(return_ref)]
pub fn file_diagnostics(db: &dyn salsa::Database, file: SourceFile) -> Vec<lyra_diag::Diagnostic> {
    let pp = preprocess_file(db, file);
    let parse = parse_file(db, file);
    let def = def_index_file(db, file);
    let resolve = resolve_index_file(db, file);
    lower_diag::lower_file_diagnostics(file.file_id(db), pp, parse, def, resolve)
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
        let diags = file_diagnostics(&db, file);
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
        // 'a' in 'assign x = a' is at offset 36
        let result = resolve_at(&db, file, lyra_source::TextSize::new(36));
        assert!(result.is_some(), "port 'a' should resolve");
        let sym = symbol_by_id(&db, file, result.expect("checked above"));
        assert_eq!(sym.name.as_str(), "a");
        assert_eq!(sym.kind, lyra_semantic::symbols::SymbolKind::Port);
    }

    #[test]
    fn resolve_at_net() {
        let db = LyraDatabase::default();
        let file = new_file(&db, 0, "module m; wire w; assign w = 1; endmodule");
        // 'w' in 'assign w = 1' -- find offset of the second 'w'
        let text = file.text(&db);
        let w_pos = text.rfind("w =").expect("should find 'w ='");
        let result = resolve_at(&db, file, lyra_source::TextSize::new(w_pos as u32));
        assert!(result.is_some(), "net 'w' should resolve");
        let sym = symbol_by_id(&db, file, result.expect("checked above"));
        assert_eq!(sym.name.as_str(), "w");
        assert_eq!(sym.kind, lyra_semantic::symbols::SymbolKind::Net);
    }

    #[test]
    fn resolve_at_var() {
        let db = LyraDatabase::default();
        let file = new_file(&db, 0, "module m; logic x; assign x = 0; endmodule");
        let text = file.text(&db);
        let x_pos = text.rfind("x =").expect("should find 'x ='");
        let result = resolve_at(&db, file, lyra_source::TextSize::new(x_pos as u32));
        assert!(result.is_some(), "var 'x' should resolve");
        let sym = symbol_by_id(&db, file, result.expect("checked above"));
        assert_eq!(sym.name.as_str(), "x");
        assert_eq!(sym.kind, lyra_semantic::symbols::SymbolKind::Variable);
    }

    #[test]
    fn unresolved_diag() {
        let db = LyraDatabase::default();
        let file = new_file(&db, 0, "module m; assign y = unknown_name; endmodule");
        let diags = file_diagnostics(&db, file);
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
        let diags = file_diagnostics(&db, file);
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
        let diags = file_diagnostics(&db, file);
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
        // The 'a' inside the block should resolve to the block-local declaration
        let text = file.text(&db);
        let a_in_block = text.find("a = 1").expect("should find 'a = 1'");
        let result = resolve_at(&db, file, lyra_source::TextSize::new(a_in_block as u32));
        assert!(result.is_some(), "inner 'a' should resolve");
        let sym = symbol_by_id(&db, file, result.expect("checked above"));
        assert_eq!(sym.name.as_str(), "a");
        assert_eq!(sym.kind, lyra_semantic::symbols::SymbolKind::Variable);
        // Inner 'a' is a Variable, not a Port
    }

    #[test]
    fn multi_declarator() {
        let db = LyraDatabase::default();
        let file = new_file(&db, 0, "module m; wire a, b; assign a = b; endmodule");
        let text = file.text(&db);
        // Resolve 'a' in assign
        let a_pos = text.find("a = b").expect("should find 'a = b'");
        let result_a = resolve_at(&db, file, lyra_source::TextSize::new(a_pos as u32));
        assert!(result_a.is_some(), "'a' should resolve");
        // Resolve 'b' in assign
        let b_pos = text.rfind('b').expect("should find 'b'");
        let result_b = resolve_at(&db, file, lyra_source::TextSize::new(b_pos as u32));
        assert!(result_b.is_some(), "'b' should resolve");
    }

    #[test]
    fn module_not_in_lexical_scope() {
        let db = LyraDatabase::default();
        // Module name should be in exports, not resolvable as a lexical name
        let file = new_file(&db, 0, "module m; assign x = m; endmodule");
        let diags = file_diagnostics(&db, file);
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

        let _ = resolve_index_file(&db, file);
        db.take_log();

        update_file_text(
            &mut db,
            file,
            "module m; logic y; assign y = 0; endmodule".to_string(),
        );
        let _ = resolve_index_file(&db, file);
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

        // Prime cache and extract stable scalars before mutable borrow
        let res_count_before = resolve_index_file(&db, file).resolutions.len();
        let diag_count_before = file_diagnostics(&db, file).len();
        // Spot-check: 'x' in 'assign x = 0' at offset 25
        let x_sym_before = resolve_at(&db, file, lyra_source::TextSize::new(25));
        db.take_log();

        // Whitespace-only edit: changes offsets but not names/scopes
        update_file_text(
            &mut db,
            file,
            "module  m;  logic  x;  assign  x  =  0;  endmodule".to_string(),
        );
        let res_count_after = resolve_index_file(&db, file).resolutions.len();
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
            file_diagnostics(&db, file).len(),
            "diagnostic count should match"
        );

        // Spot-check: 'x' in 'assign  x  =  0' resolves to same SymbolId
        let text = file.text(&db);
        let x_pos = text.rfind("x  =").expect("should find 'x  ='");
        let x_sym_after = resolve_at(&db, file, lyra_source::TextSize::new(x_pos as u32));
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
        let diags = file_diagnostics(&db, file);
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
        let diags = file_diagnostics(&db, file);
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
        let diags = file_diagnostics(&db, file);
        assert!(
            diags
                .iter()
                .any(|d| d.code == lyra_diag::DiagnosticCode::PARSE_ERROR),
            "should have parse error diagnostic with PARSE_ERROR code"
        );
    }
}
