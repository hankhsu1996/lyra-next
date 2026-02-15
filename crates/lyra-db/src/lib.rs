use lyra_preprocess::{IncludeProvider, ResolvedInclude};
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

/// Convert parse and preprocess errors into diagnostics (Salsa-cached).
#[salsa::tracked(return_ref)]
pub fn file_diagnostics(db: &dyn salsa::Database, file: SourceFile) -> Vec<lyra_diag::Diagnostic> {
    let pp = preprocess_file(db, file);
    let parse = parse_file(db, file);

    let map_error = |range: lyra_source::TextRange, message: &str| -> lyra_diag::Diagnostic {
        if let Some(span) = pp.source_map.map_span(range) {
            lyra_diag::Diagnostic::error(span, message)
        } else {
            // Unmappable range: attach the file but use an empty span
            // at offset 0, and annotate the message so the mapping gap
            // is visible rather than silently hidden.
            let span = lyra_source::Span {
                file: file.file_id(db),
                range: lyra_source::TextRange::empty(lyra_source::TextSize::new(0)),
            };
            lyra_diag::Diagnostic::error(span, format!("{message} [unmapped range {range:?}]"))
        }
    };

    let mut diags: Vec<lyra_diag::Diagnostic> = pp
        .errors
        .iter()
        .map(|e| map_error(e.range, &e.message))
        .collect();

    diags.extend(parse.errors.iter().map(|e| map_error(e.range, &e.message)));

    diags
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
            assert_eq!(d.span.file, lyra_source::FileId(42));
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
}
