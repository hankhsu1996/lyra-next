mod const_eval;
mod elab;
mod exports;
mod expr_type;
mod resolve;
mod type_at;
mod type_diag;
mod type_of;

use lyra_ast::AstNode;

use super::*;

pub(super) fn new_file(db: &dyn salsa::Database, id: u32, text: &str) -> SourceFile {
    SourceFile::new(
        db,
        lyra_source::FileId(id),
        text.to_string(),
        IncludeMap::default(),
    )
}

/// Create a single-file compilation unit (convenience for tests).
pub(super) fn single_file_unit(db: &dyn salsa::Database, file: SourceFile) -> CompilationUnit {
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
    let range =
        lyra_source::TextRange::new(lyra_source::TextSize::new(0), lyra_source::TextSize::new(6));
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
            storage: salsa::Storage::default(),
            log: std::sync::Arc::default(),
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
