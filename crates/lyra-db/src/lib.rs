/// A source file input for the Salsa database.
#[salsa::input]
pub struct SourceFile {
    pub file_id: lyra_source::FileId,
    #[return_ref]
    pub text: String,
}

/// Lex a source file into tokens (including trivia and EOF).
#[salsa::tracked(return_ref)]
pub fn lex_file(db: &dyn salsa::Database, file: SourceFile) -> Vec<lyra_lexer::Token> {
    lyra_lexer::lex(file.text(db))
}

/// Run the preprocessor over lexed tokens.
#[salsa::tracked(return_ref)]
pub fn preprocess_file(
    db: &dyn salsa::Database,
    file: SourceFile,
) -> lyra_preprocess::PreprocOutput {
    lyra_preprocess::preprocess(file.file_id(db), lex_file(db, file))
}

/// Parse a source file into a lossless green tree with diagnostics.
#[salsa::tracked(return_ref)]
pub fn parse_file(db: &dyn salsa::Database, file: SourceFile) -> lyra_parser::Parse {
    let pp = preprocess_file(db, file);
    lyra_parser::parse(&pp.tokens, file.text(db))
}

/// Access the source map for a preprocessed file.
pub fn source_map(db: &dyn salsa::Database, file: SourceFile) -> &lyra_preprocess::SourceMap {
    &preprocess_file(db, file).source_map
}

/// Access the include graph for a preprocessed file.
pub fn include_graph(db: &dyn salsa::Database, file: SourceFile) -> &lyra_preprocess::IncludeGraph {
    &preprocess_file(db, file).includes
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

/// Convert parse errors into diagnostics (Salsa-cached).
#[salsa::tracked(return_ref)]
pub fn file_diagnostics(db: &dyn salsa::Database, file: SourceFile) -> Vec<lyra_diag::Diagnostic> {
    let parse = parse_file(db, file);
    let file_id = file.file_id(db);
    parse
        .errors
        .iter()
        .map(|e| {
            lyra_diag::Diagnostic::error(
                lyra_source::Span {
                    file: file_id,
                    range: e.range,
                },
                &e.message,
            )
        })
        .collect()
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

    #[test]
    fn smoke_db_roundtrip() {
        let db = LyraDatabase::default();
        let file = SourceFile::new(
            &db,
            lyra_source::FileId(0),
            "module top; endmodule".to_string(),
        );
        assert_eq!(file.text(&db), "module top; endmodule");
    }

    #[test]
    fn end_to_end_parse() {
        let db = LyraDatabase::default();
        let file = SourceFile::new(
            &db,
            lyra_source::FileId(0),
            "module top; endmodule".to_string(),
        );
        let parse = parse_file(&db, file);
        assert!(parse.errors.is_empty());
        assert_eq!(parse.syntax().text().to_string(), "module top; endmodule");

        // Repeat call: sanity-check that a second invocation returns the same result.
        // Actual cache-hit assertions (event counting) belong in M2.
        let parse2 = parse_file(&db, file);
        assert!(parse2.errors.is_empty());
        assert_eq!(parse2.syntax().text().to_string(), "module top; endmodule");
    }

    #[test]
    fn ast_root_returns_typed_source_file() {
        let db = LyraDatabase::default();
        let file = SourceFile::new(
            &db,
            lyra_source::FileId(0),
            "module top; endmodule".to_string(),
        );
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
        let file = SourceFile::new(
            &db,
            lyra_source::FileId(0),
            "module top; endmodule".to_string(),
        );
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
        let file_a = SourceFile::new(
            &db,
            lyra_source::FileId(0),
            "module a; endmodule".to_string(),
        );
        let file_b = SourceFile::new(
            &db,
            lyra_source::FileId(1),
            "module b; endmodule".to_string(),
        );
        let root_b = ast_root(&db, file_b).expect("parser produces SourceFile root");
        let map_a = ast_id_map(&db, file_a);
        let map_b = ast_id_map(&db, file_b);
        let parse_a = parse_file(&db, file_a);

        // Get an id from map_b
        let module_b = root_b.modules().next().expect("file b has a module");
        let id_b = map_b.ast_id(&module_b).expect("module should have an id");
        // Resolving file_b's id against file_a's root via map_a returns None
        // because the id carries file_b's FileId which doesn't match map_a.
        assert!(map_a.get(&parse_a.syntax(), id_b).is_none());
    }

    #[test]
    fn source_map_identity() {
        let db = LyraDatabase::default();
        let fid = lyra_source::FileId(5);
        let file = SourceFile::new(&db, fid, "module m; endmodule".to_string());
        let sm = source_map(&db, file);
        let range = lyra_source::TextRange::new(
            lyra_source::TextSize::new(0),
            lyra_source::TextSize::new(6),
        );
        let span = sm.map_span(range);
        assert_eq!(span.file, fid);
        assert_eq!(span.range, range);
    }

    #[test]
    fn include_graph_empty() {
        let db = LyraDatabase::default();
        let file = SourceFile::new(
            &db,
            lyra_source::FileId(0),
            "module m; endmodule".to_string(),
        );
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
            "module top;".to_string(), // missing endmodule
        );
        let diags = file_diagnostics(&db, file);
        assert!(!diags.is_empty());
        // All diags should reference the correct file
        for d in diags {
            assert_eq!(d.span.file, lyra_source::FileId(42));
            assert_eq!(d.severity, lyra_diag::Severity::Error);
        }
    }
}
