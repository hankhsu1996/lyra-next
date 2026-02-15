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
pub fn preprocess_file(db: &dyn salsa::Database, file: SourceFile) -> Vec<lyra_lexer::Token> {
    lyra_preprocess::preprocess(lex_file(db, file))
}

/// Parse a source file into a lossless green tree with diagnostics.
#[salsa::tracked(return_ref)]
pub fn parse_file(db: &dyn salsa::Database, file: SourceFile) -> lyra_parser::Parse {
    let tokens = preprocess_file(db, file);
    lyra_parser::parse(tokens, file.text(db))
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
}
