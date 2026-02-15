/// A source file input for the Salsa database.
#[salsa::input]
pub struct SourceFile {
    pub file_id: lyra_source::FileId,
    #[return_ref]
    pub text: String,
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
}
