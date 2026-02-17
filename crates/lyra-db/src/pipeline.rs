use lyra_preprocess::{IncludeProvider, ResolvedInclude};
use salsa::Setter;

use crate::{IncludeMap, SourceFile};

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
