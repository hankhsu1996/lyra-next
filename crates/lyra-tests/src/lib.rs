use std::fmt::Write;
use std::path::Path;

use lyra_db::{LyraDatabase, SourceFile, parse_file};
use lyra_parser::{ParseError, SyntaxElement, SyntaxNode};
use lyra_source::FileId;

/// A test workspace that holds multiple source files and provides
/// dump utilities for snapshot testing.
pub struct TestWorkspace {
    db: LyraDatabase,
    files: Vec<(String, SourceFile)>,
}

impl TestWorkspace {
    pub fn new() -> Self {
        Self {
            db: LyraDatabase::default(),
            files: Vec::new(),
        }
    }

    /// Add a source file to the workspace.
    pub fn add_file(&mut self, path: &str, text: &str) -> &mut Self {
        #[allow(clippy::cast_possible_truncation)]
        let file_id = FileId(self.files.len() as u32);
        let source = SourceFile::new(&self.db, file_id, text.to_owned());
        self.files.push((path.to_owned(), source));
        self
    }

    /// Build a workspace from all `.sv` files in a directory, sorted by name.
    pub fn from_dir(dir: &Path) -> Result<Self, std::io::Error> {
        let mut ws = Self::new();
        let mut entries: Vec<_> = std::fs::read_dir(dir)?
            .filter_map(|entry| {
                let entry = entry.ok()?;
                let path = entry.path();
                if path.extension().and_then(|e| e.to_str()) == Some("sv") {
                    Some(path)
                } else {
                    None
                }
            })
            .collect();
        entries.sort();

        for path in entries {
            let name = path.file_name().and_then(|n| n.to_str()).ok_or_else(|| {
                std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    format!("non-UTF-8 file name: {}", path.display()),
                )
            })?;
            let text = std::fs::read_to_string(&path)?;
            ws.add_file(name, &text);
        }
        Ok(ws)
    }

    /// Dump the parse tree and diagnostics for all files as a single
    /// deterministic string suitable for snapshot comparison.
    pub fn dump_parse(&self) -> String {
        let mut out = String::new();
        let mut all_errors: Vec<(u32, &ParseError)> = Vec::new();

        for (path, source) in &self.files {
            let parse = parse_file(&self.db, *source);

            let _ = writeln!(out, "// file: {path}");
            dump_tree(&parse.syntax(), 0, &mut out);

            for err in &parse.errors {
                all_errors.push((source.file_id(&self.db).0, err));
            }
        }

        out.push_str("---\n");

        if all_errors.is_empty() {
            out.push_str("no diagnostics\n");
        } else {
            // Sort by (file_id, offset, message) for stability
            all_errors.sort_by(|a, b| {
                a.0.cmp(&b.0)
                    .then_with(|| a.1.range.start().cmp(&b.1.range.start()))
                    .then_with(|| a.1.message.cmp(&b.1.message))
            });
            for (file_id, err) in &all_errors {
                dump_error(*file_id, err, &mut out);
            }
        }

        out
    }
}

impl Default for TestWorkspace {
    fn default() -> Self {
        Self::new()
    }
}

/// Print a syntax tree node with 2-space indentation per depth level.
///
/// Nodes: `Kind@Start..End`
/// Tokens: `Kind@Start..End "text"`
fn dump_tree(node: &SyntaxNode, depth: usize, out: &mut String) {
    let indent = "  ".repeat(depth);
    let range = node.text_range();
    let _ = writeln!(
        out,
        "{indent}{:?}@{}..{}",
        node.kind(),
        u32::from(range.start()),
        u32::from(range.end()),
    );

    for child in node.children_with_tokens() {
        match child {
            SyntaxElement::Node(n) => dump_tree(&n, depth + 1, out),
            SyntaxElement::Token(t) => {
                let r = t.text_range();
                let child_indent = "  ".repeat(depth + 1);
                let _ = writeln!(
                    out,
                    "{child_indent}{:?}@{}..{} {:?}",
                    t.kind(),
                    u32::from(r.start()),
                    u32::from(r.end()),
                    t.text(),
                );
            }
        }
    }
}

/// Format a single parse error line.
fn dump_error(file_id: u32, err: &ParseError, out: &mut String) {
    let _ = writeln!(
        out,
        "error[file={file_id} {}..{}]: {}",
        u32::from(err.range.start()),
        u32::from(err.range.end()),
        err.message,
    );
}
