pub mod annotation;

use std::fmt::Write;
use std::path::Path;

use lyra_db::{
    CompilationUnit, IncludeMap, LyraDatabase, SourceFile, file_diagnostics, new_compilation_unit,
    parse_file, unit_diagnostics,
};
use lyra_diag::Severity;
use lyra_parser::{ParseError, SyntaxElement, SyntaxNode};
use lyra_source::{FileId, LineIndex};

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
        let file_id = FileId(self.files.len() as u32);
        let source = SourceFile::new(&self.db, file_id, text.to_owned(), IncludeMap::default());
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

    /// Access the underlying database.
    pub fn db(&self) -> &LyraDatabase {
        &self.db
    }

    /// The `SourceFile` at the given index.
    pub fn source_file(&self, index: usize) -> SourceFile {
        self.files[index].1
    }

    /// Source text of the file at the given index.
    pub fn file_text(&self, index: usize) -> &str {
        self.files[index].1.text(&self.db)
    }

    /// Path (name) of the file at the given index.
    pub fn file_path(&self, index: usize) -> &str {
        &self.files[index].0
    }

    /// Number of files.
    pub fn file_count(&self) -> usize {
        self.files.len()
    }

    /// Map a `FileId` back to the file's index in this workspace.
    pub fn file_index_for_id(&self, id: FileId) -> Option<usize> {
        self.files
            .iter()
            .position(|(_, s)| s.file_id(&self.db) == id)
    }

    /// Build a `CompilationUnit` from all files in the workspace.
    pub fn compilation_unit(&self) -> CompilationUnit {
        let sources: Vec<SourceFile> = self.files.iter().map(|(_, s)| *s).collect();
        new_compilation_unit(&self.db, sources)
    }

    /// Dump diagnostics summary for all files + unit diagnostics.
    ///
    /// Format:
    /// ```text
    /// // file: main.sv (2 diagnostics)
    ///   error[lyra.semantic[1]] 0:14..0:15: unresolved name `x`
    /// ---
    /// unit (0 diagnostics)
    /// ```
    pub fn dump_diagnostics(&self) -> String {
        let unit = self.compilation_unit();
        let mut out = String::new();

        // Collect and format per-file diagnostics
        for (i, (path, source)) in self.files.iter().enumerate() {
            let diags = file_diagnostics(&self.db, *source, unit);
            let line_index = LineIndex::new(self.file_text(i));
            let _ = writeln!(
                out,
                "// file: {path} ({} diagnostic{})",
                diags.len(),
                if diags.len() == 1 { "" } else { "s" }
            );

            let mut entries: Vec<_> = diags
                .iter()
                .filter_map(|d| {
                    let span = d.primary_span()?;
                    let start = line_index.line_col(span.range.start());
                    let end = line_index.line_col(span.range.end());
                    let msg = d.render_message();
                    let code = d.code.as_str();
                    let sev = severity_str(d.severity);
                    Some((
                        start.line,
                        start.col,
                        code.clone(),
                        sev,
                        msg.clone(),
                        format!(
                            "  {sev}[{code}] {}:{}..{}:{}: {msg}",
                            start.line, start.col, end.line, end.col,
                        ),
                    ))
                })
                .collect();
            entries.sort_by(|a, b| {
                a.0.cmp(&b.0)
                    .then_with(|| a.1.cmp(&b.1))
                    .then_with(|| a.2.cmp(&b.2))
                    .then_with(|| a.3.cmp(b.3))
                    .then_with(|| a.4.cmp(&b.4))
            });
            for (_, _, _, _, _, line) in &entries {
                let _ = writeln!(out, "{line}");
            }
        }

        out.push_str("---\n");

        // Unit diagnostics
        let unit_diags = unit_diagnostics(&self.db, unit);
        let _ = writeln!(
            out,
            "unit ({} diagnostic{})",
            unit_diags.len(),
            if unit_diags.len() == 1 { "" } else { "s" }
        );
        let mut unit_entries: Vec<_> = unit_diags
            .iter()
            .map(|d| {
                let code = d.code.as_str();
                let sev = severity_str(d.severity);
                let msg = d.render_message();
                // Unit diagnostics may have a span (for the duplicate location)
                let loc = d.primary_span().map(|span| {
                    let file_idx = self.file_index_for_id(span.file);
                    let file_path = file_idx.map_or("?", |i| self.file_path(i));
                    let li = file_idx.map(|i| LineIndex::new(self.file_text(i)));
                    if let Some(li) = li {
                        let start = li.line_col(span.range.start());
                        let end = li.line_col(span.range.end());
                        format!(
                            " {file_path} {}:{}..{}:{}",
                            start.line, start.col, end.line, end.col
                        )
                    } else {
                        String::new()
                    }
                });
                let loc = loc.unwrap_or_default();
                (
                    code.clone(),
                    sev,
                    msg.clone(),
                    format!("  {sev}[{code}]{loc}: {msg}"),
                )
            })
            .collect();
        unit_entries.sort_by(|a, b| {
            a.0.cmp(&b.0)
                .then_with(|| a.1.cmp(b.1))
                .then_with(|| a.2.cmp(&b.2))
        });
        for (_, _, _, line) in &unit_entries {
            let _ = writeln!(out, "{line}");
        }

        out
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

fn severity_str(sev: Severity) -> &'static str {
    match sev {
        Severity::Error => "error",
        Severity::Warning => "warning",
        Severity::Info => "info",
    }
}
