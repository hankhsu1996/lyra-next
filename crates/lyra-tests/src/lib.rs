pub mod annotation;
mod include_loader;
mod resolve;

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
///
/// `files` contains root source files (used for compilation unit,
/// per-file diagnostics, and annotation matching). `all_source_files`
/// contains every loaded file including discovered includes (used for
/// FileId-based lookup of diagnostic locations).
pub struct TestWorkspace {
    db: LyraDatabase,
    files: Vec<(String, SourceFile)>,
    all_source_files: Vec<(String, SourceFile)>,
}

impl TestWorkspace {
    pub fn new() -> Self {
        Self {
            db: LyraDatabase::default(),
            files: Vec::new(),
            all_source_files: Vec::new(),
        }
    }

    /// Add a source file to the workspace.
    pub fn add_file(&mut self, path: &str, text: &str) -> &mut Self {
        let file_id = FileId(self.all_source_files.len() as u32);
        let source = SourceFile::new(
            &self.db,
            file_id,
            path.to_owned(),
            text.to_owned(),
            IncludeMap::default(),
        );
        self.files.push((path.to_owned(), source));
        self.all_source_files.push((path.to_owned(), source));
        self
    }

    /// Build a workspace from all `.sv` and `.svh` files in a directory,
    /// sorted by name, with include resolution via the shared resolver.
    ///
    /// Root files (`.sv`) are used for the compilation unit. All files
    /// (roots + discovered includes) are available for `FileId` lookup.
    /// Paths are workspace-relative (relative to `dir`).
    pub fn from_dir(dir: &Path) -> Result<Self, std::io::Error> {
        let ws = Self::new();
        Self::from_dir_inner(ws, dir)
    }

    fn from_dir_inner(mut ws: Self, dir: &Path) -> Result<Self, std::io::Error> {
        let mut entries = Vec::new();
        collect_sv_files(dir, &mut entries)?;
        entries.sort();

        // Build workspace-relative paths and collect file contents
        let mut root_paths: Vec<(String, String)> = Vec::new();
        let mut all_fixture_files: Vec<(String, String)> = Vec::new();

        for path in &entries {
            let rel_path = path
                .strip_prefix(dir)
                .unwrap_or(path)
                .to_string_lossy()
                .into_owned();
            let text = std::fs::read_to_string(path)?;
            all_fixture_files.push((rel_path.clone(), text.clone()));

            let ext = path.extension().and_then(|e| e.to_str()).unwrap_or("");
            if ext == "sv" {
                root_paths.push((rel_path, text));
            }
        }

        // Run pure include discovery and apply to database
        let loader = include_loader::FixtureIncludeLoader::new(&all_fixture_files);
        let plan = lyra_db::include_resolve::discover(&root_paths, &[], &loader);
        let source_files = lyra_db::include_resolve::apply_include_plan(&mut ws.db, &plan, 0);

        // Track all files for FileId lookup
        for (idx, f) in plan.files.iter().enumerate() {
            ws.all_source_files
                .push((f.path.clone(), source_files[idx]));
        }

        // Track root files for compilation unit and per-file diagnostics
        for (idx, f) in plan.files.iter().enumerate() {
            if f.is_root {
                ws.files.push((f.path.clone(), source_files[idx]));
            }
        }

        Ok(ws)
    }

    /// Access the underlying database.
    pub fn db(&self) -> &LyraDatabase {
        &self.db
    }

    /// The `SourceFile` at the given index (root files only).
    pub fn source_file(&self, index: usize) -> SourceFile {
        self.files[index].1
    }

    /// Source text of the file at the given index (root files only).
    pub fn file_text(&self, index: usize) -> &str {
        self.files[index].1.text(&self.db)
    }

    /// Path (name) of the file at the given index (root files only).
    pub fn file_path(&self, index: usize) -> &str {
        &self.files[index].0
    }

    /// Number of root files.
    pub fn file_count(&self) -> usize {
        self.files.len()
    }

    /// Map a `FileId` back to the root file index. Returns `None` for
    /// include-only files. Used by annotation matching which only
    /// targets root files.
    pub fn file_index_for_id(&self, id: FileId) -> Option<usize> {
        self.files
            .iter()
            .position(|(_, s)| s.file_id(&self.db) == id)
    }

    /// Look up any file (root or include) by `FileId`. Returns
    /// `(path, source_text)`.
    fn lookup_any_file(&self, id: FileId) -> Option<(&str, &str)> {
        self.all_source_files
            .iter()
            .find(|(_, s)| s.file_id(&self.db) == id)
            .map(|(path, sf)| (path.as_str(), sf.text(&self.db).as_str()))
    }

    /// Build a `CompilationUnit` from root files.
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
                    let code = d.code;
                    let sev = severity_str(d.severity);
                    Some((
                        start.line,
                        start.col,
                        code,
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
                    .then_with(|| a.2.cmp(b.2))
                    .then_with(|| a.3.cmp(b.3))
                    .then_with(|| a.4.cmp(&b.4))
            });
            for (_, _, _, _, _, line) in &entries {
                let _ = writeln!(out, "{line}");
            }
        }

        out.push_str("---\n");

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
                let code = d.code;
                let sev = severity_str(d.severity);
                let msg = d.render_message();
                let loc = d.primary_span().map(|span| {
                    if let Some((file_path, file_text)) = self.lookup_any_file(span.file) {
                        let li = LineIndex::new(file_text);
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
                    code,
                    sev,
                    msg.clone(),
                    format!("  {sev}[{code}]{loc}: {msg}"),
                )
            })
            .collect();
        unit_entries.sort_by(|a, b| {
            a.0.cmp(b.0)
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

/// Recursively collect `.sv` and `.svh` files from a directory tree.
fn collect_sv_files(dir: &Path, out: &mut Vec<std::path::PathBuf>) -> Result<(), std::io::Error> {
    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            collect_sv_files(&path, out)?;
        } else if path
            .extension()
            .and_then(|e| e.to_str())
            .is_some_and(|ext| ext == "sv" || ext == "svh")
        {
            out.push(path);
        }
    }
    Ok(())
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
