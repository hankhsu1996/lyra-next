use std::process::ExitCode;

fn main() -> ExitCode {
    let args: Vec<String> = std::env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: lyra <command> [args...]");
        eprintln!();
        eprintln!("Commands:");
        eprintln!("  dump-tree <file>                 Print the CST of a SystemVerilog file");
        eprintln!("  check [--top <module>] <file>... Run diagnostics on SystemVerilog files");
        return ExitCode::FAILURE;
    }

    match args[1].as_str() {
        "dump-tree" => dump_tree(&args[2..]),
        "check" => check(&args[2..]),
        other => {
            eprintln!("Unknown command: {other}");
            ExitCode::FAILURE
        }
    }
}

fn dump_tree(args: &[String]) -> ExitCode {
    let Some(path) = args.first() else {
        eprintln!("Usage: lyra dump-tree <file>");
        return ExitCode::FAILURE;
    };

    let source = match std::fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error reading {path}: {e}");
            return ExitCode::FAILURE;
        }
    };

    let tokens = lyra_lexer::lex(&source);
    let pp = lyra_preprocess::preprocess_identity(lyra_source::FileId(0), &tokens, &source);
    let parse = lyra_parser::parse(&pp.tokens, &pp.expanded_text);

    // Print parse errors if any
    for err in &parse.errors {
        eprintln!("{err:?}");
    }

    // Print the tree
    print!("{:#?}", parse.syntax());

    ExitCode::SUCCESS
}

fn check(args: &[String]) -> ExitCode {
    let mut top_module: Option<&str> = None;
    let mut files: Vec<&str> = Vec::new();
    let mut i = 0;

    while i < args.len() {
        if args[i] == "--top" {
            i += 1;
            if i >= args.len() {
                eprintln!("Error: --top requires a module name argument");
                return ExitCode::FAILURE;
            }
            top_module = Some(&args[i]);
        } else {
            files.push(&args[i]);
        }
        i += 1;
    }

    if files.is_empty() {
        eprintln!("Usage: lyra check [--top <module>] <file>...");
        return ExitCode::FAILURE;
    }

    let db = lyra_db::LyraDatabase::default();
    let mut source_files = Vec::new();

    for (idx, path) in files.iter().enumerate() {
        let source = match std::fs::read_to_string(path) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("Error reading {path}: {e}");
                return ExitCode::FAILURE;
            }
        };
        let file = lyra_db::SourceFile::new(
            &db,
            lyra_source::FileId(idx as u32),
            source,
            lyra_db::IncludeMap::default(),
        );
        source_files.push(file);
    }

    let unit = lyra_db::new_compilation_unit(&db, source_files.clone());
    let all_diags = collect_diagnostics(&db, &files, &source_files, unit, top_module);
    print_diagnostics(&db, &all_diags, &source_files)
}

fn resolve_diag_path(
    db: &lyra_db::LyraDatabase,
    d: &lyra_diag::Diagnostic,
    source_files: &[lyra_db::SourceFile],
    files: &[&str],
) -> String {
    d.primary_span()
        .and_then(|s| {
            source_files
                .iter()
                .position(|f| f.file_id(db) == s.file)
                .map(|i| files[i].to_string())
        })
        .unwrap_or_else(|| "<unknown>".to_string())
}

fn collect_diagnostics(
    db: &lyra_db::LyraDatabase,
    files: &[&str],
    source_files: &[lyra_db::SourceFile],
    unit: lyra_db::CompilationUnit,
    top_module: Option<&str>,
) -> Vec<(String, lyra_diag::Diagnostic)> {
    let mut all_diags: Vec<(String, lyra_diag::Diagnostic)> = Vec::new();

    for (idx, file) in source_files.iter().enumerate() {
        let diags = lyra_db::file_diagnostics(db, *file, unit);
        let path = files[idx];
        for d in diags {
            all_diags.push((path.to_string(), d.clone()));
        }
    }

    let unit_diags = lyra_db::unit_diagnostics(db, unit);
    for d in unit_diags {
        let file_path = resolve_diag_path(db, d, source_files, files);
        all_diags.push((file_path, d.clone()));
    }

    if let Some(top_name) = top_module {
        let top = lyra_db::TopModule::new(db, unit, smol_str::SmolStr::new(top_name));
        let elab_diags = lyra_db::elab_diagnostics(db, top);
        for d in elab_diags {
            let file_path = resolve_diag_path(db, d, source_files, files);
            all_diags.push((file_path, d.clone()));
        }
    }

    all_diags.sort_by(|(path_a, a), (path_b, b)| {
        path_a.cmp(path_b).then_with(|| {
            let start_a = a
                .primary_span()
                .map(|s| s.range.start())
                .unwrap_or_default();
            let start_b = b
                .primary_span()
                .map(|s| s.range.start())
                .unwrap_or_default();
            start_a.cmp(&start_b)
        })
    });

    all_diags
}

fn print_diagnostics(
    db: &lyra_db::LyraDatabase,
    all_diags: &[(String, lyra_diag::Diagnostic)],
    source_files: &[lyra_db::SourceFile],
) -> ExitCode {
    let mut has_errors = false;
    for (path, d) in all_diags {
        let severity_str = match d.severity {
            lyra_diag::Severity::Error => {
                has_errors = true;
                "error"
            }
            lyra_diag::Severity::Warning => "warning",
            lyra_diag::Severity::Info => "info",
        };

        let msg = d.render_message();
        let code = d.code;

        if let Some(span) = d.primary_span() {
            let line_col = source_files
                .iter()
                .find(|f| f.file_id(db) == span.file)
                .map(|f| {
                    let li = lyra_db::line_index(db, *f);
                    li.line_col(span.range.start())
                });

            if let Some(lc) = line_col {
                eprintln!(
                    "{path}:{}:{}: {severity_str}[{code}]: {msg}",
                    lc.line + 1,
                    lc.col + 1,
                );
            } else {
                eprintln!("{path}: {severity_str}[{code}]: {msg}");
            }
        } else {
            eprintln!("{path}: {severity_str}[{code}]: {msg}");
        }
    }

    if has_errors {
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}
