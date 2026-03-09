mod include_loader;

use std::process::ExitCode;

fn main() -> ExitCode {
    let args: Vec<String> = std::env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: lyra-next <command> [args...]");
        eprintln!();
        eprintln!("Commands:");
        eprintln!(
            "  dump-tree <file>                              Print the CST of a SystemVerilog file"
        );
        eprintln!(
            "  check [--top <module>] [-I <dir>]... <file>... Run diagnostics on SystemVerilog files"
        );
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
        eprintln!("Usage: lyra-next dump-tree <file>");
        return ExitCode::FAILURE;
    };

    let source = match std::fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error reading {path}: {e}");
            return ExitCode::FAILURE;
        }
    };

    let tokens = lyra_lexer::lex_with_mode(&source, lyra_lexer::LexMode::Preprocess);
    let pp = lyra_preprocess::preprocess_identity(lyra_source::FileId(0), &tokens, &source);
    let parse = lyra_parser::parse(&pp.tokens, &pp.expanded_text);

    for err in &parse.errors {
        eprintln!("{err:?}");
    }

    print!("{:#?}", parse.syntax());

    ExitCode::SUCCESS
}

fn check(args: &[String]) -> ExitCode {
    let mut top_module: Option<&str> = None;
    let mut file_args: Vec<&str> = Vec::new();
    let mut include_dirs: Vec<String> = Vec::new();
    let mut i = 0;

    while i < args.len() {
        match args[i].as_str() {
            "--top" => {
                i += 1;
                if i >= args.len() {
                    eprintln!("Error: --top requires a module name argument");
                    return ExitCode::FAILURE;
                }
                top_module = Some(&args[i]);
            }
            "-I" | "--incdir" => {
                i += 1;
                if i >= args.len() {
                    eprintln!("Error: -I/--incdir requires a directory argument");
                    return ExitCode::FAILURE;
                }
                include_dirs.push(args[i].clone());
            }
            arg if arg.starts_with("-I") => {
                include_dirs.push(arg[2..].to_owned());
            }
            _ => {
                file_args.push(&args[i]);
            }
        }
        i += 1;
    }

    if file_args.is_empty() {
        eprintln!("Usage: lyra-next check [--top <module>] [-I <dir>]... <file>...");
        return ExitCode::FAILURE;
    }

    // Normalize root paths and read source text
    let mut root_paths: Vec<(String, String)> = Vec::new();
    for path in &file_args {
        let Some(norm) = include_loader::FsIncludeLoader::normalize_file_path(path) else {
            eprintln!("Error: cannot access {path}");
            return ExitCode::FAILURE;
        };
        let source = match std::fs::read_to_string(path) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("Error reading {path}: {e}");
                return ExitCode::FAILURE;
            }
        };
        root_paths.push((norm, source));
    }

    // Normalize include dirs
    let norm_inc_dirs: Vec<String> = include_dirs
        .iter()
        .filter_map(|d| {
            let p = std::path::Path::new(d);
            match p.canonicalize() {
                Ok(c) => Some(c.to_string_lossy().into_owned()),
                Err(e) => {
                    eprintln!("Warning: cannot access include directory {d}: {e}");
                    None
                }
            }
        })
        .collect();

    // Discover includes (pure algorithm, no DB)
    let loader = include_loader::FsIncludeLoader;
    let plan = lyra_db::include_resolve::discover(&root_paths, &norm_inc_dirs, &loader);

    // Apply the plan: create SourceFiles and set include maps
    let mut db = lyra_db::LyraDatabase::default();
    let source_files = lyra_db::include_resolve::apply_include_plan(&mut db, &plan, 0);

    // Root files for the compilation unit
    let root_files: Vec<lyra_db::SourceFile> = plan
        .files
        .iter()
        .enumerate()
        .filter(|(_, f)| f.is_root)
        .map(|(idx, _)| source_files[idx])
        .collect();

    let unit = lyra_db::new_compilation_unit(&db, root_files.clone());
    let all_diags = collect_diagnostics(
        &db,
        &file_args,
        &root_files,
        &source_files,
        unit,
        top_module,
    );
    print_diagnostics(&db, &all_diags, &source_files)
}

fn resolve_diag_path(
    db: &lyra_db::LyraDatabase,
    d: &lyra_diag::Diagnostic,
    all_files: &[lyra_db::SourceFile],
    display_paths: &[&str],
    root_files: &[lyra_db::SourceFile],
) -> String {
    d.primary_span()
        .and_then(|s| {
            root_files
                .iter()
                .position(|f| f.file_id(db) == s.file)
                .map(|i| display_paths[i].to_string())
                .or_else(|| {
                    all_files
                        .iter()
                        .find(|f| f.file_id(db) == s.file)
                        .map(|f| f.path(db).clone())
                })
        })
        .unwrap_or_else(|| "<unknown>".to_string())
}

fn collect_diagnostics(
    db: &lyra_db::LyraDatabase,
    display_paths: &[&str],
    root_files: &[lyra_db::SourceFile],
    all_files: &[lyra_db::SourceFile],
    unit: lyra_db::CompilationUnit,
    top_module: Option<&str>,
) -> Vec<(String, lyra_diag::Diagnostic)> {
    let mut all_diags: Vec<(String, lyra_diag::Diagnostic)> = Vec::new();

    for (idx, file) in root_files.iter().enumerate() {
        let diags = lyra_db::file_diagnostics(db, *file, unit);
        let path = display_paths[idx];
        for d in diags {
            all_diags.push((path.to_string(), d.clone()));
        }
    }

    let unit_diags = lyra_db::unit_diagnostics(db, unit);
    for d in unit_diags {
        let file_path = resolve_diag_path(db, d, all_files, display_paths, root_files);
        all_diags.push((file_path, d.clone()));
    }

    if let Some(top_name) = top_module {
        let top = lyra_db::TopModule::new(db, unit, smol_str::SmolStr::new(top_name));
        let elab_diags = lyra_db::elab_diagnostics(db, top);
        for d in elab_diags {
            let file_path = resolve_diag_path(db, d, all_files, display_paths, root_files);
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
    all_files: &[lyra_db::SourceFile],
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
            let line_col = all_files
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
