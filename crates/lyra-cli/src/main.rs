use std::process::ExitCode;

fn main() -> ExitCode {
    let args: Vec<String> = std::env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: lyra <command> [args...]");
        eprintln!();
        eprintln!("Commands:");
        eprintln!("  dump-tree <file>    Print the CST of a SystemVerilog file");
        return ExitCode::FAILURE;
    }

    match args[1].as_str() {
        "dump-tree" => dump_tree(&args[2..]),
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
