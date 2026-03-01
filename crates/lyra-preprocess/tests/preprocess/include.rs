use std::collections::HashMap;

use lyra_lexer::SyntaxKind;
use lyra_preprocess::{
    IncludeProvider, MacroEnv, PreprocessInputs, ResolvedInclude, preprocess, scan_includes,
};
use lyra_source::{ExpansionKind, FileId, TextRange, TextSize};

struct MapProvider {
    files: HashMap<String, (FileId, String, Vec<lyra_lexer::Token>)>,
}

impl MapProvider {
    fn new() -> Self {
        Self {
            files: HashMap::new(),
        }
    }

    fn add(&mut self, path: &str, file_id: FileId, text: &str) {
        let tokens = lyra_lexer::lex(text);
        self.files
            .insert(path.to_string(), (file_id, text.to_string(), tokens));
    }
}

impl IncludeProvider for MapProvider {
    fn resolve(&self, path: &str) -> Option<ResolvedInclude<'_>> {
        let (file_id, text, tokens) = self.files.get(path)?;
        Some(ResolvedInclude {
            file_id: *file_id,
            tokens,
            text,
        })
    }
}

fn run_preprocess(
    file: FileId,
    tokens: &[lyra_lexer::Token],
    text: &str,
    provider: &dyn IncludeProvider,
) -> lyra_preprocess::PreprocOutput {
    let env = MacroEnv::empty();
    preprocess(&PreprocessInputs {
        file,
        tokens,
        text,
        provider,
        starting_env: &env,
    })
}

#[test]
fn include_splices_tokens() {
    let mut provider = MapProvider::new();
    provider.add("b.sv", FileId(1), "wire w;");

    let text = "module top; `include \"b.sv\"\nendmodule";
    let tokens = lyra_lexer::lex(text);
    let output = run_preprocess(FileId(0), &tokens, text, &provider);

    let has_wire = output.tokens.iter().any(|t| t.kind == SyntaxKind::WireKw);
    assert!(has_wire, "included wire keyword should be present");

    let has_directive = output
        .tokens
        .iter()
        .any(|t| t.kind == SyntaxKind::Directive);
    assert!(!has_directive, "include directive should be removed");
}

#[test]
fn expanded_text_matches_concatenation() {
    let mut provider = MapProvider::new();
    provider.add("b.sv", FileId(1), "wire w;");

    let text = "module top; `include \"b.sv\"\nendmodule";
    let tokens = lyra_lexer::lex(text);
    let output = run_preprocess(FileId(0), &tokens, text, &provider);

    assert_eq!(output.expanded_text, "module top; wire w;\nendmodule");
}

#[test]
fn token_lengths_sum_to_expanded_text() {
    let mut provider = MapProvider::new();
    provider.add("b.sv", FileId(1), "wire w;");

    let text = "module top; `include \"b.sv\"\nendmodule";
    let tokens = lyra_lexer::lex(text);
    let output = run_preprocess(FileId(0), &tokens, text, &provider);

    let total: u32 = output
        .tokens
        .iter()
        .filter(|t| t.kind != SyntaxKind::Eof)
        .map(|t| u32::from(t.len))
        .sum();
    assert_eq!(total as usize, output.expanded_text.len());
}

#[test]
fn map_point_in_included_range() {
    let mut provider = MapProvider::new();
    provider.add("b.sv", FileId(1), "wire w;");

    let text = "module top; `include \"b.sv\"\nendmodule";
    let tokens = lyra_lexer::lex(text);
    let output = run_preprocess(FileId(0), &tokens, text, &provider);

    // "module top; " is 12 bytes, so included content starts at offset 12
    let span = output
        .source_map
        .map_point(TextSize::new(12))
        .expect("in-bounds offset should map");
    assert_eq!(
        span.file,
        FileId(1),
        "offset in included range maps to included file"
    );
    assert_eq!(span.range.start(), TextSize::new(0));
}

#[test]
fn map_point_in_main_file() {
    let mut provider = MapProvider::new();
    provider.add("b.sv", FileId(1), "wire w;");

    let text = "module top; `include \"b.sv\"\nendmodule";
    let tokens = lyra_lexer::lex(text);
    let output = run_preprocess(FileId(0), &tokens, text, &provider);

    let span = output
        .source_map
        .map_point(TextSize::new(0))
        .expect("in-bounds offset should map");
    assert_eq!(span.file, FileId(0));
    assert_eq!(span.range.start(), TextSize::new(0));
}

#[test]
fn map_point_after_include_maps_correctly() {
    let mut provider = MapProvider::new();
    provider.add("b.sv", FileId(1), "wire w;");

    let text = "module top; `include \"b.sv\"\nendmodule";
    let tokens = lyra_lexer::lex(text);
    let output = run_preprocess(FileId(0), &tokens, text, &provider);

    // expanded: "module top; wire w;\nendmodule"
    // "\n" is at expanded offset 19, which is original offset 27 (after `include "b.sv"`)
    let span = output
        .source_map
        .map_point(TextSize::new(19))
        .expect("in-bounds offset should map");
    assert_eq!(span.file, FileId(0));
    // "\n" in original is at offset 27
    assert_eq!(span.range.start(), TextSize::new(27));
}

#[test]
fn map_range_within_segment() {
    let mut provider = MapProvider::new();
    provider.add("b.sv", FileId(1), "wire w;");

    let text = "module top; `include \"b.sv\"\nendmodule";
    let tokens = lyra_lexer::lex(text);
    let output = run_preprocess(FileId(0), &tokens, text, &provider);

    let range = TextRange::new(TextSize::new(12), TextSize::new(19));
    let span = output.source_map.map_range(range);
    assert!(span.is_some(), "range within one segment should map");
    let span = span.expect("should be Some");
    assert_eq!(span.file, FileId(1));
}

#[test]
fn map_range_straddling_segments_returns_none() {
    let mut provider = MapProvider::new();
    provider.add("b.sv", FileId(1), "wire w;");

    let text = "module top; `include \"b.sv\"\nendmodule";
    let tokens = lyra_lexer::lex(text);
    let output = run_preprocess(FileId(0), &tokens, text, &provider);

    let range = TextRange::new(TextSize::new(10), TextSize::new(15));
    let span = output.source_map.map_range(range);
    assert!(
        span.is_none(),
        "range straddling segments should return None"
    );
}

#[test]
fn include_graph_records_dependency() {
    let mut provider = MapProvider::new();
    provider.add("b.sv", FileId(1), "wire w;");

    let text = "module top; `include \"b.sv\"\nendmodule";
    let tokens = lyra_lexer::lex(text);
    let output = run_preprocess(FileId(0), &tokens, text, &provider);

    assert_eq!(output.includes.dependencies(), &[FileId(1)]);
}

#[test]
fn unresolved_include_stripped_with_error() {
    let provider = MapProvider::new();

    let text = "module top; `include \"missing.sv\"\nendmodule";
    let tokens = lyra_lexer::lex(text);
    let output = run_preprocess(FileId(0), &tokens, text, &provider);

    // Directives are always stripped from output
    let has_directive = output
        .tokens
        .iter()
        .any(|t| t.kind == SyntaxKind::Directive);
    assert!(!has_directive, "directives should be stripped from output");

    // Error should be recorded
    assert_eq!(output.errors.len(), 1);
    assert!(output.errors[0].message.contains("missing.sv"));
}

#[test]
fn scan_includes_finds_paths() {
    let text = "`include \"a.sv\"\nmodule top; `include \"b.sv\"\nendmodule";
    let tokens = lyra_lexer::lex(text);
    let ranges = scan_includes(&tokens, text);
    let paths: Vec<&str> = ranges
        .iter()
        .map(|r| &text[usize::from(r.start())..usize::from(r.end())])
        .collect();
    assert_eq!(paths, vec!["a.sv", "b.sv"]);
}

#[test]
fn scan_includes_empty_for_no_includes() {
    let text = "module top; endmodule";
    let tokens = lyra_lexer::lex(text);
    let paths = scan_includes(&tokens, text);
    assert!(paths.is_empty());
}

#[test]
fn multiple_includes() {
    let mut provider = MapProvider::new();
    provider.add("a.sv", FileId(1), "wire a;");
    provider.add("b.sv", FileId(2), "wire b;");

    let text = "`include \"a.sv\"\n`include \"b.sv\"\nmodule top; endmodule";
    let tokens = lyra_lexer::lex(text);
    let output = run_preprocess(FileId(0), &tokens, text, &provider);

    assert_eq!(output.includes.dependencies(), &[FileId(1), FileId(2)]);
    assert!(output.expanded_text.contains("wire a;"));
    assert!(output.expanded_text.contains("wire b;"));
    assert!(!output.expanded_text.contains("`include"));
}

#[test]
fn resolve_file_loc_main_file() {
    let mut provider = MapProvider::new();
    provider.add("b.sv", FileId(1), "wire w;");

    let text = "module top; `include \"b.sv\"\nendmodule";
    let tokens = lyra_lexer::lex(text);
    let output = run_preprocess(FileId(0), &tokens, text, &provider);

    let loc = output
        .source_map
        .resolve_file_loc(TextSize::new(0))
        .expect("in-bounds offset should resolve");
    assert_eq!(loc.file, FileId(0));
    assert_eq!(loc.offset, TextSize::new(0));
}

#[test]
fn resolve_file_loc_included_range() {
    let mut provider = MapProvider::new();
    provider.add("b.sv", FileId(1), "wire w;");

    let text = "module top; `include \"b.sv\"\nendmodule";
    let tokens = lyra_lexer::lex(text);
    let output = run_preprocess(FileId(0), &tokens, text, &provider);

    let loc = output
        .source_map
        .resolve_file_loc(TextSize::new(12))
        .expect("in-bounds offset should resolve");
    assert_eq!(loc.file, FileId(1));
    assert_eq!(loc.offset, TextSize::new(0));
}

#[test]
fn expansion_frame_identity_returns_none() {
    let mut provider = MapProvider::new();
    provider.add("b.sv", FileId(1), "wire w;");

    let text = "module top; `include \"b.sv\"\nendmodule";
    let tokens = lyra_lexer::lex(text);
    let output = run_preprocess(FileId(0), &tokens, text, &provider);

    assert!(
        output
            .source_map
            .expansion_frame(TextSize::new(0))
            .is_none()
    );
}

#[test]
fn expansion_frame_included_position() {
    let mut provider = MapProvider::new();
    provider.add("b.sv", FileId(1), "wire w;");

    let text = "module top; `include \"b.sv\"\nendmodule";
    let tokens = lyra_lexer::lex(text);
    let output = run_preprocess(FileId(0), &tokens, text, &provider);

    let frame = output
        .source_map
        .expansion_frame(TextSize::new(12))
        .expect("included offset should have expansion frame");
    assert_eq!(frame.kind, ExpansionKind::Include);
    assert_eq!(frame.call_site.file, FileId(0));
    assert_eq!(frame.spelling.file, FileId(1));
    assert_eq!(frame.spelling.offset, TextSize::new(0));
}

#[test]
fn expansion_frame_call_site_covers_directive() {
    let mut provider = MapProvider::new();
    provider.add("b.sv", FileId(1), "wire w;");

    let text = "module top; `include \"b.sv\"\nendmodule";
    let tokens = lyra_lexer::lex(text);
    let output = run_preprocess(FileId(0), &tokens, text, &provider);

    let frame = output
        .source_map
        .expansion_frame(TextSize::new(12))
        .expect("included offset should have expansion frame");

    let directive = "`include \"b.sv\"";
    let expected_range = TextRange::new(
        TextSize::new(12),
        TextSize::new(12 + directive.len() as u32),
    );
    assert_eq!(frame.call_site.range, expected_range);
}
