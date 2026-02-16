use std::path::Path;

use lyra_db::{file_diagnostics, unit_diagnostics};
use lyra_tests::TestWorkspace;
use lyra_tests::annotation;

#[test]
fn corpus_snapshots() {
    let base = Path::new(env!("CARGO_MANIFEST_DIR")).join("testdata/corpus");
    let mut dirs: Vec<_> = std::fs::read_dir(&base)
        .unwrap_or_else(|e| panic!("failed to read {}: {e}", base.display()))
        .filter_map(|entry| {
            let entry = entry.ok()?;
            entry.file_type().ok()?.is_dir().then(|| entry.path())
        })
        .collect();
    dirs.sort();

    for dir in &dirs {
        let name = dir
            .file_name()
            .expect("dir should have a name")
            .to_str()
            .expect("dir name should be valid UTF-8");
        let ws = TestWorkspace::from_dir(dir)
            .unwrap_or_else(|e| panic!("failed to load corpus case {name}: {e}"));
        let unit = ws.compilation_unit();

        // Collect file diagnostics
        let mut all_file_diags = Vec::new();
        for i in 0..ws.file_count() {
            let source = ws.source_file(i);
            let diags = file_diagnostics(ws.db(), source, unit);
            all_file_diags.extend(diags.iter().cloned());
        }

        // Collect unit diagnostics
        let unit_diags = unit_diagnostics(ws.db(), unit);

        // Detect ALLOW-EXTRA-DIAGS directive
        let allow_extra = annotation::has_allow_extra_diags(&ws);

        // Annotation check (primary correctness mechanism)
        annotation::check_annotations(&ws, &all_file_diags, unit_diags, allow_extra)
            .unwrap_or_else(|e| panic!("annotation mismatch in {name}:\n{e}"));

        // Diagnostics snapshot (regression/observability)
        insta::allow_duplicates! {
            insta::assert_snapshot!(
                format!("corpus_{name}"),
                ws.dump_diagnostics()
            );
        }
    }
}
