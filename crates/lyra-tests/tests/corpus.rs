use std::path::{Path, PathBuf};

use walkdir::WalkDir;

use lyra_db::{file_diagnostics, unit_diagnostics};
use lyra_tests::TestWorkspace;
use lyra_tests::annotation;

#[derive(serde::Deserialize)]
#[serde(deny_unknown_fields)]
struct TestMeta {
    kind: Kind,
    #[serde(default)]
    lrm: Option<LrmMeta>,
}

#[derive(serde::Deserialize, PartialEq, Eq, Clone, Copy)]
#[serde(rename_all = "lowercase")]
enum Kind {
    Lrm,
    Regression,
}

#[derive(serde::Deserialize)]
#[serde(deny_unknown_fields)]
struct LrmMeta {
    chapter: u32,
    section: String,
    #[serde(default, rename = "title")]
    _title: Option<String>,
}

struct Case {
    key: String,
    rel: PathBuf,
    dir: PathBuf,
    _meta: TestMeta,
}

fn discover_test_cases(base: &Path) -> Vec<Case> {
    let mut cases = Vec::new();
    for entry in WalkDir::new(base)
        .follow_links(false)
        .into_iter()
        .filter_entry(|e| {
            let name = e.file_name();
            name != ".git" && name != "target"
        })
        .filter_map(Result::ok)
    {
        if entry.file_name() != "test.yaml" || !entry.file_type().is_file() {
            continue;
        }
        let yaml_path = entry.into_path();
        let dir = yaml_path.parent().expect("yaml has parent").to_path_buf();
        let rel = dir
            .strip_prefix(base)
            .expect("dir is under base")
            .to_path_buf();

        // No component may contain "__" (bijective snapshot key encoding)
        for component in rel.components() {
            let s = component.as_os_str().to_string_lossy();
            assert!(
                !s.contains("__"),
                "directory name must not contain '__': {s}"
            );
        }

        let key = rel
            .components()
            .map(|c| c.as_os_str().to_string_lossy())
            .collect::<Vec<_>>()
            .join("__");

        let content = std::fs::read_to_string(&yaml_path)
            .unwrap_or_else(|e| panic!("failed to read {}: {e}", yaml_path.display()));
        let meta: TestMeta = serde_yaml::from_str(&content)
            .unwrap_or_else(|e| panic!("invalid test.yaml in {}: {e}", rel.display()));

        // Validate LRM metadata consistency with path
        if meta.kind == Kind::Lrm {
            let lrm = meta
                .lrm
                .as_ref()
                .unwrap_or_else(|| panic!("LRM test {} must have lrm metadata", rel.display()));
            assert!(
                !lrm.section.is_empty(),
                "LRM test {} must have section",
                rel.display()
            );
            // LRM tests must live under lrm/chXX/
            let expected_ch_dir = format!("ch{:02}", lrm.chapter);
            let mut components = rel.components();
            let first = components.next().map(|c| c.as_os_str().to_string_lossy());
            let second = components.next().map(|c| c.as_os_str().to_string_lossy());
            assert_eq!(
                first.as_deref(),
                Some("lrm"),
                "LRM test {} must be under lrm/",
                rel.display()
            );
            assert_eq!(
                second.as_deref(),
                Some(expected_ch_dir.as_str()),
                "LRM test {}: chapter {} must be under lrm/{}/",
                rel.display(),
                lrm.chapter,
                expected_ch_dir
            );
            // Section must start with chapter number prefix
            let sec_prefix = format!("{}.", lrm.chapter);
            assert!(
                lrm.section.starts_with(&sec_prefix),
                "LRM test {}: section '{}' doesn't start with '{}'",
                rel.display(),
                lrm.section,
                sec_prefix
            );
        }

        cases.push(Case {
            key,
            rel,
            dir,
            _meta: meta,
        });
    }

    // Sort by key, then validate uniqueness
    cases.sort_by(|a, b| a.key.cmp(&b.key));
    for w in cases.windows(2) {
        assert_ne!(w[0].key, w[1].key, "duplicate snapshot key: {}", w[0].key);
    }

    cases
}

#[test]
fn corpus_snapshots() {
    let base = Path::new(env!("CARGO_MANIFEST_DIR")).join("testdata/corpus");
    let cases = discover_test_cases(&base);

    for case in &cases {
        let ws = TestWorkspace::from_dir(&case.dir)
            .unwrap_or_else(|e| panic!("failed to load corpus case {}: {e}", case.rel.display()));
        let unit = ws.compilation_unit();

        let mut all_file_diags = Vec::new();
        for i in 0..ws.file_count() {
            let source = ws.source_file(i);
            let diags = file_diagnostics(ws.db(), source, unit);
            all_file_diags.extend(diags.iter().cloned());
        }

        let unit_diags = unit_diagnostics(ws.db(), unit);
        let allow_extra = annotation::has_allow_extra_diags(&ws);

        annotation::check_annotations(&ws, &all_file_diags, unit_diags, allow_extra)
            .unwrap_or_else(|e| panic!("annotation mismatch in {}:\n{e}", case.rel.display()));

        insta::assert_snapshot!(case.key.as_str(), ws.dump_diagnostics());
    }
}
