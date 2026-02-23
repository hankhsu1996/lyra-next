use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::path::{Path, PathBuf};

use walkdir::WalkDir;

use lyra_db::{file_diagnostics, unit_diagnostics};
use lyra_tests::TestWorkspace;
use lyra_tests::annotation;

// Section index loaded from docs/lrm/sections.json

#[derive(serde::Deserialize)]
struct SectionEntry {
    section: String,
    chapter: u32,
    #[serde(default, rename = "title")]
    _title: Option<String>,
    parent: Option<String>,
    children: Vec<String>,
    ownable: bool,
}

struct SectionIndex {
    entries: HashMap<String, SectionEntry>,
}

impl SectionIndex {
    fn load(path: &Path) -> Self {
        let content = std::fs::read_to_string(path)
            .unwrap_or_else(|e| panic!("failed to read sections.json: {e}"));
        let entries_vec: Vec<SectionEntry> =
            serde_json::from_str(&content).unwrap_or_else(|e| panic!("invalid sections.json: {e}"));
        let mut entries = HashMap::new();
        for entry in entries_vec {
            let key = entry.section.clone();
            assert!(
                entries.insert(key.clone(), entry).is_none(),
                "duplicate section in sections.json: {key}"
            );
        }
        let index = Self { entries };
        index.validate();
        index
    }

    fn validate(&self) {
        for (section, entry) in &self.entries {
            // chapter == first number of section
            let first_num: u32 = section
                .split('.')
                .next()
                .and_then(|s| s.parse().ok())
                .unwrap_or_else(|| panic!("section '{section}' has unparseable chapter prefix"));
            assert_eq!(
                entry.chapter, first_num,
                "section '{section}': chapter {} != first number {first_num}",
                entry.chapter
            );

            // If children is non-empty, ownable must be false
            if !entry.children.is_empty() {
                assert!(
                    !entry.ownable,
                    "section '{section}' has children but ownable is true"
                );
            }

            // Every parent reference must exist
            if let Some(parent) = &entry.parent {
                assert!(
                    self.entries.contains_key(parent),
                    "section '{section}': parent '{parent}' not found in index"
                );
            }

            // Every children entry must exist
            for child in &entry.children {
                assert!(
                    self.entries.contains_key(child),
                    "section '{section}': child '{child}' not found in index"
                );
            }

            // .0 children: at most one per parent, must be a leaf
            let dot_zero_children: Vec<_> = entry
                .children
                .iter()
                .filter(|c| c.ends_with(".0"))
                .collect();
            assert!(
                dot_zero_children.len() <= 1,
                "section '{section}': multiple .0 children: {dot_zero_children:?}"
            );
            for z in &dot_zero_children {
                if let Some(ze) = self.entries.get(*z) {
                    assert!(
                        ze.children.is_empty(),
                        "section '{z}': .0 child must be a leaf (has children)"
                    );
                }
            }
        }
    }

    fn is_ownable(&self, section: &str) -> bool {
        self.entries.get(section).is_some_and(|e| e.ownable)
    }

    fn exists(&self, section: &str) -> bool {
        self.entries.contains_key(section)
    }

    fn chapter_of(&self, section: &str) -> Option<u32> {
        self.entries.get(section).map(|e| e.chapter)
    }

    fn ownable_sections_for_chapter(&self, chapter: u32) -> usize {
        self.entries
            .values()
            .filter(|e| e.chapter == chapter && e.ownable)
            .count()
    }
}

// test.yaml schema

#[derive(serde::Deserialize)]
#[serde(deny_unknown_fields)]
struct TestMeta {
    kind: Kind,
    #[serde(default)]
    lrm: Option<LrmMeta>,
}

#[derive(Debug, serde::Deserialize, PartialEq, Eq, Clone, Copy)]
#[serde(rename_all = "lowercase")]
enum Kind {
    Lrm,
    Regression,
}

#[derive(serde::Deserialize)]
#[serde(deny_unknown_fields)]
struct LrmMeta {
    section: String,
}

struct Case {
    key: String,
    rel: PathBuf,
    dir: PathBuf,
    _meta: TestMeta,
}

// Extracts e.g. "5.6.1" from "5.6.1_escaped_identifiers"
fn extract_section_prefix(dir_name: &str) -> Option<&str> {
    let underscore_pos = dir_name.find('_')?;
    let prefix = &dir_name[..underscore_pos];
    if prefix.is_empty() {
        return None;
    }
    for part in prefix.split('.') {
        if part.is_empty() || !part.chars().all(|c| c.is_ascii_digit()) {
            return None;
        }
    }
    // Must have at least one dot (e.g. "5.3", not just "5")
    if !prefix.contains('.') {
        return None;
    }
    Some(prefix)
}

fn extract_label_from_owner_dir(dir_name: &str) -> &str {
    let underscore_pos = dir_name.find('_').expect("owner dir must have underscore");
    &dir_name[underscore_pos + 1..]
}

fn is_allowed_non_test_entry(name: &str) -> bool {
    name.starts_with("README") || name.starts_with('_') || name == ".gitkeep"
}

fn discover_lrm_owner(
    base: &Path,
    ch_name: &str,
    owner_name: &str,
    owner_path: PathBuf,
    index: &SectionIndex,
    cases: &mut Vec<Case>,
) {
    let section = extract_section_prefix(owner_name).unwrap_or_else(|| {
        panic!(
            "LRM directory 'lrm/{ch_name}/{owner_name}' does not match \
             section-keyed format '{{section}}_{{label}}'. \
             All dirs under lrm/chXX/ must start with a section prefix \
             (e.g., '5.3_white_space'). Migrate old-layout dirs."
        )
    });
    let label = extract_label_from_owner_dir(owner_name);

    assert!(
        index.exists(section),
        "lrm/{ch_name}/{owner_name}: section '{section}' not in sections.json"
    );
    assert!(
        index.is_ownable(section),
        "lrm/{ch_name}/{owner_name}: section '{section}' is not ownable \
         (has children or marked non-ownable)"
    );

    let chapter = index.chapter_of(section).unwrap();
    let expected_ch = format!("ch{chapter:02}");
    assert_eq!(
        ch_name, expected_ch,
        "lrm/{ch_name}/{owner_name}: section '{section}' belongs in \
         lrm/{expected_ch}/, not lrm/{ch_name}/"
    );

    let cases_dir = owner_path.join("cases");
    if cases_dir.is_dir() {
        assert!(
            !owner_path.join("test.yaml").exists(),
            "lrm/{ch_name}/{owner_name}: has both cases/ and test.yaml \
             (mixed mode not allowed)"
        );
        for case_entry in std::fs::read_dir(&cases_dir)
            .unwrap_or_else(|e| panic!("failed to read cases/ in {}: {e}", owner_path.display()))
        {
            let case_entry = case_entry.unwrap_or_else(|e| panic!("read_dir error: {e}"));
            let case_name = case_entry.file_name().to_string_lossy().to_string();
            if !case_entry.file_type().is_ok_and(|ft| ft.is_dir()) {
                continue;
            }
            let case_path = case_entry.path();
            let yaml_path = case_path.join("test.yaml");
            assert!(
                yaml_path.exists(),
                "lrm/{ch_name}/{owner_name}/cases/{case_name}: missing test.yaml"
            );
            let rel = case_path
                .strip_prefix(base)
                .expect("case is under base")
                .to_path_buf();
            let meta = load_and_validate_yaml(&yaml_path, &rel, section, index);
            let key = format!("lrm__{chapter}__{section}__{label}__{case_name}");
            cases.push(Case {
                key,
                rel,
                dir: case_path,
                _meta: meta,
            });
        }
    } else {
        let yaml_path = owner_path.join("test.yaml");
        assert!(
            yaml_path.exists(),
            "lrm/{ch_name}/{owner_name}: missing test.yaml"
        );
        let rel = owner_path
            .strip_prefix(base)
            .expect("owner is under base")
            .to_path_buf();
        let meta = load_and_validate_yaml(&yaml_path, &rel, section, index);
        let key = format!("lrm__{chapter}__{section}__{label}");
        cases.push(Case {
            key,
            rel,
            dir: owner_path,
            _meta: meta,
        });
    }
}

fn discover_regression_cases(base: &Path, cases: &mut Vec<Case>) {
    let regression_base = base.join("regression");
    if !regression_base.is_dir() {
        return;
    }
    for entry in WalkDir::new(&regression_base)
        .follow_links(false)
        .into_iter()
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
        let name = dir
            .file_name()
            .expect("regression dir has name")
            .to_string_lossy();
        let content = std::fs::read_to_string(&yaml_path)
            .unwrap_or_else(|e| panic!("failed to read {}: {e}", yaml_path.display()));
        let meta: TestMeta = serde_yaml::from_str(&content)
            .unwrap_or_else(|e| panic!("invalid test.yaml in {}: {e}", rel.display()));
        assert_eq!(
            meta.kind,
            Kind::Regression,
            "test in regression/ must have kind: regression ({})",
            rel.display()
        );
        let key = format!("regression__{name}");
        cases.push(Case {
            key,
            rel,
            dir,
            _meta: meta,
        });
    }
}

fn discover_test_cases(base: &Path, index: &SectionIndex) -> Vec<Case> {
    let mut cases = Vec::new();
    let lrm_base = base.join("lrm");

    let mut section_owners: BTreeMap<String, String> = BTreeMap::new();
    let mut owned_per_chapter: BTreeMap<u32, BTreeSet<String>> = BTreeMap::new();

    if lrm_base.is_dir() {
        for ch_entry in
            std::fs::read_dir(&lrm_base).unwrap_or_else(|e| panic!("failed to read lrm/: {e}"))
        {
            let ch_entry = ch_entry.unwrap_or_else(|e| panic!("read_dir error: {e}"));
            let ch_name = ch_entry.file_name().to_string_lossy().to_string();
            if !ch_entry.file_type().is_ok_and(|ft| ft.is_dir()) {
                continue;
            }
            let ch_path = ch_entry.path();
            for owner_entry in std::fs::read_dir(&ch_path)
                .unwrap_or_else(|e| panic!("failed to read {}: {e}", ch_path.display()))
            {
                let owner_entry = owner_entry.unwrap_or_else(|e| panic!("read_dir error: {e}"));
                let owner_name = owner_entry.file_name().to_string_lossy().to_string();
                if !owner_entry.file_type().is_ok_and(|ft| ft.is_dir()) {
                    continue;
                }
                if is_allowed_non_test_entry(&owner_name) {
                    continue;
                }

                let section =
                    extract_section_prefix(&owner_name).expect("validated in discover_lrm_owner");

                if let Some(prev) = section_owners.get(section) {
                    panic!(
                        "duplicate section ownership: '{section}' claimed by \
                         both '{prev}' and 'lrm/{ch_name}/{owner_name}'"
                    );
                }
                let chapter = index
                    .chapter_of(section)
                    .unwrap_or_else(|| panic!("section '{section}' not in sections.json"));
                section_owners.insert(section.to_string(), format!("lrm/{ch_name}/{owner_name}"));
                owned_per_chapter
                    .entry(chapter)
                    .or_default()
                    .insert(section.to_string());

                discover_lrm_owner(
                    base,
                    &ch_name,
                    &owner_name,
                    owner_entry.path(),
                    index,
                    &mut cases,
                );
            }
        }
    }

    discover_regression_cases(base, &mut cases);

    cases.sort_by(|a, b| a.key.cmp(&b.key));
    for w in cases.windows(2) {
        assert_ne!(w[0].key, w[1].key, "duplicate snapshot key: {}", w[0].key);
    }

    // Coverage summary
    let mut summary_parts = Vec::new();
    for (&ch, owned_set) in &owned_per_chapter {
        let owned = owned_set.len();
        let total = index.ownable_sections_for_chapter(ch);
        let pct = if total > 0 { owned * 100 / total } else { 0 };
        summary_parts.push(format!("ch{ch:02} {owned}/{total} ({pct}%)"));
    }
    if !summary_parts.is_empty() {
        eprintln!("LRM coverage: {}", summary_parts.join(", "));
    }

    cases
}

fn load_and_validate_yaml(
    yaml_path: &Path,
    rel: &Path,
    expected_section: &str,
    index: &SectionIndex,
) -> TestMeta {
    let content = std::fs::read_to_string(yaml_path)
        .unwrap_or_else(|e| panic!("failed to read {}: {e}", yaml_path.display()));
    let meta: TestMeta = serde_yaml::from_str(&content)
        .unwrap_or_else(|e| panic!("invalid test.yaml in {}: {e}", rel.display()));

    assert_eq!(
        meta.kind,
        Kind::Lrm,
        "test under lrm/ must have kind: lrm ({})",
        rel.display()
    );

    let lrm = meta
        .lrm
        .as_ref()
        .unwrap_or_else(|| panic!("LRM test {} must have lrm metadata", rel.display()));

    assert!(
        !lrm.section.is_empty(),
        "LRM test {} must have section",
        rel.display()
    );

    assert!(
        index.exists(&lrm.section),
        "LRM test {}: section '{}' not in sections.json",
        rel.display(),
        lrm.section
    );

    assert_eq!(
        lrm.section,
        expected_section,
        "LRM test {}: section '{}' doesn't match owner dir section '{expected_section}'",
        rel.display(),
        lrm.section
    );

    meta
}

#[test]
fn corpus_snapshots() {
    let base = Path::new(env!("CARGO_MANIFEST_DIR")).join("testdata/corpus");

    let project_root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("crate dir has parent")
        .parent()
        .expect("crates dir has parent");
    let sections_path = project_root.join("docs/lrm/sections.json");
    let index = SectionIndex::load(&sections_path);

    let cases = discover_test_cases(&base, &index);

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
