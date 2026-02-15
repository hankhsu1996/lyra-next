use std::path::Path;

use lyra_tests::TestWorkspace;

#[test]
fn parse_snapshots() {
    let base = Path::new(env!("CARGO_MANIFEST_DIR")).join("testdata/parse");
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
        let ws = TestWorkspace::from_dir(dir).unwrap();
        insta::allow_duplicates! {
            insta::assert_snapshot!(format!("parse_{name}"), ws.dump_parse());
        }
    }
}
