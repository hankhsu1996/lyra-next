use std::collections::HashMap;

use lyra_db::include_resolve::IncludeLoader;

/// Fixture-backed include loader for test workspaces.
///
/// Files are keyed by workspace-relative paths. Resolution
/// checks if a candidate path matches any known fixture file.
pub struct FixtureIncludeLoader {
    files: HashMap<String, String>,
}

impl FixtureIncludeLoader {
    pub fn new(fixture_files: &[(String, String)]) -> Self {
        Self {
            files: fixture_files.iter().cloned().collect(),
        }
    }
}

impl IncludeLoader for FixtureIncludeLoader {
    fn normalize_path(&self, path: &str) -> Option<String> {
        let normalized = simplify_path(path);
        if self.files.contains_key(&normalized) {
            Some(normalized)
        } else {
            None
        }
    }

    fn read(&self, normalized_path: &str) -> Option<String> {
        self.files.get(normalized_path).cloned()
    }

    fn join_from_includer(&self, includer_path: &str, include_spelling: &str) -> String {
        let parent = match includer_path.rfind('/') {
            Some(pos) => &includer_path[..pos],
            None => "",
        };
        if parent.is_empty() {
            include_spelling.to_owned()
        } else {
            format!("{parent}/{include_spelling}")
        }
    }

    fn join_from_dir(&self, dir: &str, include_spelling: &str) -> String {
        if dir.is_empty() {
            include_spelling.to_owned()
        } else {
            format!("{dir}/{include_spelling}")
        }
    }
}

/// Simplify a path by resolving `.` and `..` segments.
fn simplify_path(path: &str) -> String {
    let mut parts: Vec<&str> = Vec::new();
    for seg in path.split('/') {
        match seg {
            "." | "" => {}
            ".." => {
                parts.pop();
            }
            s => parts.push(s),
        }
    }
    parts.join("/")
}
