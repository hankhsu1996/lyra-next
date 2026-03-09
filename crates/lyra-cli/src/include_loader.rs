use std::path::Path;

use lyra_db::include_resolve::IncludeLoader;

/// Filesystem-backed include loader for CLI usage.
pub struct FsIncludeLoader;

impl FsIncludeLoader {
    /// Normalize a file path to its canonical form. Returns `None` if
    /// the file does not exist.
    pub fn normalize_file_path(path: &str) -> Option<String> {
        let p = Path::new(path);
        p.canonicalize()
            .ok()
            .map(|c| c.to_string_lossy().into_owned())
    }
}

impl IncludeLoader for FsIncludeLoader {
    fn normalize_path(&self, path: &str) -> Option<String> {
        let p = Path::new(path);
        if p.is_file() {
            p.canonicalize()
                .ok()
                .map(|c| c.to_string_lossy().into_owned())
        } else {
            None
        }
    }

    fn read(&self, normalized_path: &str) -> Option<String> {
        std::fs::read_to_string(normalized_path).ok()
    }

    fn join_from_includer(&self, includer_path: &str, include_spelling: &str) -> String {
        let parent = Path::new(includer_path)
            .parent()
            .unwrap_or_else(|| Path::new(""));
        parent.join(include_spelling).to_string_lossy().into_owned()
    }

    fn join_from_dir(&self, dir: &str, include_spelling: &str) -> String {
        if dir.is_empty() {
            include_spelling.to_owned()
        } else {
            Path::new(dir)
                .join(include_spelling)
                .to_string_lossy()
                .into_owned()
        }
    }
}
