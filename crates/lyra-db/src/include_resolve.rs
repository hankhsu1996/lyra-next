use std::collections::HashMap;

use crate::{IncludeKind, IncludeRequest};
use lyra_source::FileId;
use salsa::Setter;

use crate::{IncludeMap, SourceFile};

/// Trait for loading file contents and resolving paths during
/// include discovery. CLI implements this with filesystem I/O;
/// tests implement it with fixture storage.
pub trait IncludeLoader {
    /// Normalize a path into the canonical resolver key.
    /// Returns `None` if the path does not resolve to an existing file.
    fn normalize_path(&self, path: &str) -> Option<String>;

    /// Read file contents for a normalized path.
    fn read(&self, normalized_path: &str) -> Option<String>;

    /// Build a candidate path by joining the includer file's parent
    /// directory with the include spelling. The loader owns parent
    /// extraction and join semantics.
    fn join_from_includer(&self, includer_path: &str, include_spelling: &str) -> String;

    /// Build a candidate path by joining a search directory with the
    /// include spelling.
    fn join_from_dir(&self, dir: &str, include_spelling: &str) -> String;
}

/// A file discovered during include resolution.
pub struct DiscoveredFile {
    /// Normalized path (the canonical key for this file).
    pub path: String,
    /// Source text contents.
    pub text: String,
    /// Whether this file was a root input (not discovered via include).
    pub is_root: bool,
}

/// An include map entry in the discovery plan.
pub struct IncludeEntry {
    /// The structured include request (kind + spelling).
    pub request: IncludeRequest,
    /// Index into `IncludePlan::files` for the resolved target.
    pub target: usize,
}

/// The result of include discovery: all files and their include
/// relationships as pure data.
///
/// The caller owns file identity allocation (`FileId`) and database
/// mutation (`SourceFile` creation, `set_include_map`). This
/// structure carries no database references.
pub struct IncludePlan {
    /// All discovered files. Roots appear first in input order,
    /// then discovered includes in worklist order.
    pub files: Vec<DiscoveredFile>,
    /// Per-file include maps: `(file_index, entries)`.
    /// Only files that have at least one resolved include appear here.
    pub include_maps: Vec<(usize, Vec<IncludeEntry>)>,
}

/// Apply a discovery plan to the database: create `SourceFile` inputs
/// and set include maps.
///
/// `first_file_id` is the starting `FileId` value; files are assigned
/// sequential IDs from there. The caller owns the ID space by choosing
/// this value. Returns all created `SourceFile`s in plan order.
pub fn apply_include_plan(
    db: &mut crate::LyraDatabase,
    plan: &IncludePlan,
    first_file_id: u32,
) -> Vec<SourceFile> {
    let source_files: Vec<SourceFile> = plan
        .files
        .iter()
        .enumerate()
        .map(|(idx, f)| {
            SourceFile::new(
                db,
                FileId(first_file_id + idx as u32),
                f.path.clone(),
                f.text.clone(),
                IncludeMap::default(),
            )
        })
        .collect();

    for (file_idx, entries) in &plan.include_maps {
        let map_entries: Vec<(IncludeRequest, SourceFile)> = entries
            .iter()
            .map(|e| (e.request.clone(), source_files[e.target]))
            .collect();
        source_files[*file_idx]
            .set_include_map(db)
            .to(IncludeMap::new(map_entries));
    }

    source_files
}

/// Run fixed-point include discovery starting from root files.
///
/// Search order depends on include kind (LRM 22.4):
/// - Quoted (`"file"`): includer directory first, then include dirs.
/// - Angle-bracket (`<file>`): include dirs only.
///
/// First match wins. This is a pure function with no database
/// interaction; the caller is responsible for creating `SourceFile`
/// inputs and setting include maps from the returned plan.
pub fn discover(
    roots: &[(String, String)],
    include_dirs: &[String],
    loader: &dyn IncludeLoader,
) -> IncludePlan {
    let mut path_to_index: HashMap<String, usize> = HashMap::new();
    let mut files: Vec<DiscoveredFile> = Vec::new();
    let mut include_maps: Vec<(usize, Vec<IncludeEntry>)> = Vec::new();

    for (norm_path, text) in roots {
        let idx = files.len();
        path_to_index.insert(norm_path.clone(), idx);
        files.push(DiscoveredFile {
            path: norm_path.clone(),
            text: text.clone(),
            is_root: true,
        });
    }

    let mut worklist: Vec<usize> = (0..files.len()).collect();
    let mut scanned: std::collections::HashSet<usize> = std::collections::HashSet::new();

    while let Some(current_idx) = worklist.pop() {
        if !scanned.insert(current_idx) {
            continue;
        }

        let current_path = files[current_idx].path.clone();
        let text = files[current_idx].text.clone();

        let tokens = lyra_lexer::lex_with_mode(&text, lyra_lexer::LexMode::Preprocess);
        let occurrences = lyra_preprocess::scan_includes(&tokens, &text);

        let mut entries = Vec::new();

        for occ in &occurrences {
            if let Some(target_norm) =
                resolve_include_request(&occ.request, &current_path, include_dirs, loader)
            {
                let target_idx = if let Some(&idx) = path_to_index.get(&target_norm) {
                    idx
                } else if let Some(target_text) = loader.read(&target_norm) {
                    let idx = files.len();
                    path_to_index.insert(target_norm.clone(), idx);
                    files.push(DiscoveredFile {
                        path: target_norm,
                        text: target_text,
                        is_root: false,
                    });
                    worklist.push(idx);
                    idx
                } else {
                    continue;
                };

                entries.push(IncludeEntry {
                    request: occ.request.clone(),
                    target: target_idx,
                });
            }
        }

        if !entries.is_empty() {
            include_maps.push((current_idx, entries));
        }
    }

    IncludePlan {
        files,
        include_maps,
    }
}

/// Resolve an include request against search paths.
///
/// Quoted includes search the includer's directory first, then
/// configured include dirs. Angle-bracket includes skip the
/// includer's directory and only search include dirs.
fn resolve_include_request(
    request: &IncludeRequest,
    includer_path: &str,
    include_dirs: &[String],
    loader: &dyn IncludeLoader,
) -> Option<String> {
    let spelling = request.spelling.as_str();

    if request.kind == IncludeKind::Quoted {
        let candidate = loader.join_from_includer(includer_path, spelling);
        if let Some(norm) = loader.normalize_path(&candidate) {
            return Some(norm);
        }
    }

    for dir in include_dirs {
        let candidate = loader.join_from_dir(dir, spelling);
        if let Some(norm) = loader.normalize_path(&candidate) {
            return Some(norm);
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;

    struct MemLoader {
        files: HashMap<String, String>,
    }

    impl MemLoader {
        fn new(files: Vec<(&str, &str)>) -> Self {
            Self {
                files: files
                    .into_iter()
                    .map(|(k, v)| (k.to_owned(), v.to_owned()))
                    .collect(),
            }
        }
    }

    impl IncludeLoader for MemLoader {
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

    #[test]
    fn relative_include_same_dir() {
        let loader = MemLoader::new(vec![
            (
                "src/main.sv",
                "`include \"header.svh\"\nmodule m; endmodule",
            ),
            ("src/header.svh", "typedef int T;"),
        ]);
        let roots = vec![(
            "src/main.sv".to_owned(),
            "`include \"header.svh\"\nmodule m; endmodule".to_owned(),
        )];
        let plan = discover(&roots, &[], &loader);
        assert_eq!(plan.files.len(), 2);
        assert!(plan.files[0].is_root);
        assert_eq!(plan.files[0].path, "src/main.sv");
        assert!(!plan.files[1].is_root);
        assert_eq!(plan.files[1].path, "src/header.svh");

        assert_eq!(plan.include_maps.len(), 1);
        let (idx, entries) = &plan.include_maps[0];
        assert_eq!(*idx, 0);
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].request.spelling.as_str(), "header.svh");
        assert_eq!(entries[0].request.kind, IncludeKind::Quoted);
        assert_eq!(entries[0].target, 1);
    }

    #[test]
    fn nested_include_discovery() {
        let loader = MemLoader::new(vec![
            ("a.sv", "`include \"b.svh\"\nmodule m; endmodule"),
            ("b.svh", "`include \"c.svh\"\ntypedef int B;"),
            ("c.svh", "typedef int C;"),
        ]);
        let roots = vec![(
            "a.sv".to_owned(),
            "`include \"b.svh\"\nmodule m; endmodule".to_owned(),
        )];
        let plan = discover(&roots, &[], &loader);
        assert_eq!(plan.files.len(), 3);

        let b_map = plan
            .include_maps
            .iter()
            .find(|(idx, _)| plan.files[*idx].path == "b.svh");
        assert!(b_map.is_some());
        let (_, entries) = b_map.unwrap();
        assert_eq!(entries[0].request.spelling.as_str(), "c.svh");
        assert_eq!(plan.files[entries[0].target].path, "c.svh");
    }

    #[test]
    fn include_dir_fallback() {
        let loader = MemLoader::new(vec![
            (
                "src/main.sv",
                "`include \"header.svh\"\nmodule m; endmodule",
            ),
            ("inc/header.svh", "typedef int T;"),
        ]);
        let roots = vec![(
            "src/main.sv".to_owned(),
            "`include \"header.svh\"\nmodule m; endmodule".to_owned(),
        )];
        let plan = discover(&roots, &["inc".to_owned()], &loader);
        assert_eq!(plan.files.len(), 2);
        assert_eq!(plan.files[1].path, "inc/header.svh");
    }

    #[test]
    fn first_match_wins() {
        let loader = MemLoader::new(vec![
            ("src/main.sv", "`include \"h.svh\"\nmodule m; endmodule"),
            ("inc1/h.svh", "typedef int A;"),
            ("inc2/h.svh", "typedef int B;"),
        ]);
        let roots = vec![(
            "src/main.sv".to_owned(),
            "`include \"h.svh\"\nmodule m; endmodule".to_owned(),
        )];
        let plan = discover(&roots, &["inc1".to_owned(), "inc2".to_owned()], &loader);
        let h = plan
            .files
            .iter()
            .find(|f| f.path.contains("h.svh"))
            .unwrap();
        assert_eq!(h.path, "inc1/h.svh");
        assert_eq!(h.text, "typedef int A;");
    }

    #[test]
    fn local_dir_beats_include_dirs() {
        let loader = MemLoader::new(vec![
            ("src/main.sv", "`include \"h.svh\"\nmodule m; endmodule"),
            ("src/h.svh", "typedef int LOCAL;"),
            ("inc/h.svh", "typedef int GLOBAL;"),
        ]);
        let roots = vec![(
            "src/main.sv".to_owned(),
            "`include \"h.svh\"\nmodule m; endmodule".to_owned(),
        )];
        let plan = discover(&roots, &["inc".to_owned()], &loader);
        let h = plan
            .files
            .iter()
            .find(|f| f.path.contains("h.svh"))
            .unwrap();
        assert_eq!(h.path, "src/h.svh");
        assert_eq!(h.text, "typedef int LOCAL;");
    }

    #[test]
    fn duplicate_include_loaded_once() {
        let loader = MemLoader::new(vec![
            ("a.sv", "`include \"common.svh\"\nmodule a; endmodule"),
            ("b.sv", "`include \"common.svh\"\nmodule b; endmodule"),
            ("common.svh", "typedef int T;"),
        ]);
        let roots = vec![
            (
                "a.sv".to_owned(),
                "`include \"common.svh\"\nmodule a; endmodule".to_owned(),
            ),
            (
                "b.sv".to_owned(),
                "`include \"common.svh\"\nmodule b; endmodule".to_owned(),
            ),
        ];
        let plan = discover(&roots, &[], &loader);
        assert_eq!(plan.files.len(), 3);
        let common_count = plan.files.iter().filter(|f| f.path == "common.svh").count();
        assert_eq!(common_count, 1);
    }

    #[test]
    fn root_order_preserved() {
        let loader = MemLoader::new(vec![
            ("b.sv", "module b; endmodule"),
            ("a.sv", "module a; endmodule"),
        ]);
        let roots = vec![
            ("b.sv".to_owned(), "module b; endmodule".to_owned()),
            ("a.sv".to_owned(), "module a; endmodule".to_owned()),
        ];
        let plan = discover(&roots, &[], &loader);
        assert_eq!(plan.files.len(), 2);
        assert_eq!(plan.files[0].path, "b.sv");
        assert!(plan.files[0].is_root);
        assert_eq!(plan.files[1].path, "a.sv");
        assert!(plan.files[1].is_root);
    }

    #[test]
    fn angle_bracket_skips_includer_dir() {
        let loader = MemLoader::new(vec![
            ("src/main.sv", "`include <h.svh>\nmodule m; endmodule"),
            ("src/h.svh", "typedef int LOCAL;"),
            ("inc/h.svh", "typedef int GLOBAL;"),
        ]);
        let roots = vec![(
            "src/main.sv".to_owned(),
            "`include <h.svh>\nmodule m; endmodule".to_owned(),
        )];
        let plan = discover(&roots, &["inc".to_owned()], &loader);
        let h = plan
            .files
            .iter()
            .find(|f| f.path.contains("h.svh"))
            .unwrap();
        assert_eq!(h.path, "inc/h.svh");
        assert_eq!(h.text, "typedef int GLOBAL;");
    }

    #[test]
    fn angle_bracket_no_match_without_inc_dir() {
        let loader = MemLoader::new(vec![
            ("src/main.sv", "`include <h.svh>\nmodule m; endmodule"),
            ("src/h.svh", "typedef int LOCAL;"),
        ]);
        let roots = vec![(
            "src/main.sv".to_owned(),
            "`include <h.svh>\nmodule m; endmodule".to_owned(),
        )];
        let plan = discover(&roots, &[], &loader);
        assert_eq!(plan.files.len(), 1);
        assert!(plan.include_maps.is_empty());
    }

    #[test]
    fn same_spelling_different_kind_resolves_independently() {
        let loader = MemLoader::new(vec![
            (
                "src/main.sv",
                "`include \"h.svh\"\n`include <h.svh>\nmodule m; endmodule",
            ),
            ("src/h.svh", "typedef int LOCAL;"),
            ("inc/h.svh", "typedef int GLOBAL;"),
        ]);
        let roots = vec![(
            "src/main.sv".to_owned(),
            "`include \"h.svh\"\n`include <h.svh>\nmodule m; endmodule".to_owned(),
        )];
        let plan = discover(&roots, &["inc".to_owned()], &loader);
        assert_eq!(plan.files.len(), 3);

        let (_, entries) = &plan.include_maps[0];
        assert_eq!(entries.len(), 2);

        let quoted = entries
            .iter()
            .find(|e| e.request.kind == IncludeKind::Quoted)
            .unwrap();
        assert_eq!(plan.files[quoted.target].path, "src/h.svh");

        let angle = entries
            .iter()
            .find(|e| e.request.kind == IncludeKind::AngleBracket)
            .unwrap();
        assert_eq!(plan.files[angle.target].path, "inc/h.svh");
    }
}
