use std::collections::HashMap;
use std::marker::PhantomData;

use lyra_lexer::{NODE_START, SyntaxKind};
use lyra_parser::SyntaxNode;
use lyra_source::{FileId, TextRange};
use smallvec::SmallVec;

use crate::node::AstNode;

/// Opaque, typed identity for an AST node within a file.
///
/// `AstId<N>` is scoped to a particular file and parse-tree version.
/// It cannot be constructed or inspected by downstream crates.
#[derive(Debug)]
pub struct AstId<N: AstNode> {
    raw: RawAstId,
    _ph: PhantomData<fn() -> N>,
}

// Manual impls -- PhantomData<fn() -> N> is already covariant + no bounds needed.
impl<N: AstNode> Clone for AstId<N> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<N: AstNode> Copy for AstId<N> {}

impl<N: AstNode> PartialEq for AstId<N> {
    fn eq(&self, other: &Self) -> bool {
        self.raw == other.raw
    }
}
impl<N: AstNode> Eq for AstId<N> {}

impl<N: AstNode> std::hash::Hash for AstId<N> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.raw.hash(state);
    }
}

impl<N: AstNode> AstId<N> {
    /// Erase the type parameter, producing an untyped id.
    pub fn erase(self) -> ErasedAstId {
        ErasedAstId(self.raw)
    }
}

/// Type-erased AST node identity, for use in heterogeneous collections
/// (e.g. symbol tables that store ids for different node types).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ErasedAstId(RawAstId);

impl ErasedAstId {
    /// The file this AST node belongs to.
    pub fn file(self) -> FileId {
        self.0.file
    }

    /// Sentinel id for use when no AST node is available (e.g. top-level roots).
    pub fn placeholder(file: FileId) -> Self {
        Self(RawAstId {
            file,
            index: u32::MAX,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct RawAstId {
    file: FileId,
    index: u32,
}

/// Entry in the forward table: identity + path from root.
#[derive(Debug, Clone, PartialEq, Eq)]
struct AstIdEntry {
    kind: SyntaxKind,
    range: TextRange,
    // Child-index path from root. Each element is the child index at that depth.
    path: SmallVec<[u32; 8]>,
}

/// Per-file map between AST nodes and their opaque `AstId`s.
///
/// Built once after parsing. Stores only plain data (no `SyntaxNode` values),
/// making it safe for Salsa caching.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstIdMap {
    file: FileId,
    // Forward: preorder index -> entry
    entries: Vec<AstIdEntry>,
    // Reverse: (kind, range_start, range_end) -> list of preorder indices.
    // Multiple nodes can share (kind, range) in error-recovery scenarios.
    node_to_id: HashMap<(SyntaxKind, u32, u32), SmallVec<[u32; 1]>>,
}

impl AstIdMap {
    /// Build the map from a file's parse tree root via preorder traversal.
    pub fn from_root(file: FileId, root: &SyntaxNode) -> Self {
        let mut entries = Vec::new();
        let mut node_to_id = HashMap::new();
        let mut path: SmallVec<[u32; 8]> = SmallVec::new();
        collect_preorder(root, &mut path, &mut entries, &mut node_to_id);
        Self {
            file,
            entries,
            node_to_id,
        }
    }

    /// Look up the `AstId` for a node. O(1) hash lookup in the common case.
    ///
    /// The map owns its file identity -- cross-file nodes always return `None`.
    /// Node identity is verified by matching the child-index path, so even
    /// nodes with identical (kind, range) in error-recovery are disambiguated.
    pub fn ast_id<N: AstNode>(&self, node: &N) -> Option<AstId<N>> {
        let syntax = node.syntax();
        let kind = syntax.kind();
        let range = syntax.text_range();
        let key = (kind, u32::from(range.start()), u32::from(range.end()));
        let indices = self.node_to_id.get(&key)?;
        // Common case: single entry. For collisions, verify via path.
        if indices.len() == 1 {
            let index = indices[0];
            return Some(AstId {
                raw: RawAstId {
                    file: self.file,
                    index,
                },
                _ph: PhantomData,
            });
        }
        // Collision: compute the node's path and match against entries
        let node_path = compute_path(syntax);
        for &index in indices {
            if let Some(entry) = self.entries.get(index as usize)
                && entry.path == node_path
            {
                return Some(AstId {
                    raw: RawAstId {
                        file: self.file,
                        index,
                    },
                    _ph: PhantomData,
                });
            }
        }
        None
    }

    /// Resolve an `AstId` back to its node. O(depth) walk from root.
    ///
    /// Returns `None` if the id belongs to a different file or the tree has
    /// changed since the map was built.
    pub fn get<N: AstNode>(&self, root: &SyntaxNode, id: AstId<N>) -> Option<N> {
        if id.raw.file != self.file {
            return None;
        }
        let entry = self.entries.get(id.raw.index as usize)?;
        // Walk the child-index path from root
        let mut current = root.clone();
        for &child_idx in &entry.path {
            current = current.children().nth(child_idx as usize)?;
        }
        // Verify kind and range match
        if current.kind() != entry.kind || current.text_range() != entry.range {
            return None;
        }
        N::cast(current)
    }

    /// Look up the `ErasedAstId` for a raw `SyntaxNode`. O(1) hash lookup.
    ///
    /// This is the untyped counterpart of `ast_id<N>`. It accepts any
    /// node kind recorded during the preorder traversal.
    pub fn erased_ast_id(&self, node: &SyntaxNode) -> Option<ErasedAstId> {
        let kind = node.kind();
        let range = node.text_range();
        let key = (kind, u32::from(range.start()), u32::from(range.end()));
        let indices = self.node_to_id.get(&key)?;
        if indices.len() == 1 {
            return Some(ErasedAstId(RawAstId {
                file: self.file,
                index: indices[0],
            }));
        }
        let node_path = compute_path(node);
        for &index in indices {
            if let Some(entry) = self.entries.get(index as usize)
                && entry.path == node_path
            {
                return Some(ErasedAstId(RawAstId {
                    file: self.file,
                    index,
                }));
            }
        }
        None
    }

    /// Resolve an `ErasedAstId` back to an untyped `SyntaxNode`.
    ///
    /// Same walk logic as `get<N>` but returns `SyntaxNode` without casting.
    /// O(depth) walk from root, where each step is `O(children_count)` via
    /// `children().nth()`. Acceptable for expression trees (depth < 10,
    /// children < 5 per node). Not a hot path -- called once per const-eval
    /// query invocation.
    pub fn get_node(&self, root: &SyntaxNode, id: ErasedAstId) -> Option<SyntaxNode> {
        if id.0.file != self.file {
            return None;
        }
        let entry = self.entries.get(id.0.index as usize)?;
        let mut current = root.clone();
        for &child_idx in &entry.path {
            current = current.children().nth(child_idx as usize)?;
        }
        if current.kind() != entry.kind || current.text_range() != entry.range {
            return None;
        }
        Some(current)
    }

    /// File this map belongs to.
    pub fn file(&self) -> FileId {
        self.file
    }

    /// Number of entries in the map.
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Whether the map is empty.
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}

/// Compute the child-index path from the root to `node` by walking ancestors.
fn compute_path(node: &SyntaxNode) -> SmallVec<[u32; 8]> {
    let mut path = SmallVec::new();
    let mut current = node.clone();
    while let Some(parent) = current.parent() {
        let idx = parent.children().position(|c| c == current).unwrap_or(0);
        path.push(u32::try_from(idx).unwrap_or(u32::MAX));
        current = parent;
    }
    path.reverse();
    path
}

fn collect_preorder(
    node: &SyntaxNode,
    path: &mut SmallVec<[u32; 8]>,
    entries: &mut Vec<AstIdEntry>,
    node_to_id: &mut HashMap<(SyntaxKind, u32, u32), SmallVec<[u32; 1]>>,
) {
    let kind = node.kind();
    if (kind as u16) >= NODE_START {
        let range = node.text_range();
        let key = (kind, u32::from(range.start()), u32::from(range.end()));
        let index = u32::try_from(entries.len()).unwrap_or(u32::MAX);
        node_to_id.entry(key).or_default().push(index);
        entries.push(AstIdEntry {
            kind,
            range,
            path: path.clone(),
        });
    }
    for (child_idx, child) in node.children().enumerate() {
        let idx = u32::try_from(child_idx).unwrap_or(u32::MAX);
        path.push(idx);
        collect_preorder(&child, path, entries, node_to_id);
        path.pop();
    }
}
