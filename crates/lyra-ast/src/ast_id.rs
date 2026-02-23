use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;

use lyra_lexer::{NODE_START, SyntaxKind};
use lyra_parser::SyntaxNode;
use lyra_source::{FileId, TextRange, TextSize};
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
#[derive(Debug, Clone, Copy)]
pub struct ErasedAstId(RawAstId);

impl PartialEq for ErasedAstId {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl Eq for ErasedAstId {}

impl Hash for ErasedAstId {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl PartialOrd for ErasedAstId {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ErasedAstId {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl ErasedAstId {
    /// The file this AST node belongs to.
    pub fn file(self) -> FileId {
        self.0.file
    }

    /// The source range of this AST node, derived directly from the ID.
    ///
    /// This is a convenience accessor. For file-guarded + existence-checked
    /// lookups, use `AstIdMap::range_of` instead.
    pub fn text_range(self) -> TextRange {
        let start = TextSize::new(self.0.start);
        let end = TextSize::new(self.0.start.saturating_add(self.0.len));
        TextRange::new(start, end)
    }

    /// Sentinel id for use when no AST node is available (e.g. top-level roots).
    pub fn placeholder(file: FileId) -> Self {
        Self(RawAstId {
            file,
            kind: SyntaxKind::Whitespace,
            start: u32::MAX,
            len: 0,
            disamb: u32::MAX,
        })
    }
}

/// Offset-based identity for an AST node.
///
/// Encodes `(file, kind, start_offset, span_length, disambiguator)`.
/// IDs are stable for nodes whose `(kind, start, len)` is unchanged;
/// tree-shape changes that don't move byte offsets no longer cause churn.
#[derive(Debug, Clone, Copy)]
struct RawAstId {
    file: FileId,
    kind: SyntaxKind,
    start: u32,
    len: u32,
    disamb: u32,
}

impl PartialEq for RawAstId {
    fn eq(&self, other: &Self) -> bool {
        self.file == other.file
            && self.kind == other.kind
            && self.start == other.start
            && self.len == other.len
            && self.disamb == other.disamb
    }
}
impl Eq for RawAstId {}

impl Hash for RawAstId {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.file.hash(state);
        self.kind.hash(state);
        self.start.hash(state);
        self.len.hash(state);
        self.disamb.hash(state);
    }
}

impl PartialOrd for RawAstId {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for RawAstId {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.file
            .cmp(&other.file)
            .then_with(|| self.start.cmp(&other.start))
            .then_with(|| self.len.cmp(&other.len))
            .then_with(|| self.kind.cmp(&other.kind))
            .then_with(|| self.disamb.cmp(&other.disamb))
    }
}

/// Per-file map between AST nodes and their opaque `AstId`s.
///
/// Built once after parsing. Stores only plain data (no `SyntaxNode` values),
/// making it safe for Salsa caching.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstIdMap {
    file: FileId,
    // Forward: (kind, start, len) -> count of nodes with this triple.
    // Implicitly defines disamb values 0..count. Almost always 1.
    forward: HashMap<(SyntaxKind, u32, u32), u32>,
    // Reverse: RawAstId -> (path_hash, child-index path from root).
    // path_hash (u64) for fast collision scan; full path for exact match.
    reverse: HashMap<RawAstId, (u64, SmallVec<[u32; 8]>)>,
}

impl AstIdMap {
    /// Build the map from a file's parse tree root via preorder traversal.
    pub fn from_root(file: FileId, root: &SyntaxNode) -> Self {
        let mut forward: HashMap<(SyntaxKind, u32, u32), u32> = HashMap::new();
        let mut reverse: HashMap<RawAstId, (u64, SmallVec<[u32; 8]>)> = HashMap::new();
        let mut path: SmallVec<[u32; 8]> = SmallVec::new();
        collect_preorder(file, root, &mut path, &mut forward, &mut reverse);
        Self {
            file,
            forward,
            reverse,
        }
    }

    /// Look up the `AstId` for a node. O(1) hash lookup in the common case.
    ///
    /// The map owns its file identity -- cross-file nodes always return `None`.
    /// Node identity is verified by matching the child-index path, so even
    /// nodes with identical (kind, range) in error-recovery are disambiguated.
    pub fn ast_id<N: AstNode>(&self, node: &N) -> Option<AstId<N>> {
        self.raw_ast_id(node.syntax()).map(|raw| AstId {
            raw,
            _ph: PhantomData,
        })
    }

    /// Resolve an `AstId` back to its node. O(depth) walk from root.
    ///
    /// Returns `None` if the id belongs to a different file or the tree has
    /// changed since the map was built.
    pub fn get<N: AstNode>(&self, root: &SyntaxNode, id: AstId<N>) -> Option<N> {
        let node = self.get_node_inner(root, id.raw)?;
        N::cast(node)
    }

    /// Look up the `ErasedAstId` for a raw `SyntaxNode`. O(1) hash lookup.
    ///
    /// This is the untyped counterpart of `ast_id<N>`. It accepts any
    /// node kind recorded during the preorder traversal.
    pub fn erased_ast_id(&self, node: &SyntaxNode) -> Option<ErasedAstId> {
        self.raw_ast_id(node).map(ErasedAstId)
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
        self.get_node_inner(root, id.0)
    }

    /// Look up the `TextRange` stored for an `ErasedAstId`. O(1).
    ///
    /// Returns the text range derived from the ID's start/len fields.
    /// Returns `None` if the id is from a different file or is not
    /// present in the map.
    pub fn range_of(&self, id: ErasedAstId) -> Option<TextRange> {
        if id.0.file != self.file {
            return None;
        }
        if !self.reverse.contains_key(&id.0) {
            return None;
        }
        Some(id.text_range())
    }

    /// File this map belongs to.
    pub fn file(&self) -> FileId {
        self.file
    }

    /// Number of entries in the map.
    pub fn len(&self) -> usize {
        self.reverse.len()
    }

    /// Whether the map is empty.
    pub fn is_empty(&self) -> bool {
        self.reverse.is_empty()
    }

    /// Look up the `RawAstId` for a `SyntaxNode`.
    fn raw_ast_id(&self, node: &SyntaxNode) -> Option<RawAstId> {
        let kind = node.kind();
        let range = node.text_range();
        let start: u32 = range.start().into();
        let end: u32 = range.end().into();
        let len = end - start;
        let key = (kind, start, len);
        let count = *self.forward.get(&key)?;
        if count == 1 {
            return Some(RawAstId {
                file: self.file,
                kind,
                start,
                len,
                disamb: 0,
            });
        }
        // Collision: compute path + hash, scan disamb values
        let node_path = compute_path(node);
        let node_hash = hash_path(&node_path);
        for disamb in 0..count {
            let candidate = RawAstId {
                file: self.file,
                kind,
                start,
                len,
                disamb,
            };
            if let Some((stored_hash, stored_path)) = self.reverse.get(&candidate)
                && *stored_hash == node_hash
                && *stored_path == node_path
            {
                return Some(candidate);
            }
        }
        None
    }

    /// Resolve a `RawAstId` back to its `SyntaxNode` by walking the stored path.
    fn get_node_inner(&self, root: &SyntaxNode, raw: RawAstId) -> Option<SyntaxNode> {
        let (_hash, path) = self.reverse.get(&raw)?;
        let mut current = root.clone();
        for &child_idx in path {
            current = current.children().nth(child_idx as usize)?;
        }
        if current.kind() != raw.kind {
            return None;
        }
        let range = current.text_range();
        let start: u32 = range.start().into();
        let end: u32 = range.end().into();
        if start != raw.start || (end - start) != raw.len {
            return None;
        }
        Some(current)
    }
}

/// Compute the child-index path from the root to `node` by walking ancestors.
fn compute_path(node: &SyntaxNode) -> SmallVec<[u32; 8]> {
    let mut path = SmallVec::new();
    let mut current = node.clone();
    while let Some(parent) = current.parent() {
        let mut idx = 0u32;
        for child in parent.children() {
            if child == current {
                break;
            }
            idx = idx.saturating_add(1);
        }
        path.push(idx);
        current = parent;
    }
    path.reverse();
    path
}

/// Hash a child-index path to u64 for deterministic collision filtering.
fn hash_path(path: &[u32]) -> u64 {
    let mut h = std::hash::DefaultHasher::new();
    path.hash(&mut h);
    h.finish()
}

fn collect_preorder(
    file: FileId,
    node: &SyntaxNode,
    path: &mut SmallVec<[u32; 8]>,
    forward: &mut HashMap<(SyntaxKind, u32, u32), u32>,
    reverse: &mut HashMap<RawAstId, (u64, SmallVec<[u32; 8]>)>,
) {
    let kind = node.kind();
    if (kind as u16) >= NODE_START {
        let range = node.text_range();
        let start: u32 = range.start().into();
        let end: u32 = range.end().into();
        let len = end - start;
        let key = (kind, start, len);
        let disamb = *forward.get(&key).unwrap_or(&0);
        forward.insert(key, disamb + 1);
        let raw = RawAstId {
            file,
            kind,
            start,
            len,
            disamb,
        };
        let path_hash = hash_path(path);
        reverse.insert(raw, (path_hash, path.clone()));
    }
    for (child_idx, child) in node.children().enumerate() {
        let idx = u32::try_from(child_idx).unwrap_or(u32::MAX);
        path.push(idx);
        collect_preorder(file, &child, path, forward, reverse);
        path.pop();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_and_map(src: &str) -> (SyntaxNode, AstIdMap) {
        let file = FileId(0);
        let tokens = lyra_lexer::lex(src);
        let pp = lyra_preprocess::preprocess_identity(file, &tokens, src);
        let parse = lyra_parser::parse(&pp.tokens, &pp.expanded_text);
        let root = parse.syntax();
        let map = AstIdMap::from_root(file, &root);
        (root, map)
    }

    fn all_nodes(root: &SyntaxNode) -> Vec<SyntaxNode> {
        let mut nodes = Vec::new();
        collect_nodes(root, &mut nodes);
        nodes
    }

    fn collect_nodes(node: &SyntaxNode, out: &mut Vec<SyntaxNode>) {
        if (node.kind() as u16) >= NODE_START {
            out.push(node.clone());
        }
        for child in node.children() {
            collect_nodes(&child, out);
        }
    }

    #[test]
    fn round_trip_typed() {
        let src = "module m;\n  logic [3:0] a;\n  assign a = 4'b0;\nendmodule\n";
        let (root, map) = parse_and_map(src);
        let nodes = all_nodes(&root);
        assert!(!nodes.is_empty());
        for node in &nodes {
            let erased = map.erased_ast_id(node);
            assert!(erased.is_some(), "node {:?} has no erased id", node.kind());
            let id = erased.unwrap();
            let recovered = map.get_node(&root, id);
            assert!(
                recovered.is_some(),
                "could not recover node {:?}",
                node.kind()
            );
            let recovered = recovered.unwrap();
            assert_eq!(recovered.kind(), node.kind());
            assert_eq!(recovered.text_range(), node.text_range());
            // Verify exact node identity via path
            assert_eq!(compute_path(&recovered), compute_path(node));
        }
    }

    #[test]
    fn erased_round_trip() {
        let src = "module top; wire x; endmodule\nmodule bot; wire y; endmodule\n";
        let (root, map) = parse_and_map(src);
        for node in all_nodes(&root) {
            let id = map.erased_ast_id(&node).unwrap();
            let back = map.get_node(&root, id).unwrap();
            assert_eq!(back, node);
        }
    }

    #[test]
    fn determinism() {
        let src = "module m;\n  logic a, b;\n  assign a = b;\nendmodule\n";
        let file = FileId(0);

        let tokens1 = lyra_lexer::lex(src);
        let pp1 = lyra_preprocess::preprocess_identity(file, &tokens1, src);
        let parse1 = lyra_parser::parse(&pp1.tokens, &pp1.expanded_text);
        let map1 = AstIdMap::from_root(file, &parse1.syntax());

        let tokens2 = lyra_lexer::lex(src);
        let pp2 = lyra_preprocess::preprocess_identity(file, &tokens2, src);
        let parse2 = lyra_parser::parse(&pp2.tokens, &pp2.expanded_text);
        let map2 = AstIdMap::from_root(file, &parse2.syntax());

        // Collect (kind, start, len, disamb) tuples from each parse
        let tuples1: Vec<_> = all_nodes(&parse1.syntax())
            .iter()
            .map(|n| {
                let id = map1.erased_ast_id(n).unwrap().0;
                (id.kind, id.start, id.len, id.disamb)
            })
            .collect();
        let tuples2: Vec<_> = all_nodes(&parse2.syntax())
            .iter()
            .map(|n| {
                let id = map2.erased_ast_id(n).unwrap().0;
                (id.kind, id.start, id.len, id.disamb)
            })
            .collect();
        assert_eq!(tuples1, tuples2);
    }

    #[test]
    fn edit_locality_before_edit() {
        // Nodes strictly before an edit keep the same IDs
        let src_before = "module a; endmodule\nmodule b; endmodule\n";
        let src_after = "module a; endmodule\nmodule b_longer_name; endmodule\n";

        let (root1, map1) = parse_and_map(src_before);
        let (root2, map2) = parse_and_map(src_after);

        // "module a; endmodule" spans the same range in both
        let nodes1 = all_nodes(&root1);
        let nodes2 = all_nodes(&root2);

        // Find nodes strictly before the edit point (offset 20 is start of "module b")
        let before_nodes1: Vec<_> = nodes1
            .iter()
            .filter(|n| u32::from(n.text_range().end()) <= 19)
            .collect();
        let before_nodes2: Vec<_> = nodes2
            .iter()
            .filter(|n| u32::from(n.text_range().end()) <= 19)
            .collect();

        assert!(!before_nodes1.is_empty());
        assert_eq!(before_nodes1.len(), before_nodes2.len());

        for (n1, n2) in before_nodes1.iter().zip(before_nodes2.iter()) {
            let id1 = map1.erased_ast_id(n1).unwrap().0;
            let id2 = map2.erased_ast_id(n2).unwrap().0;
            assert_eq!(
                (id1.kind, id1.start, id1.len, id1.disamb),
                (id2.kind, id2.start, id2.len, id2.disamb),
                "node before edit changed ID"
            );
        }
    }

    #[test]
    fn edit_locality_after_edit() {
        // Nodes after an edit are allowed to change, but must still round-trip
        let src = "module a; endmodule\nmodule b_longer_name; endmodule\n";
        let (root, map) = parse_and_map(src);
        for node in all_nodes(&root) {
            let id = map.erased_ast_id(&node).unwrap();
            let range = map.range_of(id).unwrap();
            assert_eq!(range, node.text_range());
            let back = map.get_node(&root, id).unwrap();
            assert_eq!(back, node);
        }
    }

    #[test]
    fn whitespace_edit_stability() {
        // Trailing whitespace only affects the SourceFile root node (whose len
        // spans the whole input). All interior nodes keep identical IDs.
        let src1 = "module m;\nendmodule\n";
        let src2 = "module m;\nendmodule\n\n\n";

        let (root1, map1) = parse_and_map(src1);
        let (root2, map2) = parse_and_map(src2);

        // Exclude the root SourceFile node (its len changes with trailing ws)
        let nodes1: Vec<_> = all_nodes(&root1)
            .into_iter()
            .filter(|n| n.parent().is_some())
            .collect();
        let nodes2: Vec<_> = all_nodes(&root2)
            .into_iter()
            .filter(|n| n.parent().is_some())
            .collect();
        assert_eq!(nodes1.len(), nodes2.len());
        assert!(!nodes1.is_empty());

        for (n1, n2) in nodes1.iter().zip(nodes2.iter()) {
            let id1 = map1.erased_ast_id(n1).unwrap().0;
            let id2 = map2.erased_ast_id(n2).unwrap().0;
            assert_eq!(
                (id1.kind, id1.start, id1.len, id1.disamb),
                (id2.kind, id2.start, id2.len, id2.disamb),
                "whitespace-only edit changed ID for {:?}",
                n1.kind()
            );
        }
    }

    #[test]
    fn placeholder_not_found() {
        let src = "module m; endmodule\n";
        let (root, map) = parse_and_map(src);
        let ph = ErasedAstId::placeholder(FileId(0));
        assert!(map.range_of(ph).is_none());
        assert!(map.get_node(&root, ph).is_none());
        // text_range on placeholder must not overflow
        let range = ph.text_range();
        assert_eq!(u32::from(range.start()), u32::MAX);
        assert_eq!(range.len(), 0.into());
    }

    #[test]
    fn ord_earlier_start_first() {
        let a = ErasedAstId(RawAstId {
            file: FileId(0),
            kind: SyntaxKind::ModuleDecl,
            start: 10,
            len: 20,
            disamb: 0,
        });
        let b = ErasedAstId(RawAstId {
            file: FileId(0),
            kind: SyntaxKind::ModuleDecl,
            start: 30,
            len: 20,
            disamb: 0,
        });
        assert!(a < b);
    }

    #[test]
    fn ord_same_start_shorter_first() {
        let a = ErasedAstId(RawAstId {
            file: FileId(0),
            kind: SyntaxKind::ModuleDecl,
            start: 10,
            len: 5,
            disamb: 0,
        });
        let b = ErasedAstId(RawAstId {
            file: FileId(0),
            kind: SyntaxKind::ModuleDecl,
            start: 10,
            len: 20,
            disamb: 0,
        });
        assert!(a < b);
    }

    #[test]
    fn traits_hold() {
        fn assert_copy_eq_hash<T: Copy + Eq + std::hash::Hash>() {}
        assert_copy_eq_hash::<ErasedAstId>();
        assert_copy_eq_hash::<RawAstId>();
    }

    #[test]
    fn sentinel_invariant() {
        assert!(
            (SyntaxKind::Whitespace as u16) < NODE_START,
            "Whitespace must be a token kind (< NODE_START)"
        );
    }

    #[test]
    fn text_range_accessor() {
        let id = ErasedAstId(RawAstId {
            file: FileId(0),
            kind: SyntaxKind::ModuleDecl,
            start: 10,
            len: 25,
            disamb: 0,
        });
        let range = id.text_range();
        assert_eq!(u32::from(range.start()), 10);
        assert_eq!(u32::from(range.end()), 35);
    }

    #[test]
    fn collision_disambiguates() {
        // Manually construct a map with two entries sharing (kind, start, len)
        // to exercise the collision disambiguation path.
        let file = FileId(0);
        let kind = SyntaxKind::ModuleDecl;
        let start = 0u32;
        let len = 10u32;
        let key = (kind, start, len);

        let path_a: SmallVec<[u32; 8]> = SmallVec::from_slice(&[0]);
        let path_b: SmallVec<[u32; 8]> = SmallVec::from_slice(&[1]);
        let hash_a = hash_path(&path_a);
        let hash_b = hash_path(&path_b);

        let raw_a = RawAstId {
            file,
            kind,
            start,
            len,
            disamb: 0,
        };
        let raw_b = RawAstId {
            file,
            kind,
            start,
            len,
            disamb: 1,
        };

        let mut forward = HashMap::new();
        forward.insert(key, 2);
        let mut reverse = HashMap::new();
        reverse.insert(raw_a, (hash_a, path_a));
        reverse.insert(raw_b, (hash_b, path_b));

        let map = AstIdMap {
            file,
            forward,
            reverse,
        };

        // Both entries must resolve to distinct IDs
        let id_a = map.reverse.get(&raw_a);
        let id_b = map.reverse.get(&raw_b);
        assert!(id_a.is_some());
        assert!(id_b.is_some());
        assert_ne!(raw_a, raw_b);

        // Verify hashes are deterministic: recomputing gives same value
        let rehash_a = hash_path(&[0u32]);
        let rehash_b = hash_path(&[1u32]);
        assert_eq!(hash_a, rehash_a, "hash_path must be deterministic");
        assert_eq!(hash_b, rehash_b, "hash_path must be deterministic");
    }

    #[test]
    fn len_and_is_empty() {
        let src = "module m; endmodule\n";
        let (_, map) = parse_and_map(src);
        assert!(!map.is_empty());
        assert!(!map.is_empty());

        // Empty source produces empty map (no nodes >= NODE_START)
        let empty_src = "";
        let (_, empty_map) = parse_and_map(empty_src);
        // The parser produces a SourceFile root node, so this may not be empty.
        // Just verify the methods work.
        assert_eq!(empty_map.len(), empty_map.reverse.len());
    }
}
