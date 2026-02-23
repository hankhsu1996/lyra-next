# ID Model

This document defines the identity model used across the Lyra Next pipeline.
IDs are small, copyable values that allow semantic data to be stored without
borrowing. All semantic storage uses IDs, not references.

The core goals are:
- Stable identity within a file across incremental re-analysis.
- Deterministic ordering and comparison across runs.
- No borrowed semantics across query boundaries.

## Source Identity

### FileId
`FileId` is an opaque `u32` identifying a source file. It is stored on
`SourceFile` (Salsa input) and on every per-file identity.

### SourceFile (Salsa input)
`SourceFile` is a tracked input with:
- `file_id: FileId`
- `text: String`
- `include_map: Vec<(String, SourceFile)>`

The `file_id` is stable; changes to `text` invalidate downstream queries
for that file only.

## AST Identity

### AstId and ErasedAstId
`AstId<N>` is the typed identity for an AST node. It is backed by
`ErasedAstId` which wraps `RawAstId { file, kind, start, len, disamb }`.

`RawAstId` encodes the node's `SyntaxKind`, byte offset (`start`), span
length (`len`), and a disambiguator (`disamb`) for the rare case where
multiple nodes share the same `(kind, start, len)` triple (e.g.
error-recovery).

`AstIdMap` is built by preorder traversal. The forward table maps
`(kind, start, len)` to a count of nodes with that triple. The reverse
table maps each `RawAstId` to a `(path_hash, child-index path)` pair
for exact node recovery.

`AstIdMap::ast_id` performs an O(1) hash lookup in the common case
(count == 1). For collisions, it computes the node's path from root and
scans disambiguator values 0..count.

### Stability boundaries for AstId
IDs are stable for nodes whose `(kind, start_offset, len)` is unchanged.
Edits before a node that shift its byte offset will change its ID. The
key property: tree-shape changes (error recovery reshaping, wrapper node
insertion) that do not move byte offsets no longer cause ID churn.
Whitespace-only edits that do not change any node's start offset keep
all IDs stable.

## Per-file Semantic Identity

### SymbolId and ScopeId
`SymbolId` and `ScopeId` are per-file indices (`u32`) into frozen tables:
- `SymbolTable` (symbols)
- `ScopeTree` (scopes)

IDs are assigned in builder order and are stable for a given build of
`DefIndex`. They are not stable across edits that reorder the collected
definitions or scopes.

### LocalDeclId, ImportDeclId, ExportDeclId
These are lightweight per-file identities that pair `ScopeId` with an
ordinal within the scope:
- `LocalDeclId { scope, ordinal }`
- `ImportDeclId { scope, ordinal }`
- `ExportDeclId { scope, ordinal }`

Ordinals are assigned in the def-index builder using deterministic
preorder traversal of the syntax tree.

## Cross-file Semantic Identity

### GlobalDefId
`GlobalDefId` wraps `ErasedAstId` to represent a cross-file definition
identity. It inherits the stability properties of `AstId`.

### GlobalSymbolId
`GlobalSymbolId` is `{ file: FileId, local: SymbolId }`. It is stable as
long as `SymbolId` stability is preserved for the owning file.

### EnumId and RecordId
`EnumId` and `RecordId` are composite identities:
- `EnumId { file: FileId, owner: Option<SmolStr>, ordinal: u32 }`
- `RecordId { file: FileId, owner: Option<SmolStr>, ordinal: u32 }`

`owner` is the enclosing symbol name (if any). `ordinal` is assigned during
`DefIndex` construction and depends on traversal order within a given owner.

## Determinism

Determinism relies on:
- Stable traversal order in the builder.
- Explicit sorting of lists used for binary search or diagnostics.
- Avoiding iteration-order dependence on `HashMap` and `HashSet` outputs.

When ordering matters, data is sorted before it is stored or returned.
