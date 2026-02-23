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
`ErasedAstId` which wraps `RawAstId { file: FileId, index: u32 }`.

`AstIdMap` assigns `index` by preorder traversal of the syntax tree.
It stores a forward table of entries, each containing:
- `SyntaxKind`
- `TextRange`
- `path` (child-index path from the root)

`AstIdMap::ast_id` looks up a node by `(kind, range)` and disambiguates
collisions using `path`. This ensures nodes with identical ranges in
error-recovery scenarios can still be distinguished.

### Stability boundaries for AstId
`AstId` stability is tied to the syntax tree shape. Any edit that changes
the preorder traversal before a node can shift its `index`, even if the
node's range is unchanged. This is acceptable for current usage but is a
known quality gap tracked in `docs/quality-gaps.md`.

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
