# Quality Gaps

This file tracks non-LRM technical debt that risks determinism, incremental
stability, or architectural drift. These are not failing tests yet, but they
need explicit resolution and a ratchet to prevent regressions.

When a gap is identified, it should be described here and fixed in a
follow-up PR. The north star reference is in `docs/architecture.md`.

## Entries

1. AstId stability vs docs mismatch
   - Risk: AstId is preorder-indexed, but docs claim (kind, offset) identity.
     This can cause widespread ID churn after unrelated edits.
   - Evidence: crates/lyra-ast/src/ast_id.rs and docs/architecture.md.
   - Action: Decide whether to change AstId encoding or update docs and add
     tests that define acceptable churn boundaries.

2. EnumId/RecordId ordinal stability depends on collection order
   - Risk: ordinals are assigned during DefIndex build and rely on traversal
     order. If that order changes, IDs can shift.
   - Evidence: crates/lyra-semantic/src/builder.rs (enum_ordinals, record_ordinals).
   - Action: Confirm traversal determinism or derive ordinals from a stable key
     (e.g., syntax offset) and add regression tests.

3. Provenance model is not explicit in semantic outputs
   - Risk: Formatter/refactor tools need "how it was written" metadata,
     but semantics currently encode meaning only (limited SymbolOrigin).
   - Evidence: SymbolOrigin exists in crates/lyra-semantic/src/record.rs but
     there is no dedicated provenance struct for surface syntax choices.
   - Action: Define a provenance layer (or metadata attached to semantic
     entities) that captures surface syntax without forking semantics.

4. AstId churn amplifies into resolution and diagnostics
   - Risk: ResolveIndex, DefIndex maps, enum diagnostics, and const-eval
     anchors are keyed by ErasedAstId. If AstId is preorder-indexed, any
     tree-shape change before a node can force whole-file re-resolution
     and churn diagnostics, even when source ranges are unchanged.
   - Evidence: crates/lyra-semantic/src/resolve_index.rs,
     crates/lyra-semantic/src/def_index.rs,
     crates/lyra-semantic/src/enum_def.rs,
     crates/lyra-db/src/resolve_at.rs.
   - Action: After deciding the AstId model, add tests that assert
     resolution/backdating stability on edits that do not change node
     ranges; consider migrating keys to a stable offset-based identity.

5. AstIdMap path fallback can silently collapse identity
   - Risk: compute_path uses unwrap_or(0) when a child position is not
     found. That can collapse distinct nodes to the same path in
     corruption or recovery scenarios, producing duplicate identity.
   - Evidence: crates/lyra-ast/src/ast_id.rs (compute_path).
   - Action: Replace unwrap_or(0) with a tracked error path or invariant
     check that produces a deterministic but unique sentinel, and add
     a test for recovery cases with duplicate (kind, range).
