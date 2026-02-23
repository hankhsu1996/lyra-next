# Quality Gaps

This file tracks non-LRM technical debt that risks determinism, incremental
stability, or architectural drift. These are not failing tests yet, but they
need explicit resolution and a ratchet to prevent regressions.

When a gap is identified, it should be described here and fixed in a
follow-up PR. The north star reference is in `docs/architecture.md`.

## Entries

1. Provenance model is not explicit in semantic outputs
   - Risk: Formatter/refactor tools need "how it was written" metadata,
     but semantics currently encode meaning only (limited SymbolOrigin).
   - Evidence: SymbolOrigin exists in crates/lyra-semantic/src/record.rs but
     there is no dedicated provenance struct for surface syntax choices.
   - Action: Define a provenance layer (or metadata attached to semantic
     entities) that captures surface syntax without forking semantics.
