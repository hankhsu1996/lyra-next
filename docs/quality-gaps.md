# Quality Gaps

This file tracks non-LRM technical debt that risks determinism, incremental
stability, or architectural drift. These are not failing tests yet, but they
need explicit resolution and a ratchet to prevent regressions.

When a gap is identified, it should be described here and fixed in a
follow-up PR. The north star reference is in `docs/architecture.md`.

## Entries

1. Missing canonical syntax anchors + range leakage (blocks provenance / refactor tools)
   - Problem: Many semantic structs and diagnostics store `TextRange` directly, coupling semantics to presentation and preventing a clean provenance/spelling query layer.
   - Evidence:
     - `SemanticDiag`, `TypeCheckItem`, `RecordField`, `EnumMemberDef`, `EnumBase`, `EnumVariantTarget`, `InstanceDecl` all store `TextRange` directly. Only `EnumValueDiag` follows the anchor-only pattern.
   - Design smell: Definition-namespace entries (module, package, interface, program, primitive, config) are stored as `Symbol` entries but do not participate in the Symbol contract (no type computation, no lexical scope resolution, always `SymbolOrigin::TypeSpec`). They are defs wearing a symbol costume; their real identity is `GlobalDefId`.
   - Action:
     1. Stop introducing new `TextRange` fields in semantic data; migrate existing ones to `ErasedAstId` incrementally and compute ranges only in diagnostics lowering / presentation.
     2. Consider extracting definition-namespace entries from the `SymbolTable` into their own first-class def structure with non-optional anchors, removing the pressure on the Symbol model.
   - Done:
     1. `def_ast` on Symbol -- non-optional declaration item anchor on every symbol. Internal error channel for broken invariants.
     2. `name_ast` and `type_ast` on Symbol -- canonical name-site and type-spelling anchors. `symbol_to_decl` removed; `symbol_global_def` now reads from `Symbol.def_ast` directly.
   - Remaining:
     1. TextRange migration in semantic structs -- replace remaining `TextRange` fields with `ErasedAstId` anchors, compute ranges only in diagnostics lowering / presentation.
     2. Extract definition-namespace entries from `SymbolTable` into their own first-class def structure with non-optional anchors.
     3. `GlobalDefId` is still used as a generic cross-file anchor in `PackageScope` value/type-namespace members. Follow-up PR will change `PackageScope` to use `ErasedAstId` directly for member anchors, restricting `GlobalDefId` to definition-namespace constructs only.
   - Outcome: Enables future `TypeSpelling` / provenance queries derived purely from CST/AST without heuristics, preserving incrementality and determinism.
