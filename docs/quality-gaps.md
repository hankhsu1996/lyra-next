# Quality Gaps

This file tracks non-LRM technical debt that risks determinism, incremental
stability, or architectural drift. These are not failing tests yet, but they
need explicit resolution and a ratchet to prevent regressions.

When a gap is identified, it should be described here and fixed in a
follow-up PR. The north star reference is in `docs/architecture.md`.

## Entries

1. Missing canonical syntax anchors + range leakage (blocks provenance / refactor tools)
   - Problem: Semantic outputs lack total, canonical `ErasedAstId` anchors for definition sites and type-spelling sites. `DefIndex.symbol_to_decl: Option<ErasedAstId>` is conditionally backfilled (always `Option`, silent dropout on parse errors) and points at the declarator rather than the type syntax subtree, forcing heuristic parent-walks to rediscover spelling. Many semantic structs and diagnostics store `TextRange` directly, coupling semantics to presentation and preventing a clean provenance/spelling query layer.
   - Evidence:
     - `Symbol` has `def_range: TextRange` but no `ErasedAstId` of its own.
     - `symbol_to_decl` is partial and declarator-granularity; no `symbol_to_typespec` exists.
     - `SemanticDiag`, `TypeCheckItem`, `RecordField`, `EnumMemberDef`, `EnumBase`, `EnumVariantTarget`, `InstanceDecl` all store `TextRange` directly. Only `EnumValueDiag` follows the anchor-only pattern.
   - Design smell: Definition-namespace entries (module, package, interface, program, primitive, config) are stored as `Symbol` entries but do not participate in the Symbol contract (no type computation, no lexical scope resolution, always `SymbolOrigin::TypeSpec`). They are defs wearing a symbol costume; their real identity is `GlobalDefId`. This forces `symbol_to_decl` to be `Option` for all symbols and inflates the symbol table with entries that have a different lifecycle.
   - Action:
     1. Make anchors first-class and total: every syntax-declared symbol/entity must carry a non-optional `def_ast: ErasedAstId` and `name_ast: ErasedAstId`.
     2. Add dedicated `type_ast: ErasedAstId` for declarations with type spelling (variables, ports, typedefs, fields).
     3. Stop introducing new `TextRange` fields in semantic data; migrate existing ones to `ErasedAstId` incrementally and compute ranges only in diagnostics lowering / presentation.
     4. Consider extracting definition-namespace entries from the `SymbolTable` into their own first-class def structure with non-optional anchors, removing the pressure on the Symbol model.
   - Phases:
     1. `def_ast` on Symbol -- non-optional declaration item anchor on every symbol. Internal error channel for broken invariants. (done)
     2. `type_ast` for type-spelling anchors + migrate `GlobalDefId` to proper def-site anchors for definition-namespace constructs + deprecate `symbol_to_decl`.
     3. TextRange migration in semantic structs -- replace remaining `TextRange` fields with `ErasedAstId` anchors, compute ranges only in diagnostics lowering / presentation.
     4. Extract definition-namespace entries from `SymbolTable` into their own first-class def structure with non-optional anchors.
   - Outcome: Enables future `TypeSpelling` / provenance queries derived purely from CST/AST without heuristics, preserving incrementality and determinism.
