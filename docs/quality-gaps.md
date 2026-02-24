# Quality Gaps

This file tracks non-LRM technical debt that risks determinism, incremental
stability, or architectural drift. These are not failing tests yet, but they
need explicit resolution and a ratchet to prevent regressions.

When a gap is identified, it should be described here and fixed in a
follow-up PR. The north star reference is in `docs/architecture.md`.

## Entries

1. Missing canonical syntax anchors + range leakage (blocks provenance / refactor tools)
   - Problem: Some semantic structs and diagnostics store `TextRange` directly, coupling semantics to presentation and preventing a clean provenance/spelling query layer.
   - Design smell: Definition-namespace entries (module, package, interface, program, primitive, config) are stored as `Symbol` entries but do not participate in the Symbol contract (no type computation, no lexical scope resolution, always `SymbolOrigin::TypeSpec`). They are defs wearing a symbol costume; their real identity is `GlobalDefId`.
   - Done:
     1. `def_ast` on Symbol -- non-optional declaration item anchor on every symbol.
     2. `name_ast` and `type_ast` on Symbol -- canonical name-site and type-spelling anchors. `symbol_to_decl` removed; `symbol_global_def` reads from `Symbol.def_ast` directly.
     3. Deleted `Symbol.def_range`, `Import.range`, `ExportDecl.range`, `EnumMemberDef.name_range`, `RecordField.name_range`. Added `ExportDecl.export_stmt_ast` and `RecordField.name_ast` as `ErasedAstId` anchors.
     4. `NameSpan` on Symbol, EnumMemberDef, RecordField, LocalDecl -- O(1) identifier token range captured at builder time via typed AST accessors. Deleted `span_index.rs` and `ident_range_of_name_site()`. Diagnostic consumers use `name_span.text_range()` directly.
     5. `NameSpan` non-optional with `INVALID` sentinel on all 4 structs. Builder produces `INVALID` + `InternalError` on parse recovery. All consumer `map_or_else` branching eliminated -- consumers call `.text_range()` directly.
     6. `Site` type alias (`= ErasedAstId`) in lyra-semantic. All semantic struct anchors use `Site` and `_site` suffix convention (`decl_site`, `name_site`, `type_site`, `name_ref_site`, `stmt_site`). Zero `ErasedAstId` references in lyra-semantic outside the alias definition.
   - Remaining:
     1. `SemanticDiag.range: TextRange` -- every diagnostic embeds a range. Migrating requires anchoring all ~30 diagnostic construction sites.
     2. `TypeCheckItem` -- ~20 `TextRange` fields. Scope explosion; needs batch migration.
     3. `InstanceDecl` -- 2 `TextRange` fields. Touches type resolution and member lookup.
     4. `EnumBase.range` -- `TypeRef` has no span; needs design work.
     5. `EnumVariantTarget.def_range` -- cascades into resolution and diagnostics.
     6. `EnumMemberDef.range_text_range` -- no obvious single anchor for `[N:M]` range spec.
     7. `DuplicateDefinition.original: TextRange` -- part of `SemanticDiag` migration.
     8. Extract definition-namespace entries from `SymbolTable` into their own first-class def structure.
   - Outcome: Enables future `TypeSpelling` / provenance queries derived purely from CST/AST without heuristics, preserving incrementality and determinism.

2. CST traversal in semantic producers (blocks clean layering)
   - Problem: ~12 locations in lyra-semantic perform raw CST traversal (`.children()`, `.descendants()`, `.children_with_tokens()`) outside the builder phase: `type_extract.rs`, `type_infer/mod.rs`, `type_check.rs`, `const_eval.rs`, `system_functions.rs`, `system_call_view.rs`, `literal.rs`, `record.rs`.
   - Design smell: Semantic queries re-walk CST to extract properties that should be cached in def structs or exposed via typed AST accessors.
   - CI enforcement: `tools/policy/check_cst_layering.py` prevents new CST usage in non-allowlisted modules.
   - Action:
     1. For syntactic classification checks (compound assign, assignment op detection in `type_check.rs`), extract to builder phase and store in symbol definitions.
     2. For type inference CST dependency (`type_infer/mod.rs`), add typed AST accessors in `lyra-ast` for expression forms (prefix, binary, conditional, replication, streaming).
     3. For `type_extract.rs`, cache extracted type properties in def structs.
   - Outcome: Semantic layer depends only on typed AST accessors and builder-extracted facts, not raw CST.

3. ~~Ambiguous `ErasedAstId` field names~~ -- CLOSED
   - All anchor fields use `_site` suffix convention: `Symbol.decl_site`/`name_site`/`type_site`, `EnumDef.enum_type_site`, `RecordDef.record_type_site`, `UseSite.name_ref_site`, `LocalDecl.decl_site`, `Import.import_stmt_site`, `ExportDecl.export_stmt_site`, `RealizedBinding.target_name_site`, `CoreResolution::Pkg { name_site }`. DefIndex maps keyed by `name_site_to_symbol`, `name_site_to_init_expr`, `enum_by_site`, `record_by_site`.
