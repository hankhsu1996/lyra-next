# Quality Gaps

This file tracks non-LRM technical debt that risks determinism, incremental
stability, or architectural drift. These are not failing tests yet, but they
need explicit resolution and a ratchet to prevent regressions.

When a gap is identified, it should be described here and fixed in a
follow-up PR. The north star reference is in `docs/architecture.md`.

## Entries

1. Missing canonical syntax anchors + range leakage (blocks provenance / refactor tools)
   - Problem: Some semantic structs and diagnostics store `TextRange` directly, coupling semantics to presentation and preventing a clean provenance/spelling query layer.
   - ~~Design smell: Definition-namespace entries stored as `Symbol` entries but do not participate in the Symbol contract.~~ Resolved: def-namespace items now stored as `DefEntry` keyed by `GlobalDefId`.
   - Done:
     1. `def_ast` on Symbol -- non-optional declaration item anchor on every symbol.
     2. `name_ast` and `type_ast` on Symbol -- canonical name-site and type-spelling anchors. `symbol_to_decl` removed; `symbol_global_def` reads from `Symbol.def_ast` directly.
     3. Deleted `Symbol.def_range`, `Import.range`, `ExportDecl.range`, `EnumMemberDef.name_range`, `RecordField.name_range`. Added `ExportDecl.export_stmt_ast` and `RecordField.name_ast` as `ErasedAstId` anchors.
     4. `NameSpan` on Symbol, EnumMemberDef, RecordField, LocalDecl -- O(1) identifier token range captured at builder time via typed AST accessors. Deleted `span_index.rs` and `ident_range_of_name_site()`. Diagnostic consumers use `name_span.text_range()` directly.
     5. `NameSpan` non-optional with `INVALID` sentinel on all 4 structs. Builder produces `INVALID` + `InternalError` on parse recovery. All consumer `map_or_else` branching eliminated -- consumers call `.text_range()` directly.
     6. `Site` type alias (`= ErasedAstId`) in lyra-semantic. All semantic struct anchors use `Site` and `_site` suffix convention (`decl_site`, `name_site`, `type_site`, `name_ref_site`, `stmt_site`). Zero `ErasedAstId` references in lyra-semantic outside the alias definition.
   - Remaining:
     1. ~~`SemanticDiag.range: TextRange`~~ -- DONE. Replaced with `DiagSpan` anchor (`Site`/`Name`/`Token`). All ~15 construction sites migrated. Policy ratchet: `tools/policy/check_diag_textrange.py`.
     2. ~~`TypeCheckItem` -- ~20 `TextRange` fields~~ -- DONE. All `TextRange` fields replaced with `Site` (expression/statement nodes) or `NameSpan` (identifier tokens). Lowering extracts `TextRange` from anchors at diagnostic time.
     3. ~~`InstanceDecl` -- 2 `TextRange` fields~~ -- DONE. Removed dead `name_range` and `type_name_range`; instance name available via `Symbol.name_span`/`name_site`, type name via `UseSite.name_ref_site`.
     4. ~~`EnumBase.range`~~ -- DONE. `TypeRef` now carries `type_site: Site`; `EnumBase.range` replaced with `type_site: Site`. Enum base diagnostics use `enum_def.base.type_site` directly.
     5. ~~`EnumVariantTarget.def_range`~~ -- DONE. Replaced with `name_site: Site` (member's stable AST anchor). `ExpandedVariant.def_range` also replaced.
     6. ~~`EnumMemberDef.range_text_range`~~ -- DONE. Replaced with `range_site: Option<Site>` anchored to the range spec AST node. Range-spec diagnostics use `range_site` as primary when available.
     7. ~~`DuplicateDefinition.original: TextRange`~~ -- DONE. Replaced with `original_primary: DiagSpan` + `original_label: Option<DiagSpan>`.
     8. ~~Extract definition-namespace entries from `SymbolTable`~~ -- DONE. Definition-namespace items (module, package, interface, program, primitive, config) stored as first-class `DefEntry` keyed by `GlobalDefId` in `DefIndex.def_entries`. No `Symbol` created for def-namespace declarations. Resolution uses `ResolvedTarget::Def(GlobalDefId)`. Interface scope/name lookups use `def_entry()` directly.
     9. ~~Record field type errors lack type-reference precision~~ -- DONE. `TypeRef::Named`/`Qualified` carry `type_site: Site`; field type error diagnostics use `type_site` as primary anchor.
     10. ~~Modport port diagnostics lack `NameSpan` label~~ -- DONE. `ModportEntry.span` replaced with `name_span: NameSpan`. `DuplicateDefinition` and `UnresolvedName` diagnostics now carry `DiagSpan::Name` labels.
   - Outcome: Enables future `TypeSpelling` / provenance queries derived purely from CST/AST without heuristics, preserving incrementality and determinism.

2. CST traversal in semantic producers (blocks clean layering)
   - Problem: Residual raw CST traversal in tier-3 semantic modules. Category A (expression-form accessors) is complete; Category B (`type_extract.rs`, `record.rs`) remains.
   - Progress: Added typed AST accessors in `lyra-ast` (`Expr`, `TfArg`, `BinExpr`, `PrefixExpr`, `CondExpr`, `ConcatExpr`, `ReplicExpr`, `StreamExpr`, `CallExpr`, `SystemTfCall`, `AssignStmt`, `ContinuousAssign`, `Declarator`, `Literal`). Migrated `const_eval.rs`, `type_infer/mod.rs`, `type_check.rs`, `system_functions.rs`, `literal.rs`. Deleted `expr_helpers.rs`, `system_call_view.rs`, `syntax_helpers.rs`.
   - CI enforcement: `tools/policy/check_cst_layering.py` prevents new CST usage in non-allowlisted modules. Tier-3 modules have ceiling enforcement via `CST_CALL_CEILINGS`.
   - Remaining:
     1. Category B: `type_extract.rs` (16 violations) -- cache extracted type properties in def structs.
     2. Residual Expression-unwrapping `first_child()` calls in `const_eval.rs`, `type_infer/mod.rs`, `literal.rs` -- could move to an `Expr::from_expression_wrapper()` accessor.
     3. `type_check.rs` tree walks in `walk_for_checks` (7 violations) -- inherent to recursive checking.
   - Outcome: Semantic layer depends only on typed AST accessors and builder-extracted facts, not raw CST.

3. ~~Ambiguous `ErasedAstId` field names~~ -- CLOSED
   - All anchor fields use `_site` suffix convention: `Symbol.decl_site`/`name_site`/`type_site`, `EnumDef.enum_type_site`, `RecordDef.record_type_site`, `UseSite.name_ref_site`, `LocalDecl.decl_site`, `Import.import_stmt_site`, `ExportDecl.export_stmt_site`, `RealizedBinding.target_name_site`, `CoreResolution::Pkg { name_site }`. DefIndex maps keyed by `name_site_to_symbol`, `name_site_to_init_expr`, `enum_by_site`, `record_by_site`.
