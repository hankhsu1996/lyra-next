# Architecture Gaps

Known architectural limitations and follow-up improvements. These are not LRM feature gaps (those live in `docs/lrm/gaps.md`), but cross-cutting design issues where the current implementation shape limits future work.

When work on a feature exposes a broader architectural limitation, record it here instead of half-fixing it in a single feature PR. When a gap is addressed, remove the entry.

## Semantic identity model: declaration vs token-level payloads

`Site` (`ErasedAstId`) provides stable identity for AST nodes but not for tokens. Semantic summary types that need to anchor individual tokens (e.g. time literals inside a `timeunit` declaration, operator tokens in expressions) currently store only raw text (`SmolStr`) without source identity. When the architecture gains token-level site identity, semantic payloads should be extended to carry it. Exposed by: timeunit/timeprecision collection (LRM 3.14.2.2).

## Builder APIs: typed AST vs raw SyntaxNode signatures

All `collect_*` functions in `builder_items.rs` take `&SyntaxNode` and re-cast internally to typed AST wrappers. A cleaner layering would have `builder.rs` perform the one cast from raw CST node to typed AST, then pass the typed wrapper to collection helpers. This would eliminate redundant casts and make the typed-AST boundary sharper. Exposed by: every `collect_*` function, most visibly in timeunit/timeprecision and modport collection.

## Deterministic container conventions in semantic indexes

`DefIndex` uses `HashMap` for all keyed maps (`scope_time_units`, `foreach_var_defs`, `modport_defs`, etc.). Iteration order over these maps is non-deterministic. Currently all access is point-lookup by key, so this is not a correctness issue, but if any future code iterates these maps the non-determinism becomes observable. The project should adopt a deterministic map convention (e.g. `IndexMap`, sorted `Vec`, or explicit "no iteration" contract) for semantic summary containers. Exposed by: `scope_time_units: HashMap<ScopeId, ScopeTimeUnits>` in `DefIndex`.

## Declaration lifetime token scanning vs typed header accessors

`decl_lifetime_token` in `nodes_decl.rs` scans the raw token stream after a keyword to find `automatic`/`static`. This works but encodes structure assumptions as "find token after keyword." The long-term shape is typed header-modifier accessors that parse declaration headers structurally, shared across container and callable declaration kinds. Exposed by: LRM 6.21 lifetime inheritance (module/interface/program/package/function/task lifetime tokens).

## Declarator lifetime context passed as raw Lifetime values

`collect_declarators` takes an `unqualified_lifetime: Lifetime` parameter. Call sites pass raw constants (`Lifetime::Static` for container items, `Lifetime::Automatic` for for-init). This works but the API exposes the symptom, not the semantic reason. A cleaner shape would use semantic modes (e.g. split helpers by role: `collect_container_declarators`, `collect_local_declarators`, `collect_for_init_declarators`) or a typed mode enum. Exposed by: LRM 6.21 callable-local lifetime inheritance.

## Lifetime test helpers not truly scope-anchored

`all_symbol_lifetimes` in lifetime tests checks the multiset of lifetimes for a symbol name, not which specific declaration got which lifetime. This leaves room for false positives when the wrong declaration gets the wrong lifetime but the set coincidentally balances. Long-term want helpers that identify declarations by containing scope or declaration site. Exposed by: scope-anchored lifetime tests (same-name variables across container and callable scopes).

## Test scope resolution: kind-based vs structural lookup

Unit tests for scope-keyed semantic data use `find_scope_by_kind()` which searches by `ScopeKind`. This is adequate when test inputs contain exactly one scope of each kind, but becomes ambiguous with multiple same-kind scopes (e.g. two modules in one file). A more robust approach would resolve the exact owner scope from the parsed file structure. Exposed by: timeunit/timeprecision builder tests.
