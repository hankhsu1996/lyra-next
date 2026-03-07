# Architecture Gaps

Known architectural limitations and follow-up improvements. These are not LRM feature gaps (those live in `docs/lrm/gaps.md`), but cross-cutting design issues where the current conventions limit future work.

When work on a feature exposes a broader architectural limitation, record it here instead of half-fixing it in a single feature PR. When a gap is addressed, remove the entry.

## Semantic identity model: declaration vs token-level payloads

`Site` (`ErasedAstId`) provides stable identity for AST nodes but not for tokens. Semantic summary types that need to anchor individual tokens (e.g. time literals inside a `timeunit` declaration, operator tokens in expressions) currently store only raw text (`SmolStr`) without source identity. When the architecture gains token-level site identity, semantic payloads should be extended to carry it. Exposed by: timeunit/timeprecision collection (LRM 3.14.2.2).

## Builder APIs: typed AST vs raw SyntaxNode signatures

All `collect_*` functions in `builder_items.rs` take `&SyntaxNode` and re-cast internally to typed AST wrappers. A cleaner layering would have `builder.rs` perform the one cast from raw CST node to typed AST, then pass the typed wrapper to collection helpers. This would eliminate redundant casts and make the typed-AST boundary sharper. Exposed by: every `collect_*` function, most visibly in timeunit/timeprecision and modport collection.

## Deterministic container conventions in semantic indexes

`DefIndex` uses `HashMap` for all keyed maps (`scope_time_units`, `foreach_var_defs`, `modport_defs`, etc.). Iteration order over these maps is non-deterministic. Currently all access is point-lookup by key, so this is not a correctness issue, but if any future code iterates these maps the non-determinism becomes observable. The project should adopt a deterministic map convention (e.g. `IndexMap`, sorted `Vec`, or explicit "no iteration" contract) for semantic summary containers. Exposed by: `scope_time_units: HashMap<ScopeId, ScopeTimeUnits>` in `DefIndex`.

## Test scope resolution: kind-based vs structural lookup

Unit tests for scope-keyed semantic data use `find_scope_by_kind()` which searches by `ScopeKind`. This is adequate when test inputs contain exactly one scope of each kind, but becomes ambiguous with multiple same-kind scopes (e.g. two modules in one file). A more robust approach would resolve the exact owner scope from the parsed file structure. Exposed by: timeunit/timeprecision builder tests.
