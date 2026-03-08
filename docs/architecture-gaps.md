# Architecture Gaps

Known architectural limitations and follow-up improvements. These are not LRM feature gaps (those live in `docs/lrm/gaps.md`), but cross-cutting design issues where the current implementation shape limits future work.

When work on a feature exposes a broader architectural limitation, record it here instead of half-fixing it in a single feature PR. When a gap is addressed, remove the entry.

## Semantic identity model: declaration vs token-level payloads

`Site` (`ErasedAstId`) provides stable identity for AST nodes but not for tokens. Semantic summary types that need to anchor individual tokens (e.g. time literals inside a `timeunit` declaration, operator tokens in expressions) currently store only raw text (`SmolStr`) without source identity. When the architecture gains token-level site identity, semantic payloads should be extended to carry it. Exposed by: timeunit/timeprecision collection (LRM 3.14.2.2).

## Builder APIs: typed AST vs raw SyntaxNode signatures

All `collect_*` functions in `builder_items.rs` take `&SyntaxNode` and re-cast internally to typed AST wrappers. A cleaner layering would have `builder.rs` perform the one cast from raw CST node to typed AST, then pass the typed wrapper to collection helpers. This would eliminate redundant casts and make the typed-AST boundary sharper. Exposed by: every `collect_*` function, most visibly in timeunit/timeprecision and modport collection.

## Declaration lifetime token scanning vs typed header accessors

`decl_lifetime_token` in `nodes_decl.rs` scans the raw token stream after a keyword to find `automatic`/`static`. This works but encodes structure assumptions as "find token after keyword." The long-term shape is typed header-modifier accessors that parse declaration headers structurally, shared across container and callable declaration kinds. Exposed by: LRM 6.21 lifetime inheritance (module/interface/program/package/function/task lifetime tokens).

## Test scope resolution: kind-based vs structural lookup

Unit tests for scope-keyed semantic data use `find_scope_by_kind()` which searches by `ScopeKind`. This is adequate when test inputs contain exactly one scope of each kind, but becomes ambiguous with multiple same-kind scopes (e.g. two modules in one file). A more robust approach would resolve the exact owner scope from the parsed file structure. Exposed by: timeunit/timeprecision builder tests.

## Scope ownership as side metadata

Scope ownership (`scope_owners: HashMap<ScopeId, Site>`) lives in `DefIndex` as a side table rather than on the `Scope` struct itself. Ownership is structural data that describes what declaration created a scope. The long-term shape is `Scope { parent, kind, owner: Option<ScopeOwner> }` where `ScopeOwner` is a typed discriminant (e.g. `Symbol(SymbolId)`, `Def(GlobalDefId)`). This is blocked by the `NameGraph` offset-independence constraint: `ScopeTree` is embedded in `NameGraph` for Salsa incremental backdating, and `Site` contains text offsets. A proper fix requires either making the owner representation offset-free or restructuring `NameGraph` to separate offset-dependent scope metadata. Exposed by: callable and container scope ownership recording.

## Scope owner reverse lookup is linear scan

`DefIndex::find_scope_by_owner(Site)` does a linear scan over `scope_owners`. This is adequate for tests but not viable as a hot-path production API. If owner lookup becomes performance-critical, store both directions explicitly (scope-to-owner and owner-to-scope) or move ownership onto the scope struct (see "Scope ownership as side metadata" above). Exposed by: owner-anchored lifetime test helpers.

## Test owner lookup is name-based

`find_owner_scope(name)` in lifetime tests resolves an owner declaration by name, then looks up its owning scope. This is not fully exact if a file contains two owners with the same name in different containers (e.g. two modules each with a function `f`). The long-term shape is structural resolution from AST/site, then querying the def index for the owning scope. Name-based lookup is acceptable for current single-container test inputs. Exposed by: owner-anchored lifetime test helpers.
