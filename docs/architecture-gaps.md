# Architecture Gaps

Known architectural limitations and follow-up improvements. These are not LRM feature gaps (those live in `docs/lrm/gaps.md`), but cross-cutting design issues where the current implementation shape limits future work.

When work on a feature exposes a broader architectural limitation, record it here instead of half-fixing it in a single feature PR. When a gap is addressed, remove the entry.

## Core architecture gaps

These are long-term limitations of the semantic data model itself.

### Semantic identity model lacks token-granular stable identity

The stable identity model currently covers AST nodes only (`Site` / `ErasedAstId`), not individual tokens. Any semantic product that needs to anchor a specific token -- a keyword, an operator, a time literal -- is forced into a weaker representation: raw text (`SmolStr`) without source identity, or ad hoc token rescans at consumption time.

This is not a local feature issue. It is a limitation of the identity model itself. Token-granular semantic products (operator anchoring, keyword-level diagnostics, time literal identity) cannot be represented with the same precision and stability as node-level products until the identity model is extended. Exposed by: timeunit/timeprecision collection (LRM 3.14.2.2), operator tokens in expressions.

## Layer and API shape gaps

These are worth fixing but are not foundational limitations of the semantic data model.

### Scope ownership model is asymmetric and lacks a typed canonical abstraction

Scope ownership is currently represented in two different ways:

- container definitions (`module`, `package`, `interface`, `program`, etc.) carry forward ownership through `DefEntry.scope: DefScope::Owned(ScopeId)`
- callable owners (`function`, `task`) rely on side tables (`scope_owners`, `owner_to_scope`) keyed by raw `Site`

This is not primarily a `NameGraph` or backdating problem. Scope ownership does not currently participate in `NameGraph`, and no production resolution path depends on ownership being embedded in `ScopeTree`. The current gap is that the model is incomplete and untyped:

- definition-owned scopes have a canonical forward link
- callable-owned scopes do not
- reverse ownership queries are expressed through raw `Site` rather than a typed `ScopeOwner` abstraction

Today this is mostly observable in tests and helper APIs, so it is not an acute production correctness issue. But it leaves scope ownership without a clean, uniform semantic model and will become a real limitation once production code needs callable-owner to owned-scope queries.

The long-term shape is:
- one canonical ownership model, not a split between `DefEntry` fields and raw `Site` side tables
- a typed owner abstraction (`ScopeOwner` or equivalent), rather than untyped `Site`
- symmetric treatment of definition and callable owners

This does **not** imply that ownership must be stored directly inside `Scope`, and it does **not** imply that ownership must live in `NameGraph`. Those are representation decisions to make after the canonical ownership model is defined. Exposed by: callable and container scope ownership recording, owner-based test helpers, lifetime tests that need owner disambiguation.
