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

(No current gaps in this category.)
