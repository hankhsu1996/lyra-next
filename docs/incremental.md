# Incremental Computation

Lyra uses [Salsa](https://github.com/salsa-rs/salsa) for demand-driven incremental computation. This document describes the query dependency graph, input mutation model, and the testing strategy for verifying cache behavior.

## Query Dependency DAG

Every `SourceFile` is a `#[salsa::input]` with two fields: `file_id` and `text`. Salsa automatically tracks which queries read which inputs. The core query chain is:

```
SourceFile.text
  |
  +-- lex_file       (tokenize)
  |     |
  |     +-- preprocess_file   (expand macros, resolve includes)
  |           |
  |           +-- parse_file          (build green tree)
  |           |     |
  |           |     +-- ast_id_map         (stable node identity)
  |           |     +-- file_diagnostics   (parse errors -> Diagnostic)
  |           |
  |           +-- source_map          (preprocessor span mapping)
  |           +-- include_graph       (include dependencies)
  |
  +-- line_index     (byte offset -> line/col mapping)
```

Each arrow means "reads from." When `SourceFile.text` changes, Salsa invalidates everything downstream of it -- but only for that specific file. Other files' queries are unaffected.

## Per-File Input Mutation

`SourceFile` is a Salsa input, so its fields can be mutated:

```rust
update_file_text(&mut db, file, new_text);
```

This calls `file.set_text(&db).to(new_text)` under the hood. Salsa bumps the revision for that input and lazily re-evaluates any downstream queries the next time they are demanded.

Key properties:

- **Per-file granularity**: changing file A does not invalidate file B's queries, even if both were parsed in the same session.
- **Lazy re-evaluation**: queries only re-execute when their result is demanded. Changing text does not eagerly reparse.
- **Memoization**: if a query's inputs have not changed since the last execution, Salsa returns the cached result without re-executing.

## Testing Cache Behavior: EventDb

Salsa provides a `salsa_event` hook that fires on notable events like `WillExecute` (a query is about to run its function body). We use this to build an `EventDb` -- a test-only database that logs events so tests can assert on cache hit/miss behavior.

The `EventDb` pattern:

1. Create an `EventDb` (logs `WillExecute` events to a shared `Vec<String>`)
2. Prime the cache by querying both files
3. Clear the log (`take_log()`)
4. Mutate one file's text
5. Re-query -- check that only the mutated file's queries re-execute

This pattern lives in `lyra-db`'s `#[cfg(test)]` module because it needs access to the concrete Salsa database type.

### Verified invariants

- **Per-file isolation**: editing file A and querying file B produces zero `WillExecute` events for B's queries.
- **No-change caching**: querying a file whose text has not changed since the last query produces zero `WillExecute` events.
- **Invalidation propagation**: editing a file and re-querying it produces `WillExecute` events for the full query chain (lex, preprocess, parse).
- **Line index invalidation**: changing text that adds/removes newlines produces a different `LineIndex` on the next query.
