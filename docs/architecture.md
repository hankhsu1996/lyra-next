# Architecture

This document defines the architectural contract of Lyra Next. It names the core identity types, query stages, and layering rules that preserve incrementality, determinism, and correctness. Names are canonical -- if a name appears here, it can be grepped in the source and the dependency direction verified.

For Salsa query mechanics and caching behavior, see `docs/incremental.md`. For the type model, see `docs/design/type-representation.md`.

## Goals

- Complete semantic understanding of SystemVerilog projects targeting IEEE 1800-2023.
- Incremental recomputation after edits with predictable, file-local cost.
- Deterministic results: same inputs produce identical IDs, diagnostics, and types.
- A stable semantic API usable by simulators, synthesizers, and language servers.

## Non-goals

- Code generation or simulation execution.
- Full synthesis pipeline.
- Editor UI beyond the semantic API and diagnostics.

## Crate layout

Each crate has a single responsibility. Dependencies flow strictly downward.

| Crate             | Purpose                                                                                            |
| ----------------- | -------------------------------------------------------------------------------------------------- |
| `lyra-arena`      | Bump allocation wrapper around bumpalo. No internal deps.                                          |
| `lyra-source`     | `FileId`, `Span`, `TextSize`, `TextRange` -- source location primitives.                           |
| `lyra-diag`       | `Diagnostic` as plain data: severity, span, message. No side channels.                             |
| `lyra-lexer`      | Tokenizer producing all tokens including trivia. Defines the stable `SyntaxKind` enum.             |
| `lyra-preprocess` | SystemVerilog preprocessor (`` `define``, `` `ifdef``, `` `include``).                             |
| `lyra-parser`     | Recursive-descent parser producing a lossless rowan green tree.                                    |
| `lyra-ast`        | Typed AST wrappers over the CST. `AstId` and `AstIdMap` for stable node identity.                  |
| `lyra-semantic`   | Symbol tables, scope trees, type representation, name resolution algorithms. Pure -- no Salsa.     |
| `lyra-db`         | Salsa incremental database. The only crate that uses `#[salsa::*]`. Wires all layers into queries. |
| `lyra-tests`      | Snapshot test harness with `TestWorkspace` for multi-file scenarios.                               |
| `lyra-cli`        | Command-line driver for `dump-tree` and dev diagnostics.                                           |

## Crate dependency graph

```
lyra-arena        (bumpalo only, no internal deps)
lyra-source       (text-size only)
lyra-diag         -> lyra-source
lyra-lexer        -> lyra-source
lyra-preprocess   -> lyra-source, lyra-lexer
lyra-parser       -> lyra-source, lyra-lexer, lyra-diag, rowan
lyra-ast          -> lyra-source, lyra-lexer, lyra-parser, rowan
lyra-semantic     -> lyra-source, lyra-ast, lyra-diag, lyra-arena
lyra-db           -> salsa + all above
lyra-tests        -> lyra-db + pipeline crates
lyra-cli          -> lyra-db + pipeline crates
```

No circular dependencies. `lyra-semantic` contains algorithms and data structures but never touches Salsa. `lyra-db` is the sole integration point.

## Identity types

Every semantic object is identified by a small, copyable, comparable ID. Semantic data stores IDs, not references. This is the foundation of the "no borrowed semantics across query boundaries" rule.

### Source identity

| Type              | Crate         | Definition                                                                  |
| ----------------- | ------------- | --------------------------------------------------------------------------- |
| `FileId`          | `lyra-source` | Opaque `u32` identifying a source file.                                     |
| `SourceFile`      | `lyra-db`     | Salsa input. Carries `file_id: FileId`, `text: String`, `include_map`.      |
| `CompilationUnit` | `lyra-db`     | Salsa input. Carries `files: Vec<SourceFile>`. Unit of cross-file analysis. |

### AST identity

| Type          | Crate      | Definition                                                     |
| ------------- | ---------- | -------------------------------------------------------------- |
| `AstId<N>`    | `lyra-ast` | Typed stable node identity: `FileId` + `(SyntaxKind, offset)`. |
| `ErasedAstId` | `lyra-ast` | Type-erased form. Used as map keys across query boundaries.    |
| `AstIdMap`    | `lyra-ast` | Per-file map from CST nodes to `AstId`. Built once per parse.  |

### Semantic identity

| Type             | Crate           | Definition                                                               |
| ---------------- | --------------- | ------------------------------------------------------------------------ |
| `SymbolId`       | `lyra-semantic` | File-local symbol index (`u32`).                                         |
| `ScopeId`        | `lyra-semantic` | File-local scope index (`u32`).                                          |
| `GlobalDefId`    | `lyra-semantic` | Cross-file definition identity. Wraps `ErasedAstId` (inherits `FileId`). |
| `GlobalSymbolId` | `lyra-semantic` | Cross-file symbol identity: `FileId` + `SymbolId`.                       |
| `EnumId`         | `lyra-semantic` | Enum definition identity: `file`, `owner` scope, `ordinal`.              |
| `RecordId`       | `lyra-semantic` | Struct/union definition identity: `file`, `owner` scope, `ordinal`.      |

Invariant: IDs survive incremental re-analysis for unchanged regions. `ErasedAstId` encodes `(kind, offset)`, so edits only shift IDs for nodes after the edit point. Downstream queries like `NameGraph` strip positional data, producing equal results on whitespace-only edits. Salsa backdates equal results, skipping re-execution of resolution and typing.

## Query pipeline

Queries form an acyclic dependency graph. Each stage is a pure function of its inputs. `lyra-db` wires them into Salsa for memoization and incremental invalidation.

### Per-file pipeline

```
SourceFile.text
  |
  +-- lex_file              -> tokens
  |     |
  |     +-- preprocess_file -> expanded token stream
  |           |
  |           +-- parse_file        -> Parse (lossless CST)
  |                 |
  |                 +-- ast_id_map        -> AstIdMap
  |                 |
  |                 +-- def_index_file    -> DefIndex
  |                       |
  |                       +-- name_graph_file  -> NameGraph
```

`parse_file(db, SourceFile) -> Parse` builds the rowan green tree. The CST is lossless -- round-tripping to text reproduces the original source exactly.

`def_index_file(db, SourceFile) -> DefIndex` extracts declarations, scopes, symbols, imports, and exports from the parse tree. This is the per-file semantic skeleton.

`name_graph_file(db, SourceFile) -> NameGraph` extracts offset-independent name-use data from `DefIndex`. On whitespace-only edits, `NameGraph` produces an equal result, so Salsa backdates it and skips re-running resolution downstream.

### Cross-file aggregation

```
def_index_file(file1) + def_index_file(file2) + ...
  |
  +-- global_def_index(unit)      -> GlobalDefIndex
  |
  +-- package_scope_index(unit)   -> PackageScopeIndex
  |
  +-- compilation_unit_env(unit)  -> CompilationUnitEnv
```

`global_def_index(db, CompilationUnit) -> GlobalDefIndex` aggregates module, interface, program, primitive, config, and package names with their `GlobalDefId` from all files.

`package_scope_index(db, CompilationUnit) -> PackageScopeIndex` extracts per-package exported symbols split by namespace (value and type). Resolves `export` declarations against dependency packages.

These are the only cross-file summary structures. All downstream queries read summaries, not raw per-file data from other files.

### Resolution

```
name_graph_file + global_def_index + package_scope_index
  |
  +-- resolve_core_file(file, unit)       -> CoreResolveOutput
  +-- resolve_index_file(file, unit)      -> ResolveIndex
```

`resolve_core_file(db, SourceFile, CompilationUnit) -> CoreResolveOutput` resolves names using scope-chain lookup, explicit imports, wildcard imports, and implicit imports.

`resolve_index_file(db, SourceFile, CompilationUnit) -> ResolveIndex` maps each use-site `ErasedAstId` to a `Resolution` containing a `ResolvedTarget` (either `GlobalSymbolId` or `EnumVariantTarget`) and a `Namespace`. This is the primary resolution query for all consumers.

Resolution has a parallel base/enriched split to avoid cycles between const-eval and enum expansion. See `docs/design/resolution-cycle.md` for details.

### Typing and evaluation

```
resolve_index_file + def_index_file
  |
  +-- type_of_symbol(sym_ref)   -> SymbolType
  +-- type_of_expr(expr_ref)    -> ExprType
  +-- eval_const_int(file, unit, ast_id) -> ConstValue
  +-- enum_sem(enum_ref)        -> EnumSem
  +-- record_sem(record_ref)    -> RecordSem
  +-- callable_signature(ref)   -> CallableSig
```

`eval_const_int` uses `base_resolve_index` to avoid cycles with enum expansion. It folds parameter references, literals, and arithmetic into `ConstValue`.

### Diagnostics

```
file_diagnostics(file, unit) -> Vec<Diagnostic>
```

`file_diagnostics` collects errors from all stages: parse errors, resolution errors, type errors, import conflicts, and enum expansion diagnostics. Diagnostics are plain data (`lyra-diag::Diagnostic`), not side-channel output.

## Layering contract

Layers form a strict DAG. No layer calls into a later layer.

1. **Syntax** -- tokenization, preprocessing, CST construction.
2. **AST** -- typed wrappers, stable node identity via `AstIdMap`.
3. **Index** -- per-file `DefIndex` (symbols, scopes, imports, exports). Per-unit `GlobalDefIndex`, `PackageScopeIndex`.
4. **Resolve** -- name lookup, binding, visibility, import ordering rules. Produces `ResolveIndex`.
5. **Type and eval** -- types, widths, parameter values, constant folding.

Diagnostics are produced at every layer and collected by `file_diagnostics`. They are not a separate layer in the dependency graph.

Cross-file relationships are expressed exclusively via IDs stored in summary structures (`GlobalDefIndex`, `PackageScopeIndex`). No query reads raw `DefIndex` from another file except through these summaries.

## Determinism

- Iteration over symbols and scopes uses stable ordering (insertion order or sorted).
- Diagnostics are deterministic within a file.
- `HashMap` is used for point lookups (e.g., `ResolveIndex.resolutions`). When ordering matters, results are sorted before output.
- Same inputs produce identical IDs, types, and diagnostics across runs.

## Data ownership

- Returned semantic objects are owned and immutable. No borrowed data across query boundaries.
- Shared data uses `Arc` where needed (e.g., `CallableSig`).
- Interned strings use `SmolStr` for cheap cloning and comparison.
- No ambient mutable global state outside Salsa's tracked inputs.

## Error tolerance

The engine handles partial and erroneous programs at every stage.

- The parser produces a CST even with syntax errors, wrapping unrecognized tokens in `ErrorNode`.
- `DefIndex` records what it can extract; missing nodes produce gaps, not panics.
- Resolution returns `Unresolved` with a structured reason, not a crash.
- Type inference produces `Ty::Error` for unresolvable types and continues.
- Diagnostics remain actionable even when downstream facts are incomplete.
- Library code never panics at runtime. No `.unwrap()` or `.expect("msg")` outside tests.

## Adding a language feature

Checklist for extending the engine with a new SystemVerilog construct:

1. **Parser** -- add CST nodes and `SyntaxKind` variants (append-only).
2. **AST** -- add typed wrappers with accessors in `lyra-ast`.
3. **Index** -- update `DefIndex` builder to extract new declarations and scopes.
4. **Resolve** -- update resolution rules if the feature introduces new visibility or import semantics.
5. **Type and eval** -- add typing rules, const-eval support, or aggregate handling.
6. **Diagnostics** -- add error codes and messages. Collect in `file_diagnostics`.
7. **Tests** -- add LRM corpus tests. Update `docs/lrm/gaps.md` if deferring.
