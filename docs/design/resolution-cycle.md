# Cycle avoidance between resolution and constant evaluation

Some language features generate new names from constant expressions. Expanding those names requires `eval_const_int`, which itself requires name resolution to look up parameters. If the enriched resolution depends on the expanded names, the query graph cycles.

This document defines the pattern that breaks such cycles and the contract that preserves it.

## The pattern: core and augmented resolution

Resolution is split into two paths:

- **Core** (`resolve_core_file` -> `base_resolve_index`): resolves names using scope-chain lookup and imports only. No augmentation data. This is the path `eval_const_int` depends on.
- **Augmented** (`enriched_resolve_core` -> `resolve_index_file`): takes the same inputs as core, plus precomputed augmentation data produced by feature-specific queries. All consumers except `eval_const_int` use this path.

The split guarantees that `eval_const_int` never transitively depends on a query that calls `eval_const_int`.

## Contract

### What core resolve must cover

Core resolution must resolve every name that `eval_const_int` can encounter. Today that means:

- Parameters (their init expressions are the primary const-eval input).
- Localparam and enum member references within constant expressions.
- Package-qualified names (`pkg::PARAM`).

All of these are real symbols with scope bindings, found by scope-chain lookup or import resolution. No augmentation is needed.

### What augmentation is allowed to do

An augmentation query may:

- Depend on `eval_const_int` (and therefore on `base_resolve_index`).
- Produce additional name-to-target mappings that `enriched_resolve_core` consults.
- Add new `CoreResolution` variants or extend existing resolution logic.

An augmentation query must not:

- Depend on `resolve_index_file` or `enriched_resolve_core`.
- Modify how core resolution works for names that `eval_const_int` can see.

### Adding a new augmentation

When a new feature generates names from constant expressions:

1. Create a Salsa query that expands names using `eval_const_int`.
2. Pass the expansion data into `build_resolve_core` as an additional parameter.
3. Wire it through `enriched_resolve_core`, not `resolve_core_file`.
4. Add an example section below documenting the dependency path.

## Example: enum range expansion (LRM 6.19.3)

Enum members like `name[N]` generate names (`name0`, `name1`, ...) where `N` is a constant expression evaluated by `eval_const_int`.

### Queries

```
resolve_core_file(file, unit)           -- core: no enum variant data
enriched_resolve_core(file, unit)       -- augmented: includes enum variant data

base_resolve_index(file, unit)          -- uses resolve_core_file
resolve_index_file(file, unit)          -- uses enriched_resolve_core
```

### Dependency graph

```
resolve_index_file
  -> enriched_resolve_core
    -> enum_variant_index
      -> enum_variants
        -> eval_const_int
          -> base_resolve_index
            -> resolve_core_file
              -> name_graph_file
```

`eval_const_int` uses `base_resolve_index`, which reaches `resolve_core_file` (no enum data). There is no path back to `enriched_resolve_core` or `enum_variant_index`.

### Why core resolution is sufficient for const-eval here

`eval_const_int` only needs parameter names, which are real symbols in scope bindings. Plain enum members (without range syntax) are also real symbols found by the core path. Only range-generated names (`A0`, `A1`, ...) require the augmented path, and const-eval never references those.
