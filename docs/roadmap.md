# Roadmap

Milestone-based plan for Lyra Next. Each milestone enables a set of
capabilities. Current status lives in `docs/progress.md`.

This repo is the **semantic engine**: front-end pipeline correctness,
semantic checks, diagnostics, and a batch driver. IDE surfaces (LSP,
hover, go-to-definition) are downstream consumers that live outside
this repo.

> If a feature exists only to serve an editor UI, it is out of scope.

## How Milestones Relate to Work

- Infra-heavy work lands mostly in M0--M2.
- Semantic capability work lands mostly in M3+.
- LRM features are pulled continuously once the required capability exists.
- A milestone is done when capabilities are stable and demonstrated.

## M0: Harness and Project Skeleton

**Objective:** Build and test infrastructure works end to end.

**Deliverables:**

- Workspace with all crates compiling and wired together.
- `lyra-db` assembles the Salsa database with placeholder queries.
- `lyra-diag` defines `Diagnostic` as plain data (severity, span, message).
- `lyra-source` provides `FileId`, `Span`, `TextSize`, `TextRange`.
- Snapshot test framework operational (input file -> expected output).
- CI runs fmt, clippy, test, and policy checks.

**Demo:** `cargo test` passes with at least one trivial end-to-end test
(source text in, empty diagnostics out).

## M1: Syntax Core

**Objective:** Parse SystemVerilog source into a lossless CST and typed AST.

**Deliverables:**

- `lyra-lexer` tokenizes all SV tokens including trivia (whitespace,
  comments). `SyntaxKind` enum is stable.
- `lyra-parser` builds rowan green trees. Roundtrip fidelity: green tree
  text equals original source, byte for byte.
- `lyra-ast` provides typed wrappers with `AstId` (stable node identity).
- `lyra-preprocess` defines the preprocessing stage contract: token
  stream output, source map (expanded span -> original file/range),
  and include dependency graph. Identity (passthrough) implementation
  with correct spans.
- Parse error diagnostics with correct spans.

**Demo:** Parse a multi-module SV file, dump the CST, verify roundtrip.
Parse errors shown with line/column. Preprocess pipeline shape is wired
through `lyra-db` queries.

## M2: Source Model and Incremental Story

**Objective:** Multi-file source set with incremental reparse and Salsa
caching.

**Deliverables:**

- File set management in `lyra-db`: add/remove/update files.
- Line index for efficient offset-to-line/column conversion.
- Incremental reparse: editing a file invalidates only affected queries.
- `AstIdMap` per file, stable across incremental runs.
- Preprocess stage participates in Salsa invalidation via include
  dependency graph.
- Source map model defined so macro expansion can plug in without
  changing downstream queries.
- Cache-hit tests: edit file A, verify file B's queries are not
  re-evaluated.

**Demo:** Load two files, edit one, show that only the edited file is
re-parsed. Query results for the untouched file come from cache.

## M3: Names and Scopes

**Objective:** First semantic analysis -- resolve names to declarations.

**Deliverables:**

- `SymbolId` and symbol table in `lyra-semantic`.
- Scope graph with typed scope kinds (module, block, generate, etc.)
  and parent-chain walk.
- Multi-namespace name tables (value vs type at minimum).
- Path model for hierarchical and qualified names (`pkg::sym`, `a.b.c`).
- `resolve(FileId, AstId) -> Option<SymbolId>` query, with result shape
  that carries namespace and ambiguity info.
- `symbols_in_scope(ScopeId) -> Vec<SymbolId>` query.
- Cross-file resolution: packages, imports, global symbol index.
- Unresolved-name diagnostic with span pointing to the use site.
- Duplicate-definition diagnostic.

**Demo:** Given a name use, resolve it to its declaration and return the source
location. Cross-file package imports resolve correctly. Unresolved names
produce a diagnostic.

## M4: Type Skeleton

**Objective:** Represent SV types and answer basic type queries.

**Deliverables:**

- Type representation in `lyra-semantic`: logic, reg, integer types,
  packed/unpacked dimensions, enums, structs (flat -- no parameterization
  yet).
- `type_of(SymbolId) -> Type` query.
- `type_of_expr(FileId, AstId) -> Type` query for simple expressions.
- Basic type-error diagnostics (assignment width mismatch, undeclared type).
- Constant expression evaluation for dimension bounds.

**Demo:** Query a variable or expression, get its resolved type.
Type errors shown with expected vs actual.

## M5: Batch Semantic Engine

**Objective:** Slang-style batch semantic engine. Build the instance tree
for a design and run semantic checks across it.

**Deliverables:**

- Module signature query: port names, directions, types. Parameters
  typed enough to resolve port types.
- Instantiation resolution: named and positional port connections.
- Instance tree for a chosen top module.
- Diagnostics: unknown port, missing port, port width mismatch, too many
  args, duplicate instance names.
- CLI batch driver: preprocess -> parse -> namegraph -> elaborate
  (instance tree) -> semantic checks -> emit diagnostics.
- Query-based and pure so Salsa can parallelize.
- Performance baseline: parse + full semantic analysis of a 10K-line
  file under 1 second.

**Demo:** Run `lyra-cli check top.sv` on a multi-module design. Port
connection mismatches and missing ports produce diagnostics with
file/line/column. Instance tree built correctly across file boundaries.

## M6: Elaboration Completeness

**Objective:** Deeper LRM coverage for real-world SV patterns. Still
batch-oriented -- richer semantics, richer diagnostics.

**Deliverables:**

- Context-determined typing (LRM 11.6): assignment context sizing,
  sign extension/truncation rules.
- Callable typing: function/task calls to declarations, parameter
  types and return types.
- Struct/union field access, assignment, and casting.
- Full array semantics: packed/unpacked dimensions, slicing, streaming.
- Enum rules: casting, comparisons, literal typing.
- Generate blocks and parameter elaboration.
- Interface/modport semantics.

**Demo:** Run batch checks on a real-world codebase with structs,
arrays, function calls, and generate blocks. Diagnostics cover field
access errors, dimension mismatches, and enum cast violations.

## Rules

1. Milestones are capability gates, not exhaustive feature lists.
2. LRM features flow continuously once dependent capabilities exist.
3. Earlier milestones can receive additions discovered by later work.
4. Each milestone needs a demonstrable outcome.
