# Working Method

How features enter Lyra Next, from LRM text to merged code.

## A. Core Principle: Query-Driven Platform

Lyra is a semantic platform, not a compiler. Every capability is a **query**:

- Inputs are stable IDs (`FileId`, `AstId`, `SymbolId`).
- Outputs are typed results plus `Vec<Diagnostic>`.
- Public API exposes IDs and queries. Never internal pointer graphs, never mutable state.

The semantic core depends on the typed AST layer, not on CST internals. CST is
syntax-only; semantic queries never inspect green/red tree nodes directly.

Salsa manages caching and invalidation. All mutable state lives inside Salsa's
database. Outside Salsa, data structures are frozen after construction.

## B. Three Backlogs

All work falls into one of three backlogs.

### Infra Backlog

Foundational plumbing that features build on:

- Tokens and `SyntaxKind` enum
- CST (rowan green tree) and parser
- Typed AST wrappers and `AstId`
- Source model (`FileId`, spans, line index)
- Diagnostics infrastructure
- Salsa wiring in `lyra-db`
- Test harness and snapshot framework

### Semantic Capability Backlog

Reusable semantic building blocks:

- Scope graph and name resolution
- Symbol tables and symbol IDs
- Type skeleton and `type_of` queries
- Dependency/import edges
- Constant evaluation
- Port and interface resolution

### LRM Feature Backlog

Specific SystemVerilog language rules from the IEEE 1800 LRM:

- Module declarations and instantiation
- Always blocks and procedural statements
- Net and variable declarations
- Package imports and exports
- Generate constructs
- And so on, chapter by chapter

### Gating Rule

**An LRM feature may only be implemented on top of existing semantic
capabilities.** If a feature requires a capability that does not exist yet,
add that capability first as its own backlog item. Never bolt semantic
infrastructure into a feature PR.

## C. Vertical Slice

The unit of progress is a **vertical slice**: a minimal end-to-end chain
through the stack.

A slice must include all of:

1. **CST nodes** -- parser produces the relevant syntax.
2. **Typed AST** -- wrappers expose the nodes with stable `AstId`.
3. **Semantic query** -- at least one query that processes the AST and returns a typed result.
4. **Diagnostics** -- the query emits diagnostics with correct spans for error cases.
5. **Tests** -- snapshot tests for CST/AST, unit tests for the query, integration tests for end-to-end behavior.

Slices must be small. A slice that touches more than two crates or takes more
than a day is too big -- split it.

Incremental behavior must be respected: the query must work correctly after
Salsa invalidates its inputs (e.g., a file edit). At least one test should
verify cache-hit or re-evaluation behavior.

## D. PR Rules

Every PR must satisfy these hard constraints:

1. **Query or refactor.** The PR introduces or modifies at least one query,
   OR it explicitly states in the description that it is a pure refactor with
   no behavioral change.

2. **Tests with observable output.** The PR adds or updates tests. Tests must
   produce observable results: diagnostics, query return values, or debug
   dumps. "It compiles" is not a test.

3. **Stable IDs preserved.** No changes that break ID stability
   (`SyntaxKind` ordering, `AstId` derivation, `SymbolId` allocation).
   Append-only for enums.

4. **Immutability outside Salsa.** No hidden mutable caches, no `RefCell`
   tricks, no thread-local state. Salsa owns all caching.

5. **Layering respected.** Syntax layer (lexer, parser, CST, AST) never
   imports from semantic. Semantic never imports from `lyra-db`. Tools are
   clients of `lyra-db`, not peers of semantic.

6. **Policy clean.** `cargo fmt`, `cargo clippy`, `cargo test`, and both
   policy checkers pass.

## E. How to Use the LRM

We have the full IEEE 1800-2023 text. This is the repeatable process for
turning LRM sections into implemented features.

### Step 1: Decompose into Feature Units

Read an LRM chapter or section and extract **Feature Units** (FUs). Each FU
is a self-contained piece of language behavior defined by:

| Field | Description |
|-------|-------------|
| **LRM reference** | Chapter/section number(s) |
| **Syntax surface** | New tokens or grammar productions, if any |
| **Semantic rules** | What the language requires (scoping, typing, elaboration) |
| **Diagnostics** | Errors and warnings the implementation must produce |
| **Required capabilities** | Which semantic capabilities this FU depends on |

Keep FUs small. "Module declarations" is too big. "Module header with port
list" is about right.

### Step 2: Map to Capabilities

For each FU, check its required capabilities against the semantic capability
backlog:

- If all capabilities exist: the FU is **ready** -- implement it.
- If capabilities are missing: create capability items first. The FU is
  **blocked** until those land.

This enforces the gating rule mechanically.

### Step 3: Write Acceptance Tests from LRM

Before implementation, write tests derived from:

- LRM examples (the spec includes many small code snippets).
- Minimal synthetic SystemVerilog covering the rule.
- At least one **success case** (valid code, no diagnostics).
- At least one **error case** (invalid code, expected diagnostic).
- At least one **incremental edit** test where applicable (edit the source,
  verify the query updates correctly).

Tests are written first or concurrently with implementation, never after.

### Step 4: Implement

Follow this pattern for each FU:

1. **AST nodes.** Add minimal `SyntaxKind` variants and typed AST wrappers.
   Append only; never reorder.
2. **Parser.** Extend the parser to produce the new CST nodes.
3. **Semantic query.** Implement the query with stable ID inputs and typed
   outputs. Emit diagnostics with correct spans.
4. **Debug dump** (optional). Add a dump query that renders the semantic
   result as text, useful for snapshot tests and debugging.
5. **Tests.** Run the acceptance tests. Add snapshot tests for CST structure.

## F. Done Criteria

A Feature Unit is done when:

- All acceptance tests pass.
- Queries return correct results for valid and invalid inputs.
- Diagnostics have correct severity, span, and message.
- At least one incremental edit test passes (Salsa re-evaluates correctly).
- No cross-layer leaks: no semantic logic in the parser, no CST-specific
  hacks in semantic queries.
- All policy checks pass (`fmt`, `clippy`, `test`, `check_errors`,
  `check_ascii`).
