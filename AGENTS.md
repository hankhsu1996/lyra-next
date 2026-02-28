# Agents Guide

This file provides instructions for AI coding agents working on this codebase.

If `AGENTS.local.md` exists, read it for additional local/personal instructions that override or extend this file.

## Overview

Lyra Next is a Rust-based SystemVerilog **semantic platform** --a single incremental semantic model that all tools (LSP, linter, formatter, compiler, simulator) query as clients.

### North Star

- **One canonical incremental semantic database.** Salsa-based, ID-keyed, no borrowed semantic objects across boundaries. This is the shared core for sim, synth, lint, and LSP.
- **Deterministic and reproducible.** Same input produces same IDs, outputs, and diagnostics. Stable naming, stable ordering, no hidden global state.
- **Strict layering.** Parse -> def/index -> name resolution -> typing/elab. Each layer is pure and query-driven. No back-edges that create cycles.
- **High performance and parallel friendly.** Shardable queries, cheap recompute, minimal cloning, owned/Arc data, predictable memory.
- **LRM-accurate by test signoff.** A feature is only "done" when lrm-add tests exist and pass; otherwise it stays in gaps.

When planning, protect invariants in this order:

1. **Protect the DAG** -- if a step risks making lower layers depend on higher ones, redesign.
2. **Protect the ID model** -- if a feature can't be represented with stable IDs and owned data, it's not done.
3. **Protect determinism and diagnostics** -- if order or duplicates are ambiguous, define it precisely and test it.
4. **Then implement mechanics** -- parser/AST/builder changes are easy; getting the query boundaries correct is the real work.

### Crate Map

| Crate | Purpose |
|-------|---------|
| `lyra-arena` | Thin bump-allocation wrapper around bumpalo |
| `lyra-source` | `FileId`, `Span`, `TextSize`/`TextRange` --source location primitives |
| `lyra-diag` | `Diagnostic` as plain data with severity + span + message |
| `lyra-lexer` | Tokenizer producing all tokens including trivia; defines stable `SyntaxKind` enum |
| `lyra-preprocess` | SystemVerilog preprocessor (`` `define ``, `` `ifdef ``, etc.) --placeholder |
| `lyra-parser` | Builds lossless rowan green tree from tokens; roundtrip-faithful |
| `lyra-ast` | Typed AST wrappers, `AstId` (stable node identity), `AstIdMap` |
| `lyra-semantic` | Symbol tables, scope trees, type representation, const evaluation |
| `lyra-db` | Salsa incremental database --the only crate that touches salsa |
| `lyra-tests` | Snapshot test harness: `TestWorkspace` builder and tree/diagnostic dump utilities |
| `lyra-cli` | Command-line driver: `dump-tree`, dev diagnostics (`cargo run -p lyra-cli -- <cmd>`) |

### Data Flow

source text -> lyra-lexer -> lyra-preprocess -> lyra-parser -> lyra-ast -> lyra-semantic -> lyra-db -> tools

### Documentation

Design docs live in `docs/`. Read these before making architectural changes:

- `docs/architecture.md` -- crate layering, data flow, design principles
- `docs/id-model.md` -- FileId, AstId, SymbolId, ScopeId stable identity strategy
- `docs/incremental.md` -- Salsa integration, query design, caching
- `docs/cst-ast.md` -- rowan green/red tree, trivia handling, typed AST wrappers
- `docs/parser-design.md` -- parser invariants, decisions, known improvements
- `docs/roadmap.md` -- milestone-based capability plan and exit demos
- `docs/progress.md` -- milestone completion checklist

## Architecture Rules

- **Salsa lives only in `lyra-db`** --no `#[salsa::*]` annotations in any other crate. Other crates define types and algorithms; `lyra-db` wires them into the incremental framework.
- **Diagnostics are plain data** --`Vec<Diagnostic>`, no mutable global sink, no side-channel error reporting.
- **SyntaxKind is append-only** --never reorder or remove existing variants.
- **Lexer produces trivia** --whitespace and comments are tokens. The parser consumes them into the green tree. Roundtrip fidelity is mandatory.
- **AstId = FileId + (kind, offset)** --stable identity for AST nodes across incremental runs.
- **Frozen stores in semantic** --symbol tables, scope trees are built once per analysis pass, keyed by stable IDs, no direct references.
- **`lyra-source` has no arena dependency** --spans and text primitives are independent of allocation strategy.
- **Design docs use semantic terms, not tool terms** --the core provides queries (`resolve`, `type_of`); tools (LSP, linter) consume them. Say "resolve a name to its declaration," not "go-to-definition."
- **Lowering depends only on anchor data, not producer-layer services** --semantic anchors are self-describing; lowering extracts presentation data (ranges, spans) from the anchor's own stored fields. Injecting producer-layer services (AST maps, syntax trees) into lowering breaks layering and hides producer bugs.
- **Producers must not silently drop findings** --when a producer encounters an unexpected state (failed lookup, missing data), it must emit an internal diagnostic with a deterministic fallback and continue. Never `return`/`continue` to skip producing a finding. Thread required (non-optional) fallbacks through the call stack so no code path can silently bail.

## Typed AST Boundary

`lyra-ast` is the only crate that walks CST structure (rowan children, token kinds). `lyra-db` and `lyra-semantic` consume typed AST accessors exclusively. Classification is typed (enums, accessors), not `SyntaxKind` matching.

- **No dual-meaning wrappers.** If an AST node appears in multiple grammar contexts, expose grammar-specific iterators or a typed sum, not two same-shaped accessors with different names.
- **No raw `SyntaxNode` in db/semantic helpers.** Functions accept typed wrappers (`&BinExpr`, `&TypeSpec`), not `&SyntaxNode`.
- **No `.syntax()` in `lyra-semantic` (non-builder).** If a semantic change needs `.syntax()`, add or extend a typed accessor in `lyra-ast` or move the conversion glue into `lyra-db`. `.syntax()` is allowed in `lyra-db` (orchestration) and `lyra-ast` (typed accessor layer).
- **Structure assumptions live in lyra-ast.** If a consumer assumes a node has a specific child structure, the accessor returns the typed result directly (e.g. `Option<BinExpr>`). Consumer layers never cast typed results into narrower types.

Enforced by `tools/policy/check_cst_layering.py` (rules C001-C005).

## Semantic API Discipline

- **Contract-preserving changes.** When replacing a classifier or guard, preserve the accepted/rejected set exactly. Any intentional behavior change requires an explicit note and a test.
- **No boolean-meaning public APIs.** Do not encode meaning in `bool` or `Option<bool>` across crate boundaries. Use small enums (`Signing`, `Direction`, `Polarity`).
- **No silent fallback to `Error`.** Do not use `unwrap_or(Ty::Error)` as control flow. Return `Option`/`Result` and force callers to decide deliberately.
- **Single choke point for shared classification.** If two call sites need the same "what is this?" logic, it becomes one typed accessor or classifier, usually in `lyra-ast`.
- **Performance boundary.** No per-call expensive infrastructure (maps, whole-tree scans, allocations) in hot paths. Build once per query/file and pass in (`&AstIdMap`, `&ResolveCtx`).
- **Behavior locks.** Any refactor that changes classification paths must include a small test that would fail if the accept/reject contract regresses.
- **No special-case escape hatches.** Do not introduce local helpers (e.g. `expr_site_of`) because a wrapper "doesn't implement X". Fix the infrastructure so the common path works (e.g. add a trait or method in `lyra-ast`).
- **No silent downgrade.** Do not return `Unsupported`/`Ty::Error` to satisfy a type mismatch (e.g. `TfArg::Type` mapped to `UnsupportedExprKind`). Use `Option`, split the helper by kind, or handle the kind correctly.
- **No global scans for internal invariants.** Surface invariant failures at creation with a fallback anchor already in hand. Do not add later-stage sweeps over all symbols/defs/types to rediscover errors; prefer structured errors lowered once.

## Crate Dependency Graph

```
lyra-arena       bumpalo only, no internal deps
lyra-source      text-size only
lyra-diag        -> lyra-source
lyra-lexer       -> lyra-source
lyra-preprocess  -> lyra-source, lyra-lexer
lyra-parser      -> lyra-source, lyra-lexer, lyra-diag, rowan
lyra-ast         -> lyra-source, lyra-lexer, lyra-parser, rowan
lyra-semantic    -> lyra-source, lyra-ast, lyra-diag, lyra-arena
lyra-db          -> salsa + all above
lyra-tests       -> lyra-source, lyra-lexer, lyra-preprocess, lyra-parser, lyra-diag, lyra-db
lyra-cli         -> lyra-source, lyra-lexer, lyra-preprocess, lyra-parser, lyra-ast, lyra-semantic, lyra-db
```

Do not introduce circular dependencies. Do not add dependencies that violate this graph without discussion.

## Coding Conventions

- **Edition**: Rust 2024, resolver 3
- **Formatting**: `cargo fmt` -- no exceptions
- **Linting**: `cargo clippy` must pass clean
- **Naming**: follow standard Rust conventions (`rustfmt` and `clippy` enforce most of it)
  - `snake_case` for functions, methods, variables, modules
  - `CamelCase` for types, traits, enum variants
  - `SCREAMING_SNAKE_CASE` for constants and statics
  - Prefix unused variables with `_`
- **Terminology**: use IEEE 1800 LRM terminology for SystemVerilog concepts
- **Prefer idiomatic Rust**:
  - `&str` / `&[T]` for non-owning references, not owned types in function parameters
  - `Option` and `Result` for fallibility, not sentinel values
  - Iterators and combinators over manual loops where clearer
  - `enum` over boolean flags or stringly-typed values
  - Derive standard traits (`Debug`, `Clone`, `PartialEq`, etc.) where appropriate
- **File size**: keep files under 800 lines. 1200 is the hard limit. When a
  file grows past 800 lines, split it into focused modules. Plan for file
  organization up front -- do not let files bloat and refactor later.
- **Tests**: every new behavior needs a test. Unit tests in `#[cfg(test)]`
  only for testing private helpers. Integration tests go in `tests/` as a
  single binary with domain submodules (one top-level `.rs` entry point,
  submodules in a matching directory). Do not use separate top-level test
  files with a shared `common/mod.rs` -- each top-level file compiles as
  its own binary, causing dead_code warnings on shared helpers.
  - **Per-crate tests**: each crate owns integration tests for its public API
    in its own `tests/` directory. Organize submodules by domain (e.g.
    `identity`, `operators`) not by type/struct.
  - **Cross-crate tests**: `lyra-tests` holds end-to-end snapshot tests that
    exercise the full pipeline (lex -> preprocess -> parse -> DB). Use
    `TestWorkspace` for multi-file scenarios.
  - **DB tests**: `lyra-db` inline `#[cfg(test)]` tests are the exception --
    they test Salsa query wiring which requires access to `LyraDatabase`.
- **Lint suppression**: do not use `#[allow(...)]` to silence warnings.
  Fix the root cause or restructure to eliminate the warning.
- **Visibility**: prefer `pub(crate)` over `pub` unless the item is part of the crate's public API.

## Philosophy

### Adding Features

Before implementing a new feature directly:

1. **Explore existing structure** -- understand how similar things work
2. **Look for generalization** -- can an existing abstraction be extended?
3. **Find the right level** -- the best change is often minimal when placed correctly
4. **Prefer extending over adding** -- modify existing infrastructure rather than creating parallel structures

The goal: make the new requirement feel like a natural extension, not a bolt-on.

### Fixing Bugs

After debugging and finding the immediate cause:

1. **Step back** -- why does this bug exist? What allowed it?
2. **Look for design issues** -- bugs often indicate deeper problems
3. **Fix the root cause** -- not just the symptom. Change the API, don't work around it at the call site. If a function should accept `&str` but you have `String`, change the function signature -- don't sprinkle `.as_str()` conversions everywhere.
4. **Avoid band-aids** -- don't just add a control branch; address the fundamental issue

The goal: leave the codebase stronger, not just patched.

### Design Principles

- **No hacks, no short-term solutions.** The codebase is meant to be clean long-term. Never introduce workarounds. If existing code has a bad pattern, flag it and fix it properly rather than copying it. Prefer the right fix even if it touches more files. Do not minimize change count -- make the right change.
- **Abstraction level matching.** Do not put implementation-specific helpers on high-level interfaces. Keep helpers file-local in the implementation that uses them. When adding a function, ask: "is this the right level?"
- **Co-locate runtime validations.** Runtime invariant checks should be co-located in a single place so they can be enabled/disabled together. Do not scatter validation helpers across unrelated files.
- **Flag existing bad patterns.** When you encounter a pre-existing bad pattern (e.g., local `#[allow(...)]` instead of a workspace-level lint config), fix it as part of the current change. Do not silently copy bad patterns.
- **No code history in comments.** Comments describe what the code IS, not what it used to be or what changed. Avoid phrases like "not stored here", "moved from X", "previously Y". Describe the current design directly.
- **Roadmap/progress: capabilities, not implementation.** Roadmap and progress docs describe what the system can do, not how it's built. No line items for internal quality fixes, refactors, or implementation details. No code-level names (types, functions, modules) in these docs.

### Capturing Learnings

When work reveals a reusable principle or a recurring mistake, update this file.
Design docs capture what to build; this file captures how to work.

Keep entries general -- this file guides all future work, not just the feature
that taught the lesson. Write principles and patterns, not feature-specific
examples. If an entry only makes sense in the context of one crate or feature,
it probably belongs in that crate's docs instead.

## Planning and Execution

- Keep planning lightweight with two docs only: `docs/roadmap.md` and
  `docs/progress.md`.
- `docs/roadmap.md` is future-facing milestone planning.
- `docs/progress.md` tracks where we are against the roadmap. Check off items
  when they ship. No dates or logs -- git history has that.
- Update roadmap only when priorities change.

## Before Submitting Changes

```bash
cargo check
cargo test
cargo fmt --check
cargo clippy
```

All four must pass.

## Error Handling Policy

Library code (everything under `crates/`) must not panic at runtime:

- No `.unwrap()` or `.expect("msg")` -- use `Result`/`Option` propagation.
  Tests are exempt. The checker flags `.expect("` (string arg) but not
  `.expect(SyntaxKind::...)` or similar non-panic method calls.
- No `panic!()` in library code. Tests are exempt.
- Utility crates under `crates/` are still library code and must not use
  panic-based error handling.
- Every `unsafe` block must have a `// SAFETY:` comment on the preceding line.
- No `dbg!()` or `println!()` -- use structured diagnostics.
- No section-header comments (e.g. `// --- Foo ---` or `// ### Bar ###`). Use plain `// Foo` instead.

Run the checker:

```bash
python3 tools/policy/check_errors.py
python3 tools/policy/check_errors.py --staged    # staged files only
```

## ASCII-Only Policy

All text files (`.rs`, `.toml`, `.md`, `.yaml`, `.json`, `.txt`, `.sh`, `.py`) must contain only ASCII characters (0x00-0x7F). No Unicode arrows, em-dashes, curly quotes, or emojis.

Use `--` instead of em-dash, `->` instead of arrow, `"` instead of curly quotes.

Run the checker:

```bash
python3 tools/policy/check_ascii.py
python3 tools/policy/check_ascii.py --staged    # staged files only
```

## CST Layering Policy

`lyra-semantic` and `lyra-db` must not perform raw CST traversal (rowan `.children()`, `.descendants()`, etc.) except in allowlisted modules. Consumer layers use typed AST accessors from `lyra-ast`. CST walking belongs in: (1) accessor implementations inside `lyra-ast`, (2) builder-phase modules in `lyra-semantic`, or (3) explicitly allowlisted transitional modules (marked with TODO tags for cleanup).

Allowlisted modules are listed in the script per crate. When adding CST access to a new module, add it to the allowlist with a comment explaining why. Prefer adding a typed AST accessor in `lyra-ast` instead.

Run the checker:

```bash
python3 tools/policy/check_cst_layering.py
python3 tools/policy/check_cst_layering.py --staged    # staged files only
```

## File Size Policy

Rust source files under `crates/` must stay under 800 lines (soft limit). 1200 lines is the hard limit. When a file grows past 800, split it into focused modules.

Run the checker:

```bash
python3 tools/policy/check_lines.py
python3 tools/policy/check_lines.py --staged    # staged files only
```

## LRM Signoff Workflow

Full methodology is in `docs/lrm-signoff.md`. This section covers operational instructions.

### Section-Keyed Test Corpus

The test corpus is keyed by LRM section IDs. `docs/lrm/sections.json` is the canonical index of all sections. Every ownable (leaf) section can have exactly one owner directory.

**Directory naming:** `lrm/chXX/{section}_{snake_case_label}/` (e.g., `lrm/ch05/5.3_white_space/`, `lrm/ch05/5.7.1_integer_literals/`). The section prefix must match the section in `test.yaml`. Regression tests go in `regression/short_name/`.

**Ownable = leaf only.** A section is ownable iff it has no children in sections.json. Parent sections are never ownable. When a parent has content beyond its children, a synthetic `.0` child captures it (e.g., `5.6.0` for simple identifier rules under `5.6`).

**cases/ nesting.** When a leaf section has multiple independent tests needing separate compilation units, use `cases/`:

```
ch26/26.3_referencing_data/
  cases/
    explicit_import/
      test.yaml    # section: "26.3"
      main.sv
    wildcard_import/
      test.yaml    # section: "26.3"
      main.sv
```

No `test.yaml` directly under the owner dir when `cases/` exists (no mixed mode).

### Creating an LRM Test Case

**Each test directory contains:**
- A `test.yaml` marker file with metadata (required for discovery).
- One or more `.sv` files with inline annotations encoding expected diagnostics.
- A passing test has no annotations (runner asserts zero diagnostics).
- A failing test has `// ^ error[code]: message` annotations pinning each expected diagnostic.
- Use `// ALLOW-EXTRA-DIAGS` only for in-progress tests where not all diagnostics are pinned yet. Remove it before signing off.

**test.yaml for LRM tests:**
```yaml
kind: lrm
lrm:
  section: "5.7.1"
```

Only `section` is required. No `chapter` or `title` fields -- those come from sections.json.

**test.yaml for regression tests:**
```yaml
kind: regression
```

**Do not create:** sidecar `.diag` files, `pass/`/`fail/` subdirectories, README files per test case, or per-chapter `.md` files unless recording an LRM ambiguity decision.

### Review Snapshots After Accepting

After running `cargo insta accept`, always read the snapshot file and review its contents. Check that every diagnostic makes sense for what the test is testing. Don't let unrelated diagnostics (e.g., type warnings in a lexer test) leak in -- either fix the test inputs to avoid them or pin them with annotations. Blindly accepting snapshots hides problems.

### Recording Gaps

When `/lrm-add` discovers a feature the engine cannot handle yet, do NOT commit a failing test. Instead:

1. Only commit tests that pass with current capabilities.
2. Add an entry to `docs/lrm/gaps.md` describing what is missing, what blocks it, and what test names will be added once the fix lands.

This is mandatory -- every deferred test must have a gaps.md entry. The gap ledger is the work queue.

When fixing a gap: add the now-passing test to the corpus AND remove the entry from gaps.md in the same PR.

### Signing Off a Chapter

A chapter is signed off when all ownable (leaf) sections in sections.json have owner directories, all tests pass, and no entries remain in `docs/lrm/gaps.md` for the chapter. Use `/lrm-signoff <chapter>` to audit.

## What Not To Do

- Don't add `salsa` annotations outside `lyra-db`
- Don't reorder `SyntaxKind` variants
- Don't break roundtrip (parse -> green tree text == original source)
- Don't introduce runtime panics in library code
- Don't add heavyweight dependencies without justification
- Don't use non-ASCII characters in any text file
