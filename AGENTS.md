# Agents Guide

This file provides instructions for AI coding agents working on this codebase.

If `AGENTS.local.md` exists, read it for additional local/personal instructions that override or extend this file.

## Overview

Lyra Next is a Rust-based SystemVerilog **semantic platform** --a single incremental semantic model that all tools (LSP, linter, formatter, compiler, simulator) query as clients.

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

### Data Flow

source text -> lyra-lexer -> lyra-preprocess -> lyra-parser -> lyra-ast -> lyra-semantic -> lyra-db -> tools

### Documentation

Design docs live in `docs/`. Read these before making architectural changes:

- `docs/architecture.md` -- crate layering, data flow, design principles
- `docs/id-model.md` -- FileId, AstId, SymbolId, ScopeId stable identity strategy
- `docs/incremental.md` -- Salsa integration, query design, caching
- `docs/cst-ast.md` -- rowan green/red tree, trivia handling, typed AST wrappers
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
- **File size**: keep files under 1000 lines. 1200 is the hard limit. When a
  file grows past 1000 lines, split it into focused modules. Plan for file
  organization up front -- do not let files bloat and refactor later.
- **Tests**: every new behavior needs a test. Unit tests in `#[cfg(test)]`
  only for testing private helpers. Integration tests go in `tests/` as a
  single binary with domain submodules (one top-level `.rs` entry point,
  submodules in a matching directory). Do not use separate top-level test
  files with a shared `common/mod.rs` -- each top-level file compiles as
  its own binary, causing dead_code warnings on shared helpers.
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
3. **Fix the root cause** -- not just the symptom
4. **Avoid band-aids** -- don't just add a control branch; address the fundamental issue

The goal: leave the codebase stronger, not just patched.

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

- No `.unwrap()` or `.expect()` -- use `Result`/`Option` propagation. Tests are exempt.
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

## What Not To Do

- Don't add `salsa` annotations outside `lyra-db`
- Don't reorder `SyntaxKind` variants
- Don't break roundtrip (parse -> green tree text == original source)
- Don't introduce runtime panics in library code
- Don't add heavyweight dependencies without justification
- Don't use non-ASCII characters in any text file
