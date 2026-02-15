# Progress

Where we are against the roadmap. See `docs/roadmap.md` for milestone details.

## M0: Harness -- done

- [x] Workspace with all crates compiling
- [x] Salsa database with placeholder queries
- [x] Diagnostics as plain data
- [x] Source model primitives (FileId, Span, TextSize)
- [x] Snapshot test framework
- [x] CI (fmt, clippy, test, policy checks)

## M1: Syntax Core -- done

- [x] Lexer (full IEEE 1800-2023 token set)
- [x] Parser (grammar rules, rowan green trees)
- [x] Preprocess stage contract (source map, include dep graph, DB queries)
- [x] Typed AST wrappers with AstId
- [x] Parse error diagnostics with correct spans

## M2: Source Model and Incremental

- [x] Line index (byte offset to line/col mapping)
- [x] File text update with incremental invalidation
- [x] Cache-hit tests (EventDb, per-file isolation, no-change caching)
- [x] Preprocess invalidation via include deps
- [x] Source map model for macro expansion

## M3: Names and Scopes -- not started

## M4: Type Skeleton -- not started

## M5: Tool-Grade Foundation -- not started
