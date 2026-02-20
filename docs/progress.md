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

## M2: Source Model and Incremental -- done

- [x] Line index (byte offset to line/col mapping)
- [x] File text update with incremental invalidation
- [x] Cache-hit tests (per-file isolation, no-change caching)
- [x] Preprocess invalidation via include deps
- [x] Source map model for macro expansion

## M3: Names and Scopes -- done

- [x] Symbol table and scope tree with parent-chain resolution
- [x] Per-file definition collection and name resolution
- [x] Cross-file resolution (modules, packages, interfaces, programs, primitives, configs)
- [x] Package imports (explicit and wildcard) with LRM precedence
- [x] Typedef declarations with type namespace resolution
- [x] Semantic diagnostics (unresolved names, duplicates, import errors)

## M4: Type Skeleton -- done

- [x] Type data model (integral types, dimensions, enums, structs, nets)
- [x] Constant expression evaluation for dimension bounds
- [x] type_of_symbol query
- [x] type_of_expr query for simple expressions
- [x] Basic type-error diagnostics (width mismatch)
- [x] Undeclared type diagnostics
- [x] Enum/struct representation

## M5: Batch Semantic Engine -- done

- [x] Module signature query (ports, params)
- [x] Instantiation resolution (named and positional)
- [x] Instance tree for a chosen top module
- [x] Port diagnostics (unknown, missing, duplicate, too-many-positional)
- [x] CLI batch driver

## M6: Elaboration Completeness -- in progress

- [x] Context-determined typing (LRM 11.6)
- [x] Callable typing (function/task calls)
- [ ] Struct/union/enum semantics
- [ ] Full array semantics
- [x] Generate blocks and parameter elaboration
- [ ] Interface/modport semantics
- [ ] Performance baseline (10K lines under 1s)
