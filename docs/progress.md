# Progress

Where we are against the roadmap. See `docs/roadmap.md` for milestone details.
Remaining gaps live in `docs/lrm/gaps.md`.

## M0: Harness -- done

- [x] Workspace with all crates compiling
- [x] Salsa database with placeholder queries
- [x] Diagnostics as plain data
- [x] Source model primitives
- [x] Snapshot test framework
- [x] CI

## M1: Syntax Core -- done

- [x] Lexer
- [x] Parser
- [x] Preprocess stage contract
- [x] Typed AST wrappers with AstId
- [x] Parse error diagnostics

## M2: Source Model and Incremental -- done

- [x] Line index
- [x] File text update with incremental invalidation
- [x] Cache-hit tests
- [x] Preprocess invalidation via include deps
- [x] Source map model for macro expansion

## M3: Names and Scopes -- done

- [x] Symbol table and scope tree
- [x] Per-file definition collection and name resolution
- [x] Cross-file resolution
- [x] Package imports with LRM precedence
- [x] Typedef declarations
- [x] Semantic diagnostics

## M4: Type Skeleton -- done

- [x] Type data model
- [x] Constant expression evaluation
- [x] type_of_symbol query
- [x] type_of_expr query
- [x] Type-error diagnostics
- [x] Enum/struct representation

## M5: Batch Semantic Engine -- done

- [x] Module signature query
- [x] Instantiation resolution
- [x] Instance tree
- [x] Port diagnostics
- [x] CLI batch driver

## M6: Elaboration Completeness -- in progress

- [x] Context-determined typing
- [x] Callable typing
- [x] Struct/union semantics
- [x] Enum semantics
- [x] Generate blocks and parameter elaboration
- [x] System function typing
- [x] Interface/modport semantics
- [ ] Array semantics
- [ ] Performance baseline
