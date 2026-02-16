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

## M3: Names and Scopes

- [x] Declarator AST node (parser wraps each declarator)
- [x] Symbol/SymbolTable with SmolStr names, SymbolKind, TextRange def_range
- [x] ScopeTree with sorted bindings, binary-search resolve, parent-chain walk
- [x] Structured semantic diagnostics (SemanticDiagKind enum)
- [x] DefIndex: per-file definition collection (symbols, scopes, exports, use sites)
- [x] ResolveIndex: per-file name resolution (HashMap<ErasedAstId, SymbolId>)
- [x] Builder: build_def_index, build_resolve_core, build_resolve_index
- [x] Salsa queries: def_index_file, name_graph_file, resolve_core_file, resolve_index_file, resolve_at, symbol_global
- [x] file_diagnostics includes semantic diagnostics
- [x] Incremental invalidation tests (cache hit, recompute, cross-file isolation)
- [x] Offset-independent NameGraph query (whitespace edits skip resolve_core via Salsa backdating)
- [x] Cross-file module instantiation resolution (CompilationUnit, GlobalDefIndex, Definition namespace)
- [x] Cursor resolution for module instantiation type names (find_module_instantiation_name_at fallback)
- [x] Unit-level diagnostics for duplicate global definitions (module/package collisions)
- [x] Package declarations: parse, AST, semantic collection, cross-file resolution
- [x] Import declarations: explicit (pkg::sym) and wildcard (pkg::*), with LRM precedence
- [x] Qualified name references (pkg::sym) in expressions with cross-file resolution
- [x] PackageScopeIndex: namespace-aware (value_ns/type_ns) package member resolution
- [x] CoreResolveResult with typed UnresolvedReason codes (PackageNotFound, MemberNotFound, AmbiguousWildcardImport, UnsupportedQualifiedPath)
- [x] Import validation: package existence and member existence checks with diagnostics
- [x] Cursor resolution for qualified names (package part -> package decl, member part -> member symbol)
- [x] Typedef declarations with type namespace resolution (local, cross-file, import)
- [x] Multi-namespace resolution: type-position names try Type then Value
- [x] Cross-file resolution for all top-level constructs (interface, program, primitive, config)

## Cross-cutting infrastructure

- [x] Salsa query granularity: NameGraph decouples offset-dependent parse from offset-independent resolution
- [x] Diagnostic codes, labels, and fixits
- [x] AST API extensibility (reduce hand-written boilerplate)
- [x] Test harness: snapshot corpus runner

## M4: Type Skeleton -- not started

## M5: Tool-Grade Foundation -- not started
