mod lower_diag;

use lyra_preprocess::{IncludeProvider, ResolvedInclude};
use lyra_semantic::def_index::DefIndex;
use lyra_semantic::global_index::{
    DefinitionKind, GlobalDefIndex, PackageScope, PackageScopeIndex,
};
use lyra_semantic::name_graph::NameGraph;
use lyra_semantic::resolve_index::{CoreResolveOutput, ResolveIndex};
use lyra_semantic::scopes::ScopeKind;
use lyra_semantic::symbols::{GlobalDefId, GlobalSymbolId};
use salsa::Setter;
use smol_str::SmolStr;

/// Sorted include-path lookup for deterministic O(log n) resolution.
///
/// Stored as a sorted `Vec` to satisfy Salsa input constraints
/// (`Clone + Eq + Hash`). Construction sorts entries by path;
/// lookup uses binary search.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct IncludeMap {
    entries: Vec<(String, SourceFile)>,
}

impl IncludeMap {
    /// Build an `IncludeMap` from unsorted entries. Sorts by path.
    pub fn new(mut entries: Vec<(String, SourceFile)>) -> Self {
        entries.sort_by(|(a, _), (b, _)| a.cmp(b));
        Self { entries }
    }

    /// Look up a path in O(log n).
    pub fn lookup(&self, path: &str) -> Option<SourceFile> {
        let idx = self
            .entries
            .binary_search_by(|(p, _)| p.as_str().cmp(path))
            .ok()?;
        Some(self.entries[idx].1)
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = &(String, SourceFile)> {
        self.entries.iter()
    }
}

/// A source file input for the Salsa database.
///
/// The `include_map` field carries resolution metadata: which include
/// paths map to which files. Only the tool layer should set this field
/// (via `set_include_map`). Long-term, this could move to a separate
/// tracked query to decouple content changes from resolution changes.
#[salsa::input]
pub struct SourceFile {
    pub file_id: lyra_source::FileId,
    #[return_ref]
    pub text: String,
    #[return_ref]
    pub include_map: IncludeMap,
}

/// A compilation unit: the set of source files compiled together.
///
/// IEEE 1800-2023 section 3.12.1: a compilation unit is a tool-defined
/// collection of source files. Module names in the definitions name space
/// (section 3.13(a)) are global within a compilation unit.
///
/// `files` must be sorted by `FileId` and deduplicated.
#[salsa::input]
pub struct CompilationUnit {
    #[return_ref]
    pub files: Vec<SourceFile>,
}

/// Create a `CompilationUnit` from a set of files, sorting and deduplicating.
pub fn new_compilation_unit(
    db: &dyn salsa::Database,
    mut files: Vec<SourceFile>,
) -> CompilationUnit {
    files.sort_by_key(|f| f.file_id(db));
    files.dedup_by_key(|f| f.file_id(db));
    CompilationUnit::new(db, files)
}

/// O(log n) lookup of `SourceFile` by `FileId` within a compilation unit.
pub fn source_file_by_id(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    file_id: lyra_source::FileId,
) -> Option<SourceFile> {
    let files = unit.files(db);
    let idx = files
        .binary_search_by_key(&file_id, |f| f.file_id(db))
        .ok()?;
    Some(files[idx])
}

/// Lex a source file into tokens (including trivia and EOF).
#[salsa::tracked(return_ref)]
pub fn lex_file(db: &dyn salsa::Database, file: SourceFile) -> Vec<lyra_lexer::Token> {
    lyra_lexer::lex(file.text(db))
}

/// Include provider that resolves paths via Salsa queries.
struct DbIncludeProvider<'a> {
    db: &'a dyn salsa::Database,
    include_map: &'a IncludeMap,
}

impl IncludeProvider for DbIncludeProvider<'_> {
    fn resolve(&self, path: &str) -> Option<ResolvedInclude<'_>> {
        let file = self.include_map.lookup(path)?;
        Some(ResolvedInclude {
            file_id: file.file_id(self.db),
            tokens: lex_file(self.db, file),
            text: file.text(self.db),
        })
    }
}

/// Run the preprocessor over lexed tokens.
#[salsa::tracked(return_ref)]
pub fn preprocess_file(
    db: &dyn salsa::Database,
    file: SourceFile,
) -> lyra_preprocess::PreprocOutput {
    let include_map = file.include_map(db);
    let provider = DbIncludeProvider { db, include_map };
    lyra_preprocess::preprocess(
        file.file_id(db),
        lex_file(db, file),
        file.text(db),
        &provider,
    )
}

/// Parse a source file into a lossless green tree with diagnostics.
#[salsa::tracked(return_ref)]
pub fn parse_file(db: &dyn salsa::Database, file: SourceFile) -> lyra_parser::Parse {
    let pp = preprocess_file(db, file);
    lyra_parser::parse(&pp.tokens, &pp.expanded_text)
}

/// Build the line index for a source file (Salsa-cached).
#[salsa::tracked(return_ref)]
pub fn line_index(db: &dyn salsa::Database, file: SourceFile) -> lyra_source::LineIndex {
    lyra_source::LineIndex::new(file.text(db))
}

/// Update the text of an existing source file, triggering Salsa
/// invalidation for all queries that depend on it.
pub fn update_file_text(db: &mut dyn salsa::Database, file: SourceFile, text: String) {
    file.set_text(db).to(text);
}

/// Access the source map for a preprocessed file.
pub fn source_map(db: &dyn salsa::Database, file: SourceFile) -> &lyra_preprocess::SourceMap {
    &preprocess_file(db, file).source_map
}

/// Access the include graph for a preprocessed file.
pub fn include_graph(db: &dyn salsa::Database, file: SourceFile) -> &lyra_preprocess::IncludeGraph {
    &preprocess_file(db, file).includes
}

/// Return the expansion frame for an offset in a file's expanded output.
///
/// Returns a single-element `Vec` if the offset falls in an included
/// region, or an empty `Vec` for identity-mapped (non-included)
/// positions. The `Vec` return type is forward-compatible with
/// recursive include expansion, which will produce multi-frame
/// stacks.
///
/// Currently, `preprocess()` only performs one level of include
/// expansion (splicing raw file text, not recursively expanded
/// output). Chasing `spelling` offsets across file boundaries would
/// compare raw-text offsets against expanded-output source maps,
/// producing incorrect provenance. Transitive chaining will be
/// added when the preprocessor gains recursive expansion.
pub fn full_expansion_stack(
    db: &dyn salsa::Database,
    file: SourceFile,
    offset: lyra_source::TextSize,
) -> Vec<lyra_source::ExpansionFrame> {
    let sm = source_map(db, file);
    sm.expansion_frame(offset).into_iter().collect()
}

/// Return a typed `SourceFile` AST root for the given file.
///
/// Returns `None` if the root node cannot be cast to `SourceFile` (should
/// not happen with a correct parser, but no panic in library code).
pub fn ast_root(db: &dyn salsa::Database, file: SourceFile) -> Option<lyra_ast::SourceFile> {
    let parse = parse_file(db, file);
    lyra_ast::AstNode::cast(parse.syntax())
}

/// Build the per-file `AstIdMap` (Salsa-cached).
#[salsa::tracked(return_ref)]
pub fn ast_id_map(db: &dyn salsa::Database, file: SourceFile) -> lyra_ast::AstIdMap {
    let parse = parse_file(db, file);
    lyra_ast::AstIdMap::from_root(file.file_id(db), &parse.syntax())
}

/// Build per-file definition index (Salsa-cached).
///
/// Collects declarations, scopes, exports, and use-sites from the parse tree.
/// Does NOT resolve name uses -- see `resolve_index_file`.
#[salsa::tracked(return_ref)]
pub fn def_index_file(db: &dyn salsa::Database, file: SourceFile) -> DefIndex {
    let parse = parse_file(db, file);
    let map = ast_id_map(db, file);
    lyra_semantic::build_def_index(file.file_id(db), parse, map)
}

/// Extract offset-independent name graph from the definition index (Salsa-cached).
///
/// On whitespace-only edits, this query re-executes but produces an equal
/// result (no ranges), so Salsa backdates it and skips re-running the
/// expensive `resolve_core_file` downstream.
#[salsa::tracked(return_ref)]
pub fn name_graph_file(db: &dyn salsa::Database, file: SourceFile) -> NameGraph {
    NameGraph::from_def_index(def_index_file(db, file))
}

/// Build the global definition index for a compilation unit (Salsa-cached).
///
/// Aggregates module and package names with `GlobalDefId`s from all files'
/// `DefIndex` exports. On whitespace-only edits, `ErasedAstId` values are
/// topology-stable, so the result backdates correctly.
#[salsa::tracked(return_ref)]
pub fn global_def_index(db: &dyn salsa::Database, unit: CompilationUnit) -> GlobalDefIndex {
    let mut entries: Vec<(SmolStr, GlobalDefId, DefinitionKind)> = Vec::new();
    for file in unit.files(db) {
        let def = def_index_file(db, *file);
        // Collect all definition-namespace constructs (module, interface, program, primitive, config)
        for &sym_id in &*def.exports.definitions {
            let sym = def.symbols.get(sym_id);
            if let Some(def_kind) = DefinitionKind::from_symbol_kind(sym.kind)
                && let Some(&Some(ast_id)) = def.symbol_to_decl.get(sym_id.index())
            {
                entries.push((sym.name.clone(), GlobalDefId::new(ast_id), def_kind));
            }
        }
        // Collect packages (separate namespace per LRM 3.13(b))
        for &sym_id in &*def.exports.packages {
            let sym = def.symbols.get(sym_id);
            if let Some(&Some(ast_id)) = def.symbol_to_decl.get(sym_id.index()) {
                entries.push((
                    sym.name.clone(),
                    GlobalDefId::new(ast_id),
                    DefinitionKind::Package,
                ));
            }
        }
    }
    lyra_semantic::global_index::build_global_def_index(&entries)
}

/// Build the package scope index for a compilation unit (Salsa-cached).
///
/// Extracts symbols from package scopes in all files, split by namespace.
#[salsa::tracked(return_ref)]
pub fn package_scope_index(db: &dyn salsa::Database, unit: CompilationUnit) -> PackageScopeIndex {
    let mut packages = Vec::new();
    for file in unit.files(db) {
        let def = def_index_file(db, *file);
        // Find package symbols and their scopes
        for &sym_id in &*def.exports.packages {
            let pkg_sym = def.symbols.get(sym_id);
            let pkg_scope = pkg_sym.scope;
            let scope_data = def.scopes.get(pkg_scope);
            if scope_data.kind != ScopeKind::Package {
                continue;
            }

            let mut value_ns: Vec<(SmolStr, GlobalDefId)> = Vec::new();
            let mut type_ns: Vec<(SmolStr, GlobalDefId)> = Vec::new();

            // Collect Value-namespace symbols from this package scope
            for &child_sym_id in &*scope_data.value_ns {
                let child_sym = def.symbols.get(child_sym_id);
                if let Some(&Some(ast_id)) = def.symbol_to_decl.get(child_sym_id.index()) {
                    value_ns.push((child_sym.name.clone(), GlobalDefId::new(ast_id)));
                }
            }

            // Collect Type-namespace symbols
            for &child_sym_id in &*scope_data.type_ns {
                let child_sym = def.symbols.get(child_sym_id);
                if let Some(&Some(ast_id)) = def.symbol_to_decl.get(child_sym_id.index()) {
                    type_ns.push((child_sym.name.clone(), GlobalDefId::new(ast_id)));
                }
            }

            value_ns.sort_by(|(a, _), (b, _)| a.cmp(b));
            type_ns.sort_by(|(a, _), (b, _)| a.cmp(b));

            packages.push(PackageScope {
                name: pkg_sym.name.clone(),
                value_ns: value_ns.into_boxed_slice(),
                type_ns: type_ns.into_boxed_slice(),
            });
        }
    }
    lyra_semantic::global_index::build_package_scope_index(packages)
}

/// Resolve all use-sites using only offset-independent data (Salsa-cached).
///
/// Depends on `name_graph_file`, `global_def_index`, and
/// `package_scope_index`. When all are backdated (e.g. whitespace edit),
/// this query is NOT re-executed.
#[salsa::tracked(return_ref)]
pub fn resolve_core_file(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
) -> CoreResolveOutput {
    let graph = name_graph_file(db, file);
    let global = global_def_index(db, unit);
    let pkg_scope = package_scope_index(db, unit);
    lyra_semantic::build_resolve_core(graph, global, pkg_scope)
}

/// Build per-file resolution index (Salsa-cached).
///
/// Combines offset-independent resolve results from `resolve_core_file`
/// with offset-dependent data from `def_index_file` (`ast_ids`, ranges)
/// to produce the final `HashMap` and diagnostics. Trivially cheap.
#[salsa::tracked(return_ref)]
pub fn resolve_index_file(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
) -> ResolveIndex {
    let def = def_index_file(db, file);
    let core = resolve_core_file(db, file, unit);
    let lookup_decl = |def_id: GlobalDefId| -> Option<lyra_semantic::symbols::SymbolId> {
        let target_file_id = def_id.file();
        let target_file = source_file_by_id(db, unit, target_file_id)?;
        let target_def = def_index_file(db, target_file);
        target_def.decl_to_symbol.get(&def_id.ast_id()).copied()
    };
    lyra_semantic::build_resolve_index(def, core, &lookup_decl)
}

/// Identifies a constant expression for evaluation.
///
/// Const-eval depends on the compilation unit because name resolution
/// (imports, global definitions, package visibility) is unit-scoped.
#[salsa::interned]
pub struct ConstExprRef<'db> {
    pub unit: CompilationUnit,
    pub expr_ast_id: lyra_ast::ErasedAstId,
}

/// Evaluate a constant integer expression (Salsa-tracked with cycle recovery).
///
/// Returns `ConstInt::Known(v)` on success, `ConstInt::Error(e)` on failure.
/// Cycles (e.g. `parameter A = B; parameter B = A;`) are detected by Salsa
/// and recovered via `const_eval_recover`.
#[salsa::tracked(recovery_fn = const_eval_recover)]
pub fn eval_const_int<'db>(
    db: &'db dyn salsa::Database,
    expr_ref: ConstExprRef<'db>,
) -> lyra_semantic::types::ConstInt {
    use lyra_semantic::types::{ConstEvalError, ConstInt};

    let unit = expr_ref.unit(db);
    let expr_ast_id = expr_ref.expr_ast_id(db);
    let file_id = expr_ast_id.file();

    let Some(source_file) = source_file_by_id(db, unit, file_id) else {
        return ConstInt::Error(ConstEvalError::Unresolved);
    };

    let parse = parse_file(db, source_file);
    let map = ast_id_map(db, source_file);

    let Some(node) = map.get_node(&parse.syntax(), expr_ast_id) else {
        return ConstInt::Error(ConstEvalError::Unresolved);
    };

    let resolve_name = |name_node: &lyra_parser::SyntaxNode| -> Result<i64, ConstEvalError> {
        // Look up the NameRef/QualifiedName's AstId
        let name_ast_id = map
            .erased_ast_id(name_node)
            .ok_or(ConstEvalError::Unresolved)?;

        // Resolve the name to a GlobalSymbolId
        let resolve = resolve_index_file(db, source_file, unit);
        let resolution = resolve
            .resolutions
            .get(&name_ast_id)
            .ok_or(ConstEvalError::Unresolved)?;

        // Look up target symbol -- only parameters are allowed
        let target_file_id = resolution.symbol.file;
        let target_local = resolution.symbol.local;
        let target_file =
            source_file_by_id(db, unit, target_file_id).ok_or(ConstEvalError::Unresolved)?;
        let target_def = def_index_file(db, target_file);
        let target_sym = target_def.symbols.get(target_local);

        if target_sym.kind != lyra_semantic::symbols::SymbolKind::Parameter {
            return Err(ConstEvalError::NonConstant);
        }

        // Look up declarator AstId via symbol_to_decl
        let decl_ast_id = target_def
            .symbol_to_decl
            .get(target_local.index())
            .and_then(|opt| *opt)
            .ok_or(ConstEvalError::Unresolved)?;

        // Look up init expression AstId
        let init_ast_id = target_def
            .decl_to_init_expr
            .get(&decl_ast_id)
            .ok_or(ConstEvalError::Unresolved)?
            .ok_or(ConstEvalError::Unresolved)?;

        // Recursively evaluate via Salsa (cycle detection here)
        let init_ref = ConstExprRef::new(db, unit, init_ast_id);
        match eval_const_int(db, init_ref) {
            ConstInt::Known(v) => Ok(v),
            ConstInt::Error(e) => Err(e),
            ConstInt::Unevaluated(_) => Err(ConstEvalError::Unsupported),
        }
    };

    match lyra_semantic::const_eval::eval_const_expr(&node, &resolve_name) {
        Ok(v) => ConstInt::Known(v),
        Err(e) => ConstInt::Error(e),
    }
}

fn const_eval_recover<'db>(
    _db: &'db dyn salsa::Database,
    _cycle: &salsa::Cycle,
    _expr_ref: ConstExprRef<'db>,
) -> lyra_semantic::types::ConstInt {
    lyra_semantic::types::ConstInt::Error(lyra_semantic::types::ConstEvalError::Cycle)
}

/// Identifies a symbol for type extraction.
///
/// Includes the compilation unit because typedef resolution and
/// const-eval depend on unit-scoped name resolution.
#[salsa::interned]
pub struct SymbolRef<'db> {
    pub unit: CompilationUnit,
    pub symbol: GlobalSymbolId,
}

/// Extract a symbol's raw type from its declaration AST (Salsa-tracked with cycle recovery).
///
/// Returns `SymbolType` with `Unevaluated` dims -- const-eval is NOT called.
/// May call `resolve_index_file` for single-step typedef expansion.
#[salsa::tracked(recovery_fn = type_of_symbol_raw_recover)]
pub fn type_of_symbol_raw<'db>(
    db: &'db dyn salsa::Database,
    sym_ref: SymbolRef<'db>,
) -> lyra_semantic::types::SymbolType {
    use lyra_semantic::symbols::SymbolKind;
    use lyra_semantic::types::{SymbolType, SymbolTypeError};
    use lyra_semantic::{extract_type_from_container, typespec_name_ref};

    let unit = sym_ref.unit(db);
    let gsym = sym_ref.symbol(db);

    let Some(source_file) = source_file_by_id(db, unit, gsym.file) else {
        return SymbolType::Error(SymbolTypeError::MissingDecl);
    };

    let def = def_index_file(db, source_file);
    let sym = def.symbols.get(gsym.local);

    // Definition-namespace symbols have no meaningful type
    match sym.kind {
        SymbolKind::Module
        | SymbolKind::Package
        | SymbolKind::Interface
        | SymbolKind::Program
        | SymbolKind::Primitive
        | SymbolKind::Config => return SymbolType::Error(SymbolTypeError::UnsupportedSymbolKind),
        _ => {}
    }

    let Some(decl_ast_id) = def.symbol_to_decl.get(gsym.local.index()).and_then(|o| *o) else {
        return SymbolType::Error(SymbolTypeError::MissingDecl);
    };

    let parse = parse_file(db, source_file);
    let map = ast_id_map(db, source_file);

    let Some(decl_node) = map.get_node(&parse.syntax(), decl_ast_id) else {
        return SymbolType::Error(SymbolTypeError::MissingDecl);
    };

    // For Port and TypedefDecl, the decl_ast_id points directly to the node.
    // For Variable/Net/Parameter, it points to a Declarator -- find the container.
    let (container, declarator) = match decl_node.kind() {
        lyra_lexer::SyntaxKind::Port | lyra_lexer::SyntaxKind::TypedefDecl => {
            (decl_node.clone(), None)
        }
        lyra_lexer::SyntaxKind::Declarator => {
            let Some(parent) = closest_decl_container(&decl_node) else {
                return SymbolType::Error(SymbolTypeError::MissingDecl);
            };
            (parent, Some(decl_node.clone()))
        }
        _ => return SymbolType::Error(SymbolTypeError::MissingDecl),
    };

    // Check for user-defined type (typedef expansion trigger)
    let typespec = container
        .children()
        .find(|c| c.kind() == lyra_lexer::SyntaxKind::TypeSpec);

    if let Some(ref ts) = typespec
        && typespec_name_ref(ts).is_some()
    {
        // For Port nodes, unpacked dims are direct children of the Port node
        // (not inside a Declarator), so pass the Port node as dim source.
        let dim_source = if decl_node.kind() == lyra_lexer::SyntaxKind::Port {
            Some(&decl_node)
        } else {
            declarator.as_ref()
        };
        return expand_typedef(db, unit, source_file, ts, dim_source, map, sym.kind);
    }

    // No user-defined type -- pure extraction
    extract_type_from_container(&container, declarator.as_ref(), map)
}

/// Expand a user-defined type reference via single-step typedef resolution.
///
/// `dim_source` is the node whose children contain unpacked dims to merge
/// (Declarator for variables, Port node for ports, None for typedefs).
/// `caller_kind` is the symbol kind of the declaration being typed, used
/// to preserve `TypeAlias` classification for typedef symbols.
fn expand_typedef(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    source_file: SourceFile,
    typespec: &lyra_parser::SyntaxNode,
    dim_source: Option<&lyra_parser::SyntaxNode>,
    id_map: &lyra_ast::AstIdMap,
    caller_kind: lyra_semantic::symbols::SymbolKind,
) -> lyra_semantic::types::SymbolType {
    use lyra_semantic::types::{SymbolType, SymbolTypeError, Ty, UnpackedDim};
    use lyra_semantic::typespec_name_ref;

    let Some(name_node) = typespec_name_ref(typespec) else {
        return SymbolType::Error(SymbolTypeError::UserTypeUnresolved);
    };

    // Look up the name in the resolve index
    let resolve = resolve_index_file(db, source_file, unit);
    let Some(name_ast_id) = id_map.erased_ast_id(&name_node) else {
        return SymbolType::Error(SymbolTypeError::UserTypeUnresolved);
    };
    let Some(resolution) = resolve.resolutions.get(&name_ast_id) else {
        return SymbolType::Error(SymbolTypeError::UserTypeUnresolved);
    };

    let target_id = resolution.symbol;

    // Look up target symbol kind
    let Some(target_file) = source_file_by_id(db, unit, target_id.file) else {
        return SymbolType::Error(SymbolTypeError::UserTypeUnresolved);
    };
    let target_def = def_index_file(db, target_file);
    let target_info = target_def.symbols.get(target_id.local);

    if target_info.kind != lyra_semantic::symbols::SymbolKind::Typedef {
        return SymbolType::Error(SymbolTypeError::UserTypeUnresolved);
    }

    // Recursively get the typedef's type (Salsa cycle detection)
    let typedef_ref = SymbolRef::new(db, unit, target_id);
    let typedef_type = type_of_symbol_raw(db, typedef_ref);

    // Extract the underlying Ty from the typedef result
    let underlying_ty = match &typedef_type {
        SymbolType::TypeAlias(ty) | SymbolType::Value(ty) => ty.clone(),
        SymbolType::Error(e) => return SymbolType::Error(*e),
        SymbolType::Net(_) => return SymbolType::Error(SymbolTypeError::UserTypeUnresolved),
    };

    // Collect use-site unpacked dims from dim source node
    let use_site_unpacked: Vec<UnpackedDim> = dim_source
        .map(|d| extract_unpacked_dims_raw(d, id_map))
        .unwrap_or_default();

    // Wrap the result in the correct classification
    let wrap = if caller_kind == lyra_semantic::symbols::SymbolKind::Typedef {
        SymbolType::TypeAlias
    } else {
        SymbolType::Value
    };

    if use_site_unpacked.is_empty() {
        return wrap(underlying_ty);
    }

    // Merge unpacked dims: typedef dims first, use-site dims appended after
    match underlying_ty {
        Ty::Integral(mut i) => {
            let mut merged: Vec<UnpackedDim> = i.unpacked.to_vec();
            merged.extend(use_site_unpacked);
            i.unpacked = merged.into_boxed_slice();
            wrap(Ty::Integral(i))
        }
        _ => SymbolType::Error(SymbolTypeError::UserTypeUnresolved),
    }
}

/// Extract unpacked dims from a node (used during typedef expansion in db crate).
fn extract_unpacked_dims_raw(
    node: &lyra_parser::SyntaxNode,
    ast_id_map: &lyra_ast::AstIdMap,
) -> Vec<lyra_semantic::types::UnpackedDim> {
    use lyra_semantic::types::{ConstEvalError, ConstInt, UnpackedDim};

    let mut dims = Vec::new();
    for child in node.children() {
        if child.kind() == lyra_lexer::SyntaxKind::UnpackedDimension {
            let exprs: Vec<_> = child
                .children()
                .filter(|c| is_expression_kind(c.kind()))
                .collect();
            match exprs.len() {
                2 => {
                    let msb = expr_to_const_int(&exprs[0], ast_id_map);
                    let lsb = expr_to_const_int(&exprs[1], ast_id_map);
                    dims.push(UnpackedDim::Range { msb, lsb });
                }
                1 => {
                    let size = expr_to_const_int(&exprs[0], ast_id_map);
                    dims.push(UnpackedDim::Size(size));
                }
                _ => {
                    dims.push(UnpackedDim::Size(ConstInt::Error(
                        ConstEvalError::Unsupported,
                    )));
                }
            }
        }
    }
    dims
}

fn expr_to_const_int(
    expr: &lyra_parser::SyntaxNode,
    ast_id_map: &lyra_ast::AstIdMap,
) -> lyra_semantic::types::ConstInt {
    use lyra_semantic::types::{ConstEvalError, ConstInt};
    match ast_id_map.erased_ast_id(expr) {
        Some(id) => ConstInt::Unevaluated(id),
        None => ConstInt::Error(ConstEvalError::Unsupported),
    }
}

fn is_expression_kind(kind: lyra_lexer::SyntaxKind) -> bool {
    matches!(
        kind,
        lyra_lexer::SyntaxKind::Expression
            | lyra_lexer::SyntaxKind::BinExpr
            | lyra_lexer::SyntaxKind::PrefixExpr
            | lyra_lexer::SyntaxKind::ParenExpr
            | lyra_lexer::SyntaxKind::CondExpr
            | lyra_lexer::SyntaxKind::ConcatExpr
            | lyra_lexer::SyntaxKind::ReplicExpr
            | lyra_lexer::SyntaxKind::IndexExpr
            | lyra_lexer::SyntaxKind::RangeExpr
            | lyra_lexer::SyntaxKind::FieldExpr
            | lyra_lexer::SyntaxKind::CallExpr
            | lyra_lexer::SyntaxKind::NameRef
            | lyra_lexer::SyntaxKind::Literal
            | lyra_lexer::SyntaxKind::QualifiedName
    )
}

fn type_of_symbol_raw_recover<'db>(
    _db: &'db dyn salsa::Database,
    _cycle: &salsa::Cycle,
    _sym_ref: SymbolRef<'db>,
) -> lyra_semantic::types::SymbolType {
    lyra_semantic::types::SymbolType::Error(lyra_semantic::types::SymbolTypeError::TypedefCycle)
}

/// Find the closest declaration container above a Declarator.
fn closest_decl_container(node: &lyra_parser::SyntaxNode) -> Option<lyra_parser::SyntaxNode> {
    node.ancestors().find(|n| {
        matches!(
            n.kind(),
            lyra_lexer::SyntaxKind::VarDecl
                | lyra_lexer::SyntaxKind::NetDecl
                | lyra_lexer::SyntaxKind::ParamDecl
        )
    })
}

/// Extract the normalized type of a symbol (Salsa-tracked).
///
/// Calls `type_of_symbol_raw` then normalizes all `Unevaluated` dims
/// via const-eval.
#[salsa::tracked]
pub fn type_of_symbol<'db>(
    db: &'db dyn salsa::Database,
    sym_ref: SymbolRef<'db>,
) -> lyra_semantic::types::SymbolType {
    let raw = type_of_symbol_raw(db, sym_ref);
    let unit = sym_ref.unit(db);
    lyra_semantic::normalize_symbol_type(&raw, &|expr_ast_id| {
        let expr_ref = ConstExprRef::new(db, unit, expr_ast_id);
        eval_const_int(db, expr_ref)
    })
}

/// Resolve the name at a cursor position.
///
/// Finds the nearest `NameRef` at `offset`, looks up its `AstId`,
/// and returns the resolved `GlobalSymbolId` if found.
/// Also handles module instantiation type names (e.g. `adder` in
/// `adder u1(...)`) and qualified names (`pkg::sym`).
pub fn resolve_at(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
    offset: lyra_source::TextSize,
) -> Option<GlobalSymbolId> {
    let parse = parse_file(db, file);
    let ast_map = ast_id_map(db, file);
    let resolve = resolve_index_file(db, file, unit);

    // Try NameRef first (common case)
    if let Some(name_ref) = find_name_ref_at(&parse.syntax(), offset)
        && let Some(ast_id) = ast_map.ast_id(&name_ref)
        && let Some(resolution) = resolve.resolutions.get(&ast_id.erase())
    {
        return Some(resolution.symbol);
    }

    // Try qualified name (pkg::sym)
    if let Some(result) = find_qualified_name_at(db, file, unit, &parse.syntax(), offset) {
        return Some(result);
    }

    // Fallback: module instantiation type name
    if let Some(inst) = find_module_instantiation_name_at(&parse.syntax(), offset)
        && let Some(ast_id) = ast_map.ast_id(&inst)
        && let Some(resolution) = resolve.resolutions.get(&ast_id.erase())
    {
        return Some(resolution.symbol);
    }

    None
}

/// Look up a symbol by its global id within a compilation unit.
pub fn symbol_global(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    id: GlobalSymbolId,
) -> Option<&lyra_semantic::symbols::Symbol> {
    let file = source_file_by_id(db, unit, id.file)?;
    Some(def_index_file(db, file).symbols.get(id.local))
}

/// Find a `NameRef` node at or near the given offset.
fn find_name_ref_at(
    root: &lyra_parser::SyntaxNode,
    offset: lyra_source::TextSize,
) -> Option<lyra_ast::NameRef> {
    use lyra_ast::AstNode;
    let token = root.token_at_offset(offset).right_biased()?;
    token.parent_ancestors().find_map(lyra_ast::NameRef::cast)
}

/// Find a qualified name at the cursor position and resolve it.
///
/// When on the package name part: resolve to the package declaration.
/// When on the member name part: look up the `QualifiedName`'s `AstId`
/// in the resolve index.
fn find_qualified_name_at(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
    root: &lyra_parser::SyntaxNode,
    offset: lyra_source::TextSize,
) -> Option<GlobalSymbolId> {
    use lyra_ast::AstNode;
    let token = root.token_at_offset(offset).right_biased()?;
    let qn = token
        .parent_ancestors()
        .find_map(lyra_ast::QualifiedName::cast)?;

    let segments: Vec<_> = qn.segments().collect();
    if segments.len() < 2 {
        return None;
    }

    // Determine if cursor is on the package name or member name
    let on_package = segments[0].text_range().contains(offset);

    if on_package {
        // Resolve to the package declaration
        let pkg_name = segments[0].text();
        let global = global_def_index(db, unit);
        let def_id = global.resolve_package(pkg_name.as_ref())?;
        let target_file = source_file_by_id(db, unit, def_id.file())?;
        let target_def = def_index_file(db, target_file);
        let local = target_def.decl_to_symbol.get(&def_id.ast_id()).copied()?;
        Some(GlobalSymbolId {
            file: def_id.file(),
            local,
        })
    } else {
        // Resolve via the QualifiedName's AstId in the resolve index
        let ast_map = ast_id_map(db, file);
        let resolve = resolve_index_file(db, file, unit);
        let ast_id = ast_map.ast_id(&qn)?;
        let resolution = resolve.resolutions.get(&ast_id.erase())?;
        Some(resolution.symbol)
    }
}

/// Find a `ModuleInstantiation` node where the cursor is on the module
/// type name (not the instance name).
fn find_module_instantiation_name_at(
    root: &lyra_parser::SyntaxNode,
    offset: lyra_source::TextSize,
) -> Option<lyra_ast::ModuleInstantiation> {
    use lyra_ast::AstNode;
    let token = root.token_at_offset(offset).right_biased()?;
    let inst = token
        .parent_ancestors()
        .find_map(lyra_ast::ModuleInstantiation::cast)?;
    // Only match if offset is within the module name token
    let name_token = inst.module_name()?;
    if name_token.text_range().contains(offset) {
        Some(inst)
    } else {
        None
    }
}

/// Convert parse, preprocess, and semantic errors into structured diagnostics (Salsa-cached).
#[salsa::tracked(return_ref)]
pub fn file_diagnostics(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
) -> Vec<lyra_diag::Diagnostic> {
    let pp = preprocess_file(db, file);
    let parse = parse_file(db, file);
    let def = def_index_file(db, file);
    let resolve = resolve_index_file(db, file, unit);
    lower_diag::lower_file_diagnostics(file.file_id(db), pp, parse, def, resolve)
}

/// Unit-level diagnostics: duplicate definitions in the definitions namespace.
///
/// Walks `GlobalDefIndex.definitions()`, finds adjacent entries with the
/// same name, and emits one diagnostic per duplicate. Catches module/module,
/// package/package, and module/package name collisions.
#[salsa::tracked(return_ref)]
pub fn unit_diagnostics(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
) -> Box<[lyra_diag::Diagnostic]> {
    let global = global_def_index(db, unit);
    let defs = global.definitions();
    let mut diags = Vec::new();

    let mut i = 0;
    while i < defs.len() {
        let (name, _, _) = &defs[i];
        let mut j = i + 1;
        while j < defs.len() && defs[j].0 == *name {
            j += 1;
        }
        if j - i > 1 {
            // Duplicate group: emit diagnostics for entries [i+1..j]
            for (_, dup_def_id, _) in &defs[(i + 1)..j] {
                let dup_file_id = dup_def_id.file();
                if let Some(dup_file) = source_file_by_id(db, unit, dup_file_id) {
                    let dup_def = def_index_file(db, dup_file);
                    if let Some(&sym_id) = dup_def.decl_to_symbol.get(&dup_def_id.ast_id()) {
                        let sym = dup_def.symbols.get(sym_id);
                        let pp = preprocess_file(db, dup_file);
                        if let Some(span) = pp.source_map.map_span(sym.def_range) {
                            diags.push(
                                lyra_diag::Diagnostic::new(
                                    lyra_diag::Severity::Error,
                                    lyra_diag::DiagnosticCode::DUPLICATE_DEFINITION,
                                    lyra_diag::Message::new(
                                        lyra_diag::MessageId::DuplicateDefinitionInUnit,
                                        vec![lyra_diag::Arg::Name(name.clone())],
                                    ),
                                )
                                .with_label(lyra_diag::Label {
                                    kind: lyra_diag::LabelKind::Primary,
                                    span,
                                    message: lyra_diag::Message::simple(
                                        lyra_diag::MessageId::RedefinedHere,
                                    ),
                                }),
                            );
                        }
                    }
                }
            }
        }
        i = j;
    }

    diags.into_boxed_slice()
}

/// The central Salsa database for Lyra.
#[salsa::db]
#[derive(Default, Clone)]
pub struct LyraDatabase {
    storage: salsa::Storage<Self>,
}

#[salsa::db]
impl salsa::Database for LyraDatabase {
    fn salsa_event(&self, _event: &dyn Fn() -> salsa::Event) {}
}

#[cfg(test)]
mod tests;
