use std::collections::HashMap;

use lyra_ast::{AstIdMap, AstNode, ModuleInstantiation, NameRef, QualifiedName};
use lyra_lexer::SyntaxKind;
use lyra_parser::{Parse, SyntaxNode};
use lyra_source::{FileId, TextRange};
use smol_str::SmolStr;

use crate::def_index::{DefIndex, Exports, Import, ImportName, NamePath, UseSite};
use crate::diagnostic::{SemanticDiag, SemanticDiagKind};
use crate::global_index::{GlobalDefIndex, PackageScopeIndex};
use crate::name_graph::NameGraph;
use crate::resolve_index::{
    CoreResolution, CoreResolveOutput, CoreResolveResult, ImportError, Resolution, ResolveIndex,
    UnresolvedReason,
};
use crate::scopes::{ScopeId, ScopeKind, ScopeTreeBuilder};
use crate::symbols::{
    GlobalDefId, GlobalSymbolId, Namespace, Symbol, SymbolId, SymbolKind, SymbolTableBuilder,
};

pub fn build_def_index(file: FileId, parse: &Parse, ast_id_map: &AstIdMap) -> DefIndex {
    let mut ctx = DefContext::new(ast_id_map);
    let root = parse.syntax();

    // Root file scope
    let file_scope = ctx.scopes.push(ScopeKind::File, None);
    collect_source_file(&mut ctx, &root, file_scope);

    // Freeze symbols first, then scopes (which need symbol names for sorting)
    let symbols = ctx.symbols.freeze();
    let scopes = ctx.scopes.freeze(&symbols);

    // Sort export module/package ids by symbol name
    ctx.export_modules
        .sort_by(|a, b| symbols.get(*a).name.cmp(&symbols.get(*b).name));
    ctx.export_packages
        .sort_by(|a, b| symbols.get(*a).name.cmp(&symbols.get(*b).name));

    // Detect duplicate definitions within each scope, per namespace
    let mut diagnostics = Vec::new();
    for scope_idx in 0..scopes.len() {
        let s = scopes.get(ScopeId(scope_idx as u32));
        detect_duplicates(&symbols, &s.value_ns, &mut diagnostics);
        detect_duplicates(&symbols, &s.type_ns, &mut diagnostics);
    }

    DefIndex {
        file,
        symbols,
        scopes,
        exports: Exports {
            modules: ctx.export_modules.into_boxed_slice(),
            packages: ctx.export_packages.into_boxed_slice(),
        },
        use_sites: ctx.use_sites.into_boxed_slice(),
        imports: ctx.imports.into_boxed_slice(),
        decl_to_symbol: ctx.decl_to_symbol,
        diagnostics: diagnostics.into_boxed_slice(),
    }
}

fn detect_duplicates(
    symbols: &crate::symbols::SymbolTable,
    bindings: &[SymbolId],
    diagnostics: &mut Vec<SemanticDiag>,
) {
    for i in 1..bindings.len() {
        let prev = symbols.get(bindings[i - 1]);
        let curr = symbols.get(bindings[i]);
        if prev.name == curr.name {
            diagnostics.push(SemanticDiag {
                kind: SemanticDiagKind::DuplicateDefinition {
                    name: curr.name.clone(),
                    original: prev.def_range,
                },
                range: curr.def_range,
            });
        }
    }
}

/// Resolve all use-sites using only offset-independent data.
///
/// Returns `CoreResolveOutput` with reason codes for unresolved names.
///
/// Resolution precedence for `Value`/`Type` namespace (LRM 26.3):
/// 1. Lexical scope (local declarations)
/// 2. Explicit imports in scope chain
/// 3. Wildcard imports in scope chain
///
/// `Definition` namespace: `GlobalDefIndex::resolve_definition`.
/// `Qualified` paths: resolved via `GlobalDefIndex` + `PackageScopeIndex`.
pub fn build_resolve_core(
    graph: &NameGraph,
    global: &GlobalDefIndex,
    pkg_scope: &PackageScopeIndex,
) -> CoreResolveOutput {
    // Validate imports first
    let mut import_errors = Vec::new();
    for (idx, imp) in graph.imports.iter().enumerate() {
        if !pkg_scope.has_package(&imp.package) {
            import_errors.push(ImportError {
                import_idx: idx as u32,
                reason: UnresolvedReason::PackageNotFound {
                    package: imp.package.clone(),
                },
            });
            continue;
        }
        if let ImportName::Explicit(ref member) = imp.name
            && pkg_scope.resolve_any_ns(&imp.package, member).is_none()
        {
            import_errors.push(ImportError {
                import_idx: idx as u32,
                reason: UnresolvedReason::MemberNotFound {
                    package: imp.package.clone(),
                    member: member.clone(),
                },
            });
        }
    }

    let resolutions: Box<[CoreResolveResult]> = graph
        .use_entries
        .iter()
        .map(|entry| match &entry.path {
            NamePath::Simple(name) => match entry.expected_ns {
                Namespace::Value | Namespace::Type => {
                    // 1. Lexical scope
                    if let Some(sym_id) =
                        graph
                            .scopes
                            .resolve(graph, entry.scope, entry.expected_ns, name)
                    {
                        return CoreResolveResult::Resolved(CoreResolution::Local {
                            symbol: sym_id,
                            namespace: graph.symbol_kinds[sym_id.0 as usize].namespace(),
                        });
                    }
                    // 2. Explicit imports in scope chain
                    if let Some(resolution) = resolve_via_explicit_import(
                        graph,
                        pkg_scope,
                        entry.scope,
                        name,
                        entry.expected_ns,
                    ) {
                        return CoreResolveResult::Resolved(resolution);
                    }
                    // 3. Wildcard imports in scope chain
                    match resolve_via_wildcard_import(
                        graph,
                        pkg_scope,
                        entry.scope,
                        name,
                        entry.expected_ns,
                    ) {
                        WildcardResult::Found(resolution) => {
                            CoreResolveResult::Resolved(resolution)
                        }
                        WildcardResult::Ambiguous(candidates) => CoreResolveResult::Unresolved(
                            UnresolvedReason::AmbiguousWildcardImport { candidates },
                        ),
                        WildcardResult::NotFound => {
                            CoreResolveResult::Unresolved(UnresolvedReason::NotFound)
                        }
                    }
                }
                Namespace::Definition => {
                    if let Some((def_id, _)) = global.resolve_definition(name) {
                        CoreResolveResult::Resolved(CoreResolution::Global {
                            decl: def_id,
                            namespace: Namespace::Definition,
                        })
                    } else {
                        CoreResolveResult::Unresolved(UnresolvedReason::NotFound)
                    }
                }
            },
            NamePath::Qualified { segments } => {
                resolve_qualified(segments, global, pkg_scope, entry.expected_ns)
            }
        })
        .collect();

    CoreResolveOutput {
        resolutions,
        import_errors: import_errors.into_boxed_slice(),
    }
}

fn resolve_qualified(
    segments: &[SmolStr],
    global: &GlobalDefIndex,
    pkg_scope: &PackageScopeIndex,
    use_ns: Namespace,
) -> CoreResolveResult {
    if segments.len() != 2 {
        return CoreResolveResult::Unresolved(UnresolvedReason::UnsupportedQualifiedPath {
            len: segments.len(),
        });
    }
    let pkg_name = &segments[0];
    let member_name = &segments[1];

    if global.resolve_package(pkg_name).is_none() {
        return CoreResolveResult::Unresolved(UnresolvedReason::PackageNotFound {
            package: pkg_name.clone(),
        });
    }

    // Try requested namespace first, fall back to Value for expression context
    let ns = if use_ns == Namespace::Definition {
        Namespace::Value
    } else {
        use_ns
    };
    if let Some(def_id) = pkg_scope.resolve(pkg_name, member_name, ns) {
        return CoreResolveResult::Resolved(CoreResolution::Global {
            decl: def_id,
            namespace: ns,
        });
    }
    CoreResolveResult::Unresolved(UnresolvedReason::MemberNotFound {
        package: pkg_name.clone(),
        member: member_name.clone(),
    })
}

fn resolve_via_explicit_import(
    graph: &NameGraph,
    pkg_scope: &PackageScopeIndex,
    scope: ScopeId,
    name: &str,
    ns: Namespace,
) -> Option<CoreResolution> {
    // Walk scope chain looking for explicit imports
    let mut current = Some(scope);
    while let Some(sid) = current {
        for imp in &*graph.imports {
            if imp.scope == sid
                && let ImportName::Explicit(ref member) = imp.name
                && member.as_str() == name
                && let Some(def_id) = pkg_scope.resolve(&imp.package, name, ns)
            {
                return Some(CoreResolution::Global {
                    decl: def_id,
                    namespace: ns,
                });
            }
        }
        current = graph.scopes.get(sid).parent;
    }
    None
}

enum WildcardResult {
    Found(CoreResolution),
    Ambiguous(Box<[SmolStr]>),
    NotFound,
}

fn resolve_via_wildcard_import(
    graph: &NameGraph,
    pkg_scope: &PackageScopeIndex,
    scope: ScopeId,
    name: &str,
    ns: Namespace,
) -> WildcardResult {
    let mut current = Some(scope);
    while let Some(sid) = current {
        let mut found: Option<(GlobalDefId, SmolStr)> = None;
        let mut ambiguous_pkgs: Vec<SmolStr> = Vec::new();

        for imp in &*graph.imports {
            if imp.scope == sid
                && imp.name == ImportName::Wildcard
                && let Some(def_id) = pkg_scope.resolve(&imp.package, name, ns)
            {
                if let Some((existing_id, _)) = &found {
                    if *existing_id != def_id {
                        if ambiguous_pkgs.is_empty() {
                            ambiguous_pkgs
                                .push(found.as_ref().map(|(_, p)| p.clone()).unwrap_or_default());
                        }
                        ambiguous_pkgs.push(imp.package.clone());
                    }
                } else {
                    found = Some((def_id, imp.package.clone()));
                }
            }
        }

        if !ambiguous_pkgs.is_empty() {
            return WildcardResult::Ambiguous(ambiguous_pkgs.into_boxed_slice());
        }
        if let Some((def_id, _)) = found {
            return WildcardResult::Found(CoreResolution::Global {
                decl: def_id,
                namespace: ns,
            });
        }
        current = graph.scopes.get(sid).parent;
    }
    WildcardResult::NotFound
}

/// Build the per-file resolution index from pre-computed core results.
///
/// Zips `def.use_sites` with `core.resolutions`, builds the `HashMap`
/// and diagnostics. Import errors are also mapped to diagnostics.
///
/// `lookup_decl` maps a `GlobalDefId` (from `CoreResolution::Global`)
/// to a `SymbolId` in the target file.
pub fn build_resolve_index(
    def: &DefIndex,
    core: &CoreResolveOutput,
    lookup_decl: &dyn Fn(GlobalDefId) -> Option<SymbolId>,
) -> ResolveIndex {
    let mut resolutions = HashMap::new();
    let mut diagnostics = Vec::new();

    for (use_site, result) in def.use_sites.iter().zip(core.resolutions.iter()) {
        match result {
            CoreResolveResult::Resolved(CoreResolution::Local { symbol, namespace }) => {
                resolutions.insert(
                    use_site.ast_id,
                    Resolution {
                        symbol: GlobalSymbolId {
                            file: def.file,
                            local: *symbol,
                        },
                        namespace: *namespace,
                    },
                );
            }
            CoreResolveResult::Resolved(CoreResolution::Global { decl, namespace }) => {
                if let Some(local) = lookup_decl(*decl) {
                    resolutions.insert(
                        use_site.ast_id,
                        Resolution {
                            symbol: GlobalSymbolId {
                                file: decl.file(),
                                local,
                            },
                            namespace: *namespace,
                        },
                    );
                }
            }
            CoreResolveResult::Unresolved(reason) => {
                let diag = reason_to_diagnostic(reason, &use_site.path, use_site.range);
                diagnostics.push(diag);
            }
        }
    }

    // Map import errors to diagnostics
    for err in &core.import_errors {
        let imp = &def.imports[err.import_idx as usize];
        let path = match &imp.name {
            ImportName::Explicit(member) => NamePath::Qualified {
                segments: Box::new([imp.package.clone(), member.clone()]),
            },
            ImportName::Wildcard => NamePath::Simple(SmolStr::new(format!("{}::*", imp.package))),
        };
        let diag = reason_to_diagnostic(&err.reason, &path, imp.range);
        diagnostics.push(diag);
    }

    ResolveIndex {
        file: def.file,
        resolutions,
        diagnostics: diagnostics.into_boxed_slice(),
    }
}

fn reason_to_diagnostic(
    reason: &UnresolvedReason,
    path: &NamePath,
    range: TextRange,
) -> SemanticDiag {
    match reason {
        UnresolvedReason::NotFound => SemanticDiag {
            kind: SemanticDiagKind::UnresolvedName {
                name: SmolStr::new(path.display_name()),
            },
            range,
        },
        UnresolvedReason::PackageNotFound { package } => SemanticDiag {
            kind: SemanticDiagKind::PackageNotFound {
                package: package.clone(),
            },
            range,
        },
        UnresolvedReason::MemberNotFound { package, member } => SemanticDiag {
            kind: SemanticDiagKind::MemberNotFound {
                package: package.clone(),
                member: member.clone(),
            },
            range,
        },
        UnresolvedReason::AmbiguousWildcardImport { candidates } => SemanticDiag {
            kind: SemanticDiagKind::AmbiguousWildcardImport {
                name: SmolStr::new(path.display_name()),
                candidates: candidates.clone(),
            },
            range,
        },
        UnresolvedReason::UnsupportedQualifiedPath { .. } => SemanticDiag {
            kind: SemanticDiagKind::UnsupportedQualifiedPath {
                path: SmolStr::new(path.display_name()),
            },
            range,
        },
    }
}

struct DefContext<'a> {
    ast_id_map: &'a AstIdMap,
    symbols: SymbolTableBuilder,
    scopes: ScopeTreeBuilder,
    export_modules: Vec<SymbolId>,
    export_packages: Vec<SymbolId>,
    decl_to_symbol: HashMap<lyra_ast::ErasedAstId, SymbolId>,
    use_sites: Vec<UseSite>,
    imports: Vec<Import>,
}

impl<'a> DefContext<'a> {
    fn new(ast_id_map: &'a AstIdMap) -> Self {
        Self {
            ast_id_map,
            symbols: SymbolTableBuilder::new(),
            scopes: ScopeTreeBuilder::new(),
            export_modules: Vec::new(),
            export_packages: Vec::new(),
            decl_to_symbol: HashMap::new(),
            use_sites: Vec::new(),
            imports: Vec::new(),
        }
    }

    fn add_symbol(
        &mut self,
        name: SmolStr,
        kind: SymbolKind,
        def_range: TextRange,
        scope: ScopeId,
    ) -> SymbolId {
        let id = self.symbols.push(Symbol {
            name,
            kind,
            def_range,
            scope,
        });
        self.scopes.add_binding(scope, id, kind);
        id
    }
}

fn collect_source_file(ctx: &mut DefContext<'_>, root: &SyntaxNode, file_scope: ScopeId) {
    for child in root.children() {
        match child.kind() {
            SyntaxKind::ModuleDecl => {
                collect_module(ctx, &child, file_scope);
            }
            SyntaxKind::PackageDecl => {
                collect_package(ctx, &child, file_scope);
            }
            _ => {}
        }
    }
}

fn collect_module(ctx: &mut DefContext<'_>, node: &SyntaxNode, _file_scope: ScopeId) {
    // Module name -> exports (not lexical scope)
    let module_name = first_ident_token(node);
    if let Some(name_tok) = &module_name {
        let name = SmolStr::new(name_tok.text());
        let range = name_tok.text_range();
        // Module scope is the parent for all declarations inside the module
        let module_scope = ctx.scopes.push(ScopeKind::Module, None);
        let sym_id = ctx.symbols.push(Symbol {
            name,
            kind: SymbolKind::Module,
            def_range: range,
            scope: module_scope,
        });
        ctx.export_modules.push(sym_id);

        // Record decl_to_symbol for cross-file resolution
        if let Some(module_decl) = lyra_ast::ModuleDecl::cast(node.clone())
            && let Some(ast_id) = ctx.ast_id_map.ast_id(&module_decl)
        {
            ctx.decl_to_symbol.insert(ast_id.erase(), sym_id);
        }

        // Visit parameter port list
        for child in node.children() {
            match child.kind() {
                SyntaxKind::ParamPortList => {
                    collect_param_port_list(ctx, &child, module_scope);
                }
                SyntaxKind::PortList => {
                    collect_port_list(ctx, &child, module_scope);
                }
                SyntaxKind::ModuleBody => {
                    collect_module_body(ctx, &child, module_scope);
                }
                _ => {}
            }
        }
    }
}

fn collect_package(ctx: &mut DefContext<'_>, node: &SyntaxNode, _file_scope: ScopeId) {
    let package_name = first_ident_token(node);
    if let Some(name_tok) = &package_name {
        let name = SmolStr::new(name_tok.text());
        let range = name_tok.text_range();
        let package_scope = ctx.scopes.push(ScopeKind::Package, None);
        let sym_id = ctx.symbols.push(Symbol {
            name,
            kind: SymbolKind::Package,
            def_range: range,
            scope: package_scope,
        });
        ctx.export_packages.push(sym_id);

        // Record decl_to_symbol for cross-file resolution
        if let Some(pkg_decl) = lyra_ast::PackageDecl::cast(node.clone())
            && let Some(ast_id) = ctx.ast_id_map.ast_id(&pkg_decl)
        {
            ctx.decl_to_symbol.insert(ast_id.erase(), sym_id);
        }

        // Visit package body
        for child in node.children() {
            if child.kind() == SyntaxKind::PackageBody {
                collect_package_body(ctx, &child, package_scope);
            }
        }
    }
}

fn collect_package_body(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    for child in node.children() {
        collect_module_item(ctx, &child, scope);
    }
}

fn collect_param_port_list(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    for child in node.children() {
        if child.kind() == SyntaxKind::ParamDecl {
            collect_param_decl(ctx, &child, scope);
        }
    }
}

fn collect_port_list(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    for child in node.children() {
        if child.kind() == SyntaxKind::Port
            && let Some(name_tok) = first_ident_token(&child)
        {
            // The first Ident in a Port might be a type name if the port
            // has an explicit type. We need the *last* Ident that is a
            // direct child token (not inside a child node like TypeSpec).
            let name = port_name_ident(&child);
            if let Some(name_tok) = name {
                ctx.add_symbol(
                    SmolStr::new(name_tok.text()),
                    SymbolKind::Port,
                    name_tok.text_range(),
                    scope,
                );
            } else {
                // Fallback: use first ident
                ctx.add_symbol(
                    SmolStr::new(name_tok.text()),
                    SymbolKind::Port,
                    name_tok.text_range(),
                    scope,
                );
            }
        }
    }
}

fn collect_module_body(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    for child in node.children() {
        collect_module_item(ctx, &child, scope);
    }
}

fn collect_module_item(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    match node.kind() {
        SyntaxKind::NetDecl => {
            collect_declarators(ctx, node, SymbolKind::Net, scope);
        }
        SyntaxKind::VarDecl => {
            collect_declarators(ctx, node, SymbolKind::Variable, scope);
        }
        SyntaxKind::ParamDecl => {
            collect_param_decl(ctx, node, scope);
        }
        SyntaxKind::ContinuousAssign => {
            collect_name_refs(ctx, node, scope);
        }
        SyntaxKind::ImportDecl => {
            collect_import_decl(ctx, node, scope);
        }
        SyntaxKind::ModuleInstantiation => {
            // Record the module type name as a Definition-namespace use-site
            if let Some(inst) = ModuleInstantiation::cast(node.clone())
                && let Some(name_tok) = inst.module_name()
                && let Some(ast_id) = ctx.ast_id_map.ast_id(&inst)
            {
                ctx.use_sites.push(UseSite {
                    path: NamePath::Simple(SmolStr::new(name_tok.text())),
                    expected_ns: Namespace::Definition,
                    range: name_tok.text_range(),
                    scope,
                    ast_id: ast_id.erase(),
                });
            }
            // Still collect NameRefs in port connection expressions
            collect_name_refs(ctx, node, scope);
        }
        SyntaxKind::AlwaysBlock | SyntaxKind::InitialBlock => {
            collect_procedural_block(ctx, node, scope);
        }
        _ => {}
    }
}

fn collect_import_decl(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    for child in node.children() {
        if child.kind() == SyntaxKind::ImportItem {
            collect_import_item(ctx, &child, scope);
        }
    }
}

fn collect_import_item(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    // Determine if wildcard or explicit
    let has_star = node
        .children_with_tokens()
        .filter_map(lyra_parser::SyntaxElement::into_token)
        .any(|tok| tok.kind() == SyntaxKind::Star);

    if has_star {
        // Wildcard: first Ident is the package name
        if let Some(pkg_tok) = first_ident_token(node) {
            ctx.imports.push(Import {
                package: SmolStr::new(pkg_tok.text()),
                name: ImportName::Wildcard,
                scope,
                range: node.text_range(),
            });
        }
    } else if let Some(qn) = node
        .children()
        .find(|c| c.kind() == SyntaxKind::QualifiedName)
    {
        // Non-wildcard: QualifiedName child with [pkg, sym]
        let idents: Vec<_> = qn
            .children_with_tokens()
            .filter_map(lyra_parser::SyntaxElement::into_token)
            .filter(|tok| tok.kind() == SyntaxKind::Ident)
            .collect();
        if idents.len() >= 2 {
            ctx.imports.push(Import {
                package: SmolStr::new(idents[0].text()),
                name: ImportName::Explicit(SmolStr::new(idents[1].text())),
                scope,
                range: node.text_range(),
            });
        }
    }
}

fn collect_param_decl(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    for child in node.children() {
        if child.kind() == SyntaxKind::Declarator {
            if let Some(name_tok) = first_ident_token(&child) {
                let sym_id = ctx.add_symbol(
                    SmolStr::new(name_tok.text()),
                    SymbolKind::Parameter,
                    name_tok.text_range(),
                    scope,
                );
                // Record decl_to_symbol for package member resolution
                if let Some(decl) = lyra_ast::Declarator::cast(child.clone())
                    && let Some(ast_id) = ctx.ast_id_map.ast_id(&decl)
                {
                    ctx.decl_to_symbol.insert(ast_id.erase(), sym_id);
                }
            }
            // Collect name refs in default value expressions
            collect_name_refs(ctx, &child, scope);
        }
    }
}

fn collect_declarators(
    ctx: &mut DefContext<'_>,
    node: &SyntaxNode,
    kind: SymbolKind,
    scope: ScopeId,
) {
    for child in node.children() {
        if child.kind() == SyntaxKind::Declarator {
            if let Some(name_tok) = first_ident_token(&child) {
                let sym_id = ctx.add_symbol(
                    SmolStr::new(name_tok.text()),
                    kind,
                    name_tok.text_range(),
                    scope,
                );
                // Record decl_to_symbol for package member resolution
                if let Some(decl) = lyra_ast::Declarator::cast(child.clone())
                    && let Some(ast_id) = ctx.ast_id_map.ast_id(&decl)
                {
                    ctx.decl_to_symbol.insert(ast_id.erase(), sym_id);
                }
            }
            // Collect name refs in initializer expressions
            collect_name_refs(ctx, &child, scope);
        }
    }
}

fn collect_procedural_block(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    for child in node.children() {
        collect_statement(ctx, &child, scope);
    }
}

fn collect_statement(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    match node.kind() {
        SyntaxKind::BlockStmt => {
            let block_scope = ctx.scopes.push(ScopeKind::Block, Some(scope));
            for child in node.children() {
                match child.kind() {
                    SyntaxKind::VarDecl => {
                        collect_declarators(ctx, &child, SymbolKind::Variable, block_scope);
                    }
                    _ => {
                        collect_statement(ctx, &child, block_scope);
                    }
                }
            }
        }
        SyntaxKind::VarDecl => {
            collect_declarators(ctx, node, SymbolKind::Variable, scope);
        }
        SyntaxKind::IfStmt
        | SyntaxKind::CaseStmt
        | SyntaxKind::CaseItem
        | SyntaxKind::ForStmt
        | SyntaxKind::WhileStmt
        | SyntaxKind::RepeatStmt
        | SyntaxKind::ForeverStmt
        | SyntaxKind::TimingControl => {
            // Recurse into children
            for child in node.children() {
                collect_statement(ctx, &child, scope);
            }
            // Collect name refs from expression tokens at this level
            collect_direct_name_refs(ctx, node, scope);
        }
        SyntaxKind::AssignStmt | SyntaxKind::EventExpr | SyntaxKind::EventItem => {
            collect_name_refs(ctx, node, scope);
        }
        _ => {
            // For expression nodes and others, collect name refs
            if is_expression_kind(node.kind()) {
                collect_name_refs(ctx, node, scope);
            } else {
                // Recurse for other structural nodes
                for child in node.children() {
                    collect_statement(ctx, &child, scope);
                }
            }
        }
    }
}

fn collect_name_refs(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    for child in node.children() {
        if child.kind() == SyntaxKind::NameRef {
            if let Some(name_ref) = NameRef::cast(child.clone())
                && let Some(ident) = name_ref.ident()
                && let Some(ast_id) = ctx.ast_id_map.ast_id(&name_ref)
            {
                ctx.use_sites.push(UseSite {
                    path: NamePath::Simple(SmolStr::new(ident.text())),
                    expected_ns: Namespace::Value,
                    range: name_ref.text_range(),
                    scope,
                    ast_id: ast_id.erase(),
                });
            }
        } else if child.kind() == SyntaxKind::QualifiedName {
            if let Some(qn) = QualifiedName::cast(child.clone())
                && let Some(ast_id) = ctx.ast_id_map.ast_id(&qn)
            {
                let segments: Box<[SmolStr]> = qn
                    .segments()
                    .map(|ident| SmolStr::new(ident.text()))
                    .collect();
                if !segments.is_empty() {
                    ctx.use_sites.push(UseSite {
                        path: NamePath::Qualified { segments },
                        expected_ns: Namespace::Value,
                        range: qn.text_range(),
                        scope,
                        ast_id: ast_id.erase(),
                    });
                }
            }
        } else if child.kind() == SyntaxKind::Declarator {
            // Skip declarator names but collect refs in their initializers
            // (already handled by collect_declarators)
        } else {
            collect_name_refs(ctx, &child, scope);
        }
    }
}

fn collect_direct_name_refs(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    // Collect name refs from direct expression children only (not structural children)
    for child in node.children() {
        if is_expression_kind(child.kind())
            || child.kind() == SyntaxKind::NameRef
            || child.kind() == SyntaxKind::QualifiedName
        {
            collect_name_refs_from_expr(ctx, &child, scope);
        }
    }
}

fn collect_name_refs_from_expr(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    if node.kind() == SyntaxKind::NameRef {
        if let Some(name_ref) = NameRef::cast(node.clone())
            && let Some(ident) = name_ref.ident()
            && let Some(ast_id) = ctx.ast_id_map.ast_id(&name_ref)
        {
            ctx.use_sites.push(UseSite {
                path: NamePath::Simple(SmolStr::new(ident.text())),
                expected_ns: Namespace::Value,
                range: name_ref.text_range(),
                scope,
                ast_id: ast_id.erase(),
            });
        }
        return;
    }
    if node.kind() == SyntaxKind::QualifiedName {
        if let Some(qn) = QualifiedName::cast(node.clone())
            && let Some(ast_id) = ctx.ast_id_map.ast_id(&qn)
        {
            let segments: Box<[SmolStr]> = qn
                .segments()
                .map(|ident| SmolStr::new(ident.text()))
                .collect();
            if !segments.is_empty() {
                ctx.use_sites.push(UseSite {
                    path: NamePath::Qualified { segments },
                    expected_ns: Namespace::Value,
                    range: qn.text_range(),
                    scope,
                    ast_id: ast_id.erase(),
                });
            }
        }
        return;
    }
    for child in node.children() {
        collect_name_refs_from_expr(ctx, &child, scope);
    }
}

fn is_expression_kind(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::Expression
            | SyntaxKind::BinExpr
            | SyntaxKind::PrefixExpr
            | SyntaxKind::ParenExpr
            | SyntaxKind::CondExpr
            | SyntaxKind::ConcatExpr
            | SyntaxKind::ReplicExpr
            | SyntaxKind::IndexExpr
            | SyntaxKind::RangeExpr
            | SyntaxKind::FieldExpr
            | SyntaxKind::CallExpr
            | SyntaxKind::ArgList
            | SyntaxKind::NameRef
            | SyntaxKind::Literal
            | SyntaxKind::QualifiedName
    )
}

fn first_ident_token(node: &SyntaxNode) -> Option<lyra_parser::SyntaxToken> {
    node.children_with_tokens()
        .filter_map(lyra_parser::SyntaxElement::into_token)
        .find(|tok| tok.kind() == SyntaxKind::Ident)
}

fn port_name_ident(port_node: &SyntaxNode) -> Option<lyra_parser::SyntaxToken> {
    // In an ANSI port declaration, the port name is the Ident token
    // that is a direct child of the Port node (not inside TypeSpec).
    // If there's a TypeSpec, the port name follows it.
    let mut last_ident = None;
    for el in port_node.children_with_tokens() {
        match el {
            lyra_parser::SyntaxElement::Token(tok) if tok.kind() == SyntaxKind::Ident => {
                last_ident = Some(tok);
            }
            _ => {}
        }
    }
    last_ident
}

#[cfg(test)]
mod tests {
    use lyra_source::FileId;

    use super::*;
    use crate::name_graph::NameGraph;

    fn parse_source(src: &str) -> (lyra_parser::Parse, lyra_ast::AstIdMap) {
        let tokens = lyra_lexer::lex(src);
        let pp = lyra_preprocess::preprocess_identity(FileId(0), &tokens, src);
        let parse = lyra_parser::parse(&pp.tokens, &pp.expanded_text);
        let map = lyra_ast::AstIdMap::from_root(FileId(0), &parse.syntax());
        (parse, map)
    }

    #[test]
    fn use_site_ordering_stable_across_whitespace() {
        let src_a = "module m; logic x; assign x = 0; endmodule";
        let src_b = "module  m;  logic  x;  assign  x  =  0;  endmodule";

        let (parse_a, map_a) = parse_source(src_a);
        let def_a = build_def_index(FileId(0), &parse_a, &map_a);
        let graph_a = NameGraph::from_def_index(&def_a);

        let (parse_b, map_b) = parse_source(src_b);
        let def_b = build_def_index(FileId(0), &parse_b, &map_b);
        let graph_b = NameGraph::from_def_index(&def_b);

        // NameGraph should be equal (offset-independent)
        assert_eq!(
            graph_a, graph_b,
            "NameGraph should be equal across whitespace edits"
        );

        // Ranges and ast_ids differ, confirming DefIndex is NOT equal
        assert_ne!(def_a, def_b, "DefIndex should differ (offsets changed)");
    }

    #[test]
    fn package_symbols_collected() {
        let src = "package pkg; logic x; parameter P = 1; endpackage";
        let (parse, map) = parse_source(src);
        let def = build_def_index(FileId(0), &parse, &map);

        assert_eq!(def.exports.packages.len(), 1);
        let pkg_sym = def.symbols.get(def.exports.packages[0]);
        assert_eq!(pkg_sym.name.as_str(), "pkg");
        assert_eq!(pkg_sym.kind, SymbolKind::Package);

        // Check that internal symbols were collected
        let has_x = def
            .symbols
            .iter()
            .any(|(_, s)| s.name.as_str() == "x" && s.kind == SymbolKind::Variable);
        assert!(has_x, "variable 'x' should be collected in package");
    }

    #[test]
    fn import_recorded_in_def_index() {
        let src = "module m; import pkg::x; import pkg2::*; endmodule";
        let (parse, map) = parse_source(src);
        let def = build_def_index(FileId(0), &parse, &map);

        assert_eq!(def.imports.len(), 2);
        assert_eq!(def.imports[0].package.as_str(), "pkg");
        assert_eq!(def.imports[0].name, ImportName::Explicit(SmolStr::new("x")));
        assert_eq!(def.imports[1].package.as_str(), "pkg2");
        assert_eq!(def.imports[1].name, ImportName::Wildcard);

        // Imports should NOT appear as use-sites
        let import_use_sites: Vec<_> = def
            .use_sites
            .iter()
            .filter(|u| {
                u.path
                    .as_simple()
                    .is_some_and(|s| s == "pkg" || s == "pkg2")
            })
            .collect();
        assert!(
            import_use_sites.is_empty(),
            "imports should not be recorded as use-sites"
        );
    }

    #[test]
    fn qualified_name_use_site() {
        let src = "module m; assign y = pkg::x; endmodule";
        let (parse, map) = parse_source(src);
        let def = build_def_index(FileId(0), &parse, &map);

        let qualified_sites: Vec<_> = def
            .use_sites
            .iter()
            .filter(|u| u.path.as_qualified().is_some())
            .collect();
        assert_eq!(qualified_sites.len(), 1);
        let segs = qualified_sites[0].path.as_qualified().unwrap();
        assert_eq!(segs.len(), 2);
        assert_eq!(segs[0].as_str(), "pkg");
        assert_eq!(segs[1].as_str(), "x");
    }

    #[test]
    fn name_graph_includes_imports() {
        let src = "module m; import pkg::x; endmodule";
        let (parse, map) = parse_source(src);
        let def = build_def_index(FileId(0), &parse, &map);
        let graph = NameGraph::from_def_index(&def);

        assert_eq!(graph.imports.len(), 1);
        assert_eq!(graph.imports[0].package.as_str(), "pkg");
    }
}
