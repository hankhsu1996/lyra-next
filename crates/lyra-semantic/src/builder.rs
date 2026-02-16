use std::collections::HashMap;

use lyra_ast::{
    AstIdMap, AstNode, ConfigDecl, InterfaceDecl, ModuleInstantiation, NameRef, PrimitiveDecl,
    ProgramDecl, QualifiedName, TypedefDecl,
};
use lyra_lexer::SyntaxKind;
use lyra_parser::{Parse, SyntaxNode};
use lyra_source::{FileId, TextRange};
use smol_str::SmolStr;

use crate::def_index::{DefIndex, ExpectedNs, Exports, Import, ImportName, NamePath, UseSite};
use crate::diagnostic::{SemanticDiag, SemanticDiagKind};
use crate::scopes::{ScopeId, ScopeKind, ScopeTreeBuilder};
use crate::symbols::{Namespace, Symbol, SymbolId, SymbolKind, SymbolTableBuilder};

pub fn build_def_index(file: FileId, parse: &Parse, ast_id_map: &AstIdMap) -> DefIndex {
    let mut ctx = DefContext::new(ast_id_map);
    let root = parse.syntax();

    // Root file scope
    let file_scope = ctx.scopes.push(ScopeKind::File, None);
    collect_source_file(&mut ctx, &root, file_scope);

    // Freeze symbols first, then scopes (which need symbol names for sorting)
    let symbols = ctx.symbols.freeze();
    let scopes = ctx.scopes.freeze(&symbols);

    // Sort export definition/package ids by symbol name
    ctx.export_definitions
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
            definitions: ctx.export_definitions.into_boxed_slice(),
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

struct DefContext<'a> {
    ast_id_map: &'a AstIdMap,
    symbols: SymbolTableBuilder,
    scopes: ScopeTreeBuilder,
    export_definitions: Vec<SymbolId>,
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
            export_definitions: Vec::new(),
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
            SyntaxKind::InterfaceDecl => {
                collect_interface(ctx, &child);
            }
            SyntaxKind::ProgramDecl => {
                collect_program(ctx, &child);
            }
            SyntaxKind::PrimitiveDecl => {
                collect_primitive(ctx, &child);
            }
            SyntaxKind::ConfigDecl => {
                collect_config(ctx, &child);
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
        ctx.export_definitions.push(sym_id);

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

fn collect_interface(ctx: &mut DefContext<'_>, node: &SyntaxNode) {
    let name_tok = first_ident_token(node);
    if let Some(name_tok) = &name_tok {
        let name = SmolStr::new(name_tok.text());
        let range = name_tok.text_range();
        let iface_scope = ctx.scopes.push(ScopeKind::Interface, None);
        let sym_id = ctx.symbols.push(Symbol {
            name,
            kind: SymbolKind::Interface,
            def_range: range,
            scope: iface_scope,
        });
        ctx.export_definitions.push(sym_id);

        if let Some(iface_decl) = InterfaceDecl::cast(node.clone())
            && let Some(ast_id) = ctx.ast_id_map.ast_id(&iface_decl)
        {
            ctx.decl_to_symbol.insert(ast_id.erase(), sym_id);
        }

        for child in node.children() {
            match child.kind() {
                SyntaxKind::ParamPortList => {
                    collect_param_port_list(ctx, &child, iface_scope);
                }
                SyntaxKind::PortList => {
                    collect_port_list(ctx, &child, iface_scope);
                }
                SyntaxKind::InterfaceBody => {
                    collect_module_body(ctx, &child, iface_scope);
                }
                _ => {}
            }
        }
    }
}

fn collect_program(ctx: &mut DefContext<'_>, node: &SyntaxNode) {
    let name_tok = first_ident_token(node);
    if let Some(name_tok) = &name_tok {
        let name = SmolStr::new(name_tok.text());
        let range = name_tok.text_range();
        let prog_scope = ctx.scopes.push(ScopeKind::Program, None);
        let sym_id = ctx.symbols.push(Symbol {
            name,
            kind: SymbolKind::Program,
            def_range: range,
            scope: prog_scope,
        });
        ctx.export_definitions.push(sym_id);

        if let Some(prog_decl) = ProgramDecl::cast(node.clone())
            && let Some(ast_id) = ctx.ast_id_map.ast_id(&prog_decl)
        {
            ctx.decl_to_symbol.insert(ast_id.erase(), sym_id);
        }

        for child in node.children() {
            match child.kind() {
                SyntaxKind::ParamPortList => {
                    collect_param_port_list(ctx, &child, prog_scope);
                }
                SyntaxKind::PortList => {
                    collect_port_list(ctx, &child, prog_scope);
                }
                SyntaxKind::ProgramBody => {
                    collect_module_body(ctx, &child, prog_scope);
                }
                _ => {}
            }
        }
    }
}

fn collect_primitive(ctx: &mut DefContext<'_>, node: &SyntaxNode) {
    let name_tok = first_ident_token(node);
    if let Some(name_tok) = &name_tok {
        let name = SmolStr::new(name_tok.text());
        let range = name_tok.text_range();
        let prim_scope = ctx.scopes.push(ScopeKind::Module, None);
        let sym_id = ctx.symbols.push(Symbol {
            name,
            kind: SymbolKind::Primitive,
            def_range: range,
            scope: prim_scope,
        });
        ctx.export_definitions.push(sym_id);

        if let Some(prim_decl) = PrimitiveDecl::cast(node.clone())
            && let Some(ast_id) = ctx.ast_id_map.ast_id(&prim_decl)
        {
            ctx.decl_to_symbol.insert(ast_id.erase(), sym_id);
        }
    }
}

fn collect_config(ctx: &mut DefContext<'_>, node: &SyntaxNode) {
    let name_tok = first_ident_token(node);
    if let Some(name_tok) = &name_tok {
        let name = SmolStr::new(name_tok.text());
        let range = name_tok.text_range();
        // Config bodies are static rules with no declarations, so use a
        // Module scope as a placeholder (no internal body collection).
        let cfg_scope = ctx.scopes.push(ScopeKind::Module, None);
        let sym_id = ctx.symbols.push(Symbol {
            name,
            kind: SymbolKind::Config,
            def_range: range,
            scope: cfg_scope,
        });
        ctx.export_definitions.push(sym_id);

        if let Some(cfg_decl) = ConfigDecl::cast(node.clone())
            && let Some(ast_id) = ctx.ast_id_map.ast_id(&cfg_decl)
        {
            ctx.decl_to_symbol.insert(ast_id.erase(), sym_id);
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
                    expected_ns: ExpectedNs::Exact(Namespace::Definition),
                    range: name_tok.text_range(),
                    scope,
                    ast_id: ast_id.erase(),
                });
            }
            // Still collect NameRefs in port connection expressions
            collect_name_refs(ctx, node, scope);
        }
        SyntaxKind::TypedefDecl => {
            collect_typedef(ctx, node, scope);
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
        if child.kind() == SyntaxKind::TypeSpec {
            collect_type_spec_refs(ctx, &child, scope);
        } else if child.kind() == SyntaxKind::Declarator {
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
        if child.kind() == SyntaxKind::TypeSpec {
            collect_type_spec_refs(ctx, &child, scope);
        } else if child.kind() == SyntaxKind::Declarator {
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

fn collect_type_spec_refs(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    for child in node.children() {
        match child.kind() {
            SyntaxKind::NameRef => {
                if let Some(name_ref) = NameRef::cast(child.clone())
                    && let Some(ident) = name_ref.ident()
                    && let Some(ast_id) = ctx.ast_id_map.ast_id(&name_ref)
                {
                    ctx.use_sites.push(UseSite {
                        path: NamePath::Simple(SmolStr::new(ident.text())),
                        expected_ns: ExpectedNs::TypeThenValue,
                        range: name_ref.text_range(),
                        scope,
                        ast_id: ast_id.erase(),
                    });
                }
            }
            SyntaxKind::QualifiedName => {
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
                            expected_ns: ExpectedNs::TypeThenValue,
                            range: qn.text_range(),
                            scope,
                            ast_id: ast_id.erase(),
                        });
                    }
                }
            }
            SyntaxKind::PackedDimension | SyntaxKind::UnpackedDimension => {
                collect_name_refs(ctx, &child, scope);
            }
            _ => {}
        }
    }
}

fn collect_typedef(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    for child in node.children() {
        if child.kind() == SyntaxKind::TypeSpec {
            collect_type_spec_refs(ctx, &child, scope);
        }
    }
    if let Some(td) = TypedefDecl::cast(node.clone())
        && let Some(name_tok) = td.name()
    {
        let sym_id = ctx.add_symbol(
            SmolStr::new(name_tok.text()),
            SymbolKind::Typedef,
            name_tok.text_range(),
            scope,
        );
        if let Some(ast_id) = ctx.ast_id_map.ast_id(&td) {
            ctx.decl_to_symbol.insert(ast_id.erase(), sym_id);
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
                    expected_ns: ExpectedNs::Exact(Namespace::Value),
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
                        expected_ns: ExpectedNs::Exact(Namespace::Value),
                        range: qn.text_range(),
                        scope,
                        ast_id: ast_id.erase(),
                    });
                }
            }
        } else if child.kind() == SyntaxKind::TypeSpec {
            // Handled by collect_type_spec_refs with TypeThenValue.
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
                expected_ns: ExpectedNs::Exact(Namespace::Value),
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
                    expected_ns: ExpectedNs::Exact(Namespace::Value),
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

    #[test]
    fn typedef_collected_as_type_ns() {
        let src = "module m; typedef logic [7:0] byte_t; endmodule";
        let (parse, map) = parse_source(src);
        let def = build_def_index(FileId(0), &parse, &map);

        let typedef_sym = def
            .symbols
            .iter()
            .find(|(_, s)| s.name.as_str() == "byte_t")
            .map(|(_, s)| s);
        assert!(
            typedef_sym.is_some(),
            "typedef 'byte_t' should be collected"
        );
        assert_eq!(typedef_sym.expect("checked").kind, SymbolKind::Typedef);
    }

    #[test]
    fn typedef_use_site_type_then_value() {
        let src = "module m; typedef my_t other_t; endmodule";
        let (parse, map) = parse_source(src);
        let def = build_def_index(FileId(0), &parse, &map);

        let my_t_use = def
            .use_sites
            .iter()
            .find(|u| u.path.as_simple() == Some("my_t"));
        assert!(my_t_use.is_some(), "'my_t' should be a use-site");
        assert_eq!(
            my_t_use.expect("checked").expected_ns,
            ExpectedNs::TypeThenValue,
            "type-position NameRef should have TypeThenValue"
        );

        let other_sym = def
            .symbols
            .iter()
            .find(|(_, s)| s.name.as_str() == "other_t");
        assert!(other_sym.is_some(), "'other_t' should be a typedef symbol");
    }

    #[test]
    fn same_name_different_namespace_no_duplicate() {
        let src = "module m; logic x; typedef int x; endmodule";
        let (parse, map) = parse_source(src);
        let def = build_def_index(FileId(0), &parse, &map);

        assert!(
            def.diagnostics.is_empty(),
            "value 'x' and type 'x' should not conflict: {:?}",
            def.diagnostics
        );
    }

    #[test]
    fn same_namespace_typedef_duplicate() {
        let src = "module m; typedef int t; typedef logic t; endmodule";
        let (parse, map) = parse_source(src);
        let def = build_def_index(FileId(0), &parse, &map);

        let dup_diags: Vec<_> = def
            .diagnostics
            .iter()
            .filter(|d| matches!(d.kind, SemanticDiagKind::DuplicateDefinition { .. }))
            .collect();
        assert!(
            !dup_diags.is_empty(),
            "two typedefs with same name should produce duplicate diagnostic"
        );
    }
}
