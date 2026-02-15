use std::collections::HashMap;

use lyra_ast::{AstIdMap, AstNode, NameRef};
use lyra_lexer::SyntaxKind;
use lyra_parser::{Parse, SyntaxNode};
use lyra_source::{FileId, TextRange};
use smol_str::SmolStr;

use crate::def_index::{DefIndex, Exports, NamePath, UseSite};
use crate::diagnostic::{SemanticDiag, SemanticDiagKind};
use crate::resolve_index::{Resolution, ResolveIndex};
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

    // Sort export module ids by symbol name
    ctx.export_modules
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
        },
        use_sites: ctx.use_sites.into_boxed_slice(),
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

pub fn build_resolve_index(def: &DefIndex) -> ResolveIndex {
    let mut resolutions = HashMap::new();
    let mut diagnostics = Vec::new();

    for use_site in &def.use_sites {
        match &use_site.path {
            NamePath::Simple(name) => {
                if let Some(sym_id) =
                    def.scopes
                        .resolve(&def.symbols, use_site.scope, use_site.expected_ns, name)
                {
                    resolutions.insert(
                        use_site.ast_id,
                        Resolution {
                            symbol: sym_id,
                            namespace: def.symbols.get(sym_id).kind.namespace(),
                        },
                    );
                } else {
                    diagnostics.push(SemanticDiag {
                        kind: SemanticDiagKind::UnresolvedName { name: name.clone() },
                        range: use_site.range,
                    });
                }
            }
        }
    }

    ResolveIndex {
        file: def.file,
        resolutions,
        diagnostics: diagnostics.into_boxed_slice(),
    }
}

struct DefContext<'a> {
    ast_id_map: &'a AstIdMap,
    symbols: SymbolTableBuilder,
    scopes: ScopeTreeBuilder,
    export_modules: Vec<SymbolId>,
    use_sites: Vec<UseSite>,
}

impl<'a> DefContext<'a> {
    fn new(ast_id_map: &'a AstIdMap) -> Self {
        Self {
            ast_id_map,
            symbols: SymbolTableBuilder::new(),
            scopes: ScopeTreeBuilder::new(),
            export_modules: Vec::new(),
            use_sites: Vec::new(),
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
        if child.kind() == SyntaxKind::ModuleDecl {
            collect_module(ctx, &child, file_scope);
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
            // Ports use typed accessors: the port name is the bare Ident
            // token that is NOT inside a TypeSpec child.
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
        SyntaxKind::ContinuousAssign | SyntaxKind::ModuleInstantiation => {
            collect_name_refs(ctx, node, scope);
        }
        SyntaxKind::AlwaysBlock | SyntaxKind::InitialBlock => {
            collect_procedural_block(ctx, node, scope);
        }
        _ => {}
    }
}

fn collect_param_decl(ctx: &mut DefContext<'_>, node: &SyntaxNode, scope: ScopeId) {
    for child in node.children() {
        if child.kind() == SyntaxKind::Declarator {
            if let Some(name_tok) = first_ident_token(&child) {
                ctx.add_symbol(
                    SmolStr::new(name_tok.text()),
                    SymbolKind::Parameter,
                    name_tok.text_range(),
                    scope,
                );
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
                ctx.add_symbol(
                    SmolStr::new(name_tok.text()),
                    kind,
                    name_tok.text_range(),
                    scope,
                );
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
        if is_expression_kind(child.kind()) || child.kind() == SyntaxKind::NameRef {
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
