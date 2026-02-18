use lyra_semantic::symbols::GlobalSymbolId;
use lyra_semantic::type_infer::ExprType;
use lyra_semantic::types::{SymbolType, Ty};
use smol_str::SmolStr;

use crate::expr_queries::ExprRef;
use crate::pipeline::{ast_id_map, parse_file};
use crate::semantic::{def_index_file, global_def_index, resolve_index_file};
use crate::type_queries::{SymbolRef, type_of_symbol};
use crate::{CompilationUnit, SourceFile, source_file_by_id};

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

/// Result of a `type_at` query: either a symbol's declared type or
/// an expression's inferred type.
pub enum TypeAtResult {
    Expr(ExprType),
    Symbol(SymbolType),
    SymbolEnriched { st: SymbolType, pretty: SmolStr },
}

impl TypeAtResult {
    pub fn pretty(&self) -> SmolStr {
        match self {
            TypeAtResult::Expr(et) => et.pretty(),
            TypeAtResult::Symbol(st) => st.pretty(),
            TypeAtResult::SymbolEnriched { pretty, .. } => pretty.clone(),
        }
    }
}

fn pretty_symbol_type_enriched(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    st: &SymbolType,
) -> SmolStr {
    match st {
        SymbolType::Value(ty) => pretty_ty_enriched(db, unit, ty),
        SymbolType::TypeAlias(ty) => {
            SmolStr::new(format!("type = {}", pretty_ty_enriched(db, unit, ty)))
        }
        SymbolType::Net(net) => SmolStr::new(format!(
            "{} {}",
            net.kind.keyword_str(),
            pretty_ty_enriched(db, unit, &net.data)
        )),
        SymbolType::Error(_) => SmolStr::new_static("<error>"),
    }
}

// Name-enriched pretty-printing for types with aggregate IDs.
// Looks up the defining file from the aggregate ID, not the query source file.
fn pretty_ty_enriched(db: &dyn salsa::Database, unit: CompilationUnit, ty: &Ty) -> SmolStr {
    match ty {
        Ty::Enum(id) => {
            let Some(src) = source_file_by_id(db, unit, id.file) else {
                return SmolStr::new_static("enum");
            };
            let def = def_index_file(db, src);
            let name = def.enum_defs.iter().find_map(|d| {
                if d.owner == id.owner && d.ordinal == id.ordinal {
                    d.name.clone()
                } else {
                    None
                }
            });
            match name {
                Some(n) => SmolStr::new(format!("enum {n}")),
                None => SmolStr::new_static("enum"),
            }
        }
        Ty::Struct(id) => {
            let Some(src) = source_file_by_id(db, unit, id.file) else {
                return SmolStr::new_static("struct");
            };
            let def = def_index_file(db, src);
            let info = def
                .struct_defs
                .iter()
                .find(|d| d.owner == id.owner && d.ordinal == id.ordinal);
            match info {
                Some(d) => {
                    let mut s = String::new();
                    if d.is_union {
                        s.push_str("union");
                    } else {
                        s.push_str("struct");
                    }
                    if d.packed {
                        s.push_str(" packed");
                    }
                    if let Some(ref n) = d.name {
                        s.push(' ');
                        s.push_str(n);
                    }
                    SmolStr::new(s)
                }
                None => SmolStr::new_static("struct"),
            }
        }
        Ty::Array { elem, dim } => {
            let base = pretty_ty_enriched(db, unit, elem);
            let dim_str = format_unpacked_dim(dim);
            SmolStr::new(format!("{base} {dim_str}"))
        }
        other => other.pretty(),
    }
}

fn format_unpacked_dim(dim: &lyra_semantic::types::UnpackedDim) -> String {
    use lyra_semantic::types::UnpackedDim;
    match dim {
        UnpackedDim::Range { msb, lsb } => {
            format!("[{}:{}]", fmt_const(msb), fmt_const(lsb))
        }
        UnpackedDim::Size(c) => {
            format!("[{}]", fmt_const(c))
        }
    }
}

fn fmt_const(c: &lyra_semantic::types::ConstInt) -> String {
    use lyra_semantic::types::ConstInt;
    match c {
        ConstInt::Known(v) => v.to_string(),
        ConstInt::Unevaluated(_) => "?".to_string(),
        ConstInt::Error(_) => "!".to_string(),
    }
}

/// Return type information at a source position.
///
/// `offset` is in expanded-text space (post-preprocess), the same coordinate
/// system used by `resolve_at` and `parse_file`. Tool layers that receive
/// original-source positions must map through the source map first.
///
/// Not a Salsa query -- convenience function over cached queries.
/// Priority: symbol > smallest enclosing expression > None.
pub fn type_at(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
    offset: lyra_source::TextSize,
) -> Option<TypeAtResult> {
    let parse = parse_file(db, file);
    let ast_map = ast_id_map(db, file);

    // Priority 1: Symbol (NameRef / QualifiedName that resolves)
    if let Some(gsym) = resolve_at(db, file, unit, offset) {
        let sym_ref = SymbolRef::new(db, unit, gsym);
        let st = type_of_symbol(db, sym_ref);
        let pretty = pretty_symbol_type_enriched(db, unit, &st);
        return Some(TypeAtResult::SymbolEnriched { st, pretty });
    }

    // Priority 2: Smallest enclosing "real" expression
    let token = parse.syntax().token_at_offset(offset).right_biased()?;
    for ancestor in token.parent_ancestors() {
        if is_real_expression_kind(ancestor.kind()) {
            let expr_ast_id = ast_map.erased_ast_id(&ancestor)?;
            let expr_ref = ExprRef::new(db, unit, expr_ast_id);
            let et = crate::expr_queries::type_of_expr(db, expr_ref);
            return Some(TypeAtResult::Expr(et));
        }
    }

    None
}

/// Expression kinds that are "real" (not transparent wrappers).
fn is_real_expression_kind(kind: lyra_lexer::SyntaxKind) -> bool {
    use lyra_lexer::SyntaxKind;
    matches!(
        kind,
        SyntaxKind::BinExpr
            | SyntaxKind::PrefixExpr
            | SyntaxKind::CondExpr
            | SyntaxKind::ConcatExpr
            | SyntaxKind::ReplicExpr
            | SyntaxKind::IndexExpr
            | SyntaxKind::CallExpr
            | SyntaxKind::Literal
            | SyntaxKind::NameRef
            | SyntaxKind::QualifiedName
    )
}
