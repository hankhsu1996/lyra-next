use lyra_semantic::symbols::GlobalSymbolId;

use crate::pipeline::{ast_id_map, parse_file};
use crate::semantic::{def_index_file, global_def_index, resolve_index_file};
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
