use lyra_semantic::types::{SymbolType, Ty};

use crate::semantic::resolve_index_file;
use crate::type_queries::{SymbolRef, type_of_symbol};
use crate::{CompilationUnit, SourceFile};

/// Resolve a NameRef/QualifiedName node as a type in the type namespace.
///
/// Shared between `DbInferCtx` and `DbTypeCheckCtx` to avoid duplication.
pub(crate) fn resolve_type_arg_impl(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    source_file: SourceFile,
    ast_id_map: &lyra_ast::AstIdMap,
    name_node: &lyra_parser::SyntaxNode,
) -> Option<Ty> {
    let ast_id = ast_id_map.erased_ast_id(name_node)?;
    let resolve = resolve_index_file(db, source_file, unit);
    let resolution = resolve.resolutions.get(&ast_id)?;
    let sym_ref = SymbolRef::new(db, unit, resolution.symbol);
    let sym_type = type_of_symbol(db, sym_ref);
    match &sym_type {
        SymbolType::TypeAlias(ty) => Some(ty.clone()),
        _ => None,
    }
}
