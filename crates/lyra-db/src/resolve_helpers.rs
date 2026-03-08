use lyra_ast::HasSyntax;
use lyra_semantic::UserTypeRef;
use lyra_semantic::resolve_index::ImplicitNetId;
use lyra_semantic::types::{NetType, SymbolType, Ty};

use crate::semantic::resolve_index_file;
use crate::type_queries::{SymbolRef, type_of_symbol};
use crate::{CompilationUnit, SourceFile};

/// Internal error: `ImplicitNetId` not found in `ImplicitNetIndex`.
///
/// This is an invariant violation (resolve created the ID but the entity
/// is missing). Returns `SymbolType::Net` with `Ty::Error` to avoid
/// masking the bug as a user-facing diagnostic.
fn implicit_net_internal_error() -> SymbolType {
    SymbolType::Net(NetType {
        kind: lyra_semantic::types::NetKind::Wire,
        data: Ty::Error,
    })
}

/// Get the syntax node from a `UserTypeRef` for AST ID lookup.
pub(crate) fn utr_syntax(utr: &UserTypeRef) -> &lyra_parser::SyntaxNode {
    match utr {
        UserTypeRef::Simple(nr) | UserTypeRef::DottedType { base: nr, .. } => nr.syntax(),
        UserTypeRef::Qualified(qn) => qn.syntax(),
    }
}

/// Resolve a `UserTypeRef` as a type in the type namespace.
///
/// Shared between `DbInferCtx` and `DbTypeCheckCtx` to avoid duplication.
pub(crate) fn resolve_type_arg_impl(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    source_file: SourceFile,
    ast_id_map: &lyra_ast::AstIdMap,
    utr: &UserTypeRef,
) -> Option<Ty> {
    let name_node = utr_syntax(utr);
    let ast_id = ast_id_map.erased_ast_id(name_node)?;
    let resolve = resolve_index_file(db, source_file, unit);
    let resolution = resolve.resolutions.get(&ast_id)?;
    let sym_id = match &resolution.target {
        lyra_semantic::resolve_index::ResolvedTarget::Symbol(s) => *s,
        lyra_semantic::resolve_index::ResolvedTarget::Def(def_id) => {
            return Some(crate::ty_resolve::def_target_ty(db, unit, *def_id));
        }
        lyra_semantic::resolve_index::ResolvedTarget::EnumVariant(target) => {
            return Some(Ty::Enum(target.enum_id));
        }
        lyra_semantic::resolve_index::ResolvedTarget::ImplicitNet(_) => {
            return None;
        }
    };
    let sym_ref = SymbolRef::new(db, unit, sym_id);
    let sym_type = type_of_symbol(db, sym_ref);
    match &sym_type {
        SymbolType::TypeAlias(ty) => Some(ty.clone()),
        _ => None,
    }
}

/// Get the `SymbolType` of an implicit net: `SymbolType::Net` with the
/// net's kind and scalar logic data type.
///
/// A missing `ImplicitNetId` is an internal invariant violation (the
/// resolve layer created the ID). This must not be masked as a
/// user-facing `UserTypeUnresolved` error.
pub(crate) fn implicit_net_symbol_type(
    resolve: &lyra_semantic::resolve_index::ResolveIndex,
    id: ImplicitNetId,
) -> SymbolType {
    match resolve.implicit_nets.get(id) {
        Some(net) => SymbolType::Net(NetType {
            kind: net.net_kind,
            data: Ty::implicit_net_data_ty(),
        }),
        None => implicit_net_internal_error(),
    }
}
