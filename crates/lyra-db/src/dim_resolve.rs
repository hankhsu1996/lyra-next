use lyra_ast::UnpackedDimSource;
use lyra_semantic::resolve_index::ResolveIndex;
use lyra_semantic::types::{Ty, UnpackedDim};

use crate::CompilationUnit;
use crate::type_queries::{SymbolRef, type_of_symbol};

/// Pre-established context for resolving unpacked dimensions within one file.
///
/// Construct once per file/unit, then reuse across all dimension lowering
/// calls in the same query path.
pub(crate) struct DimResolveCtx<'a> {
    db: &'a dyn salsa::Database,
    unit: CompilationUnit,
    resolve: &'a ResolveIndex,
    ast_id_map: &'a lyra_ast::AstIdMap,
}

impl<'a> DimResolveCtx<'a> {
    pub(crate) fn new(
        db: &'a dyn salsa::Database,
        unit: CompilationUnit,
        resolve: &'a ResolveIndex,
        ast_id_map: &'a lyra_ast::AstIdMap,
    ) -> Self {
        Self {
            db,
            unit,
            resolve,
            ast_id_map,
        }
    }

    /// Resolve a single unpacked dimension, checking name resolution for bare `NameRef`s.
    ///
    /// If the dim is `Size { expr }` with a bare `NameRef` that resolves to a type-namespace
    /// target (typedef/type param), returns `Assoc(Typed(resolved_ty))`. Otherwise falls
    /// through to standard `extract_unpacked_dim`.
    pub(crate) fn resolve_unpacked_dim(&self, dim: &lyra_ast::UnpackedDimension) -> UnpackedDim {
        use lyra_ast::UnpackedDimKind;
        use lyra_semantic::types::AssocIndex;

        if let UnpackedDimKind::Size { ref expr } = dim.classify()
            && let Some(utr) = lyra_semantic::user_type_ref_from_expr(expr)
        {
            let resolve_node = crate::resolve_helpers::utr_syntax(&utr);
            if let Some(name_site_id) = self.ast_id_map.erased_ast_id(resolve_node)
                && let Some(resolution) = self.resolve.resolutions.get(&name_site_id)
                && resolution.namespace == lyra_semantic::symbols::Namespace::Type
            {
                let resolved_ty = match &resolution.target {
                    lyra_semantic::resolve_index::ResolvedTarget::Symbol(sym_id) => {
                        let sym_ref = SymbolRef::new(self.db, self.unit, *sym_id);
                        let sym_type = type_of_symbol(self.db, sym_ref);
                        match sym_type {
                            lyra_semantic::types::SymbolType::TypeAlias(ty) => ty,
                            _ => Ty::Error,
                        }
                    }
                    lyra_semantic::resolve_index::ResolvedTarget::Def(def_id) => {
                        crate::ty_resolve::def_target_ty(self.db, self.unit, *def_id)
                    }
                    lyra_semantic::resolve_index::ResolvedTarget::EnumVariant(target) => {
                        Ty::Enum(target.enum_id)
                    }
                    lyra_semantic::resolve_index::ResolvedTarget::ImplicitNet(_) => Ty::Error,
                };
                return UnpackedDim::Assoc(AssocIndex::Typed(Box::new(resolved_ty)));
            }
        }
        lyra_semantic::extract_unpacked_dim(dim, self.ast_id_map)
    }

    /// Wrap a base type with resolved unpacked dims from a dim source.
    pub(crate) fn wrap_unpacked(&self, ty: Ty, owner: Option<&UnpackedDimSource>) -> Ty {
        let Some(owner) = owner else { return ty };
        let dims: Vec<UnpackedDim> = owner
            .unpacked_dimensions()
            .map(|dim| self.resolve_unpacked_dim(&dim))
            .collect();
        if dims.is_empty() {
            return ty;
        }
        lyra_semantic::wrap_unpacked(ty, &dims)
    }
}
