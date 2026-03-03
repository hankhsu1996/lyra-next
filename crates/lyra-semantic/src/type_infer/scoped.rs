use lyra_ast::{AstIdMap, Expr};
use smol_str::SmolStr;

use crate::member::{MemberInfo, MemberLookupError};
use crate::member_name::MemberNameToken;
use crate::symbols::GlobalSymbolId;
use crate::type_extract::UserTypeRef;
use crate::types::{ConstInt, Ty};

use super::expr_type::{BitVecType, CallableSigRef, ExprType, InferCtx, ResolveCallableError};

/// Allocation-free decorator that injects local name bindings over an
/// inner `InferCtx`.
///
/// Used by `with (expr)` clauses (LRM 7.12) to bind the implicit
/// iterator variable `item` while type-checking the with-expression.
pub(crate) struct ScopedInferCtx<'a, const N: usize> {
    pub inner: &'a dyn InferCtx,
    pub bindings: [(SmolStr, ExprType); N],
}

impl<const N: usize> InferCtx for ScopedInferCtx<'_, N> {
    fn file_id(&self) -> lyra_source::FileId {
        self.inner.file_id()
    }

    fn ast_id_map(&self) -> &AstIdMap {
        self.inner.ast_id_map()
    }

    fn type_of_name(&self, name_expr: &Expr) -> ExprType {
        if let Some(lyra_ast::ExprKind::NameRef(nr)) = name_expr.classify()
            && let Some(ident_tok) = nr.ident()
        {
            let text = ident_tok.text();
            for (key, val) in &self.bindings {
                if key.as_str() == text {
                    return val.clone();
                }
            }
        }
        self.inner.type_of_name(name_expr)
    }

    fn const_eval(&self, expr: &Expr) -> ConstInt {
        self.inner.const_eval(expr)
    }

    fn resolve_callable(&self, callee: &Expr) -> Result<GlobalSymbolId, ResolveCallableError> {
        self.inner.resolve_callable(callee)
    }

    fn callable_sig(&self, sym: GlobalSymbolId) -> Option<CallableSigRef> {
        self.inner.callable_sig(sym)
    }

    fn member_lookup(
        &self,
        ty: &Ty,
        member: &MemberNameToken,
    ) -> Result<MemberInfo, MemberLookupError> {
        self.inner.member_lookup(ty, member)
    }

    fn enum_integral_view(&self, id: &crate::enum_def::EnumId) -> Option<BitVecType> {
        self.inner.enum_integral_view(id)
    }

    fn resolve_type_arg(&self, utr: &UserTypeRef) -> Option<Ty> {
        self.inner.resolve_type_arg(utr)
    }
}
