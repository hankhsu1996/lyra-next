use lyra_ast::{AstIdMap, Expr, HasSyntax};
use lyra_semantic::coerce::IntegralCtx;
use lyra_semantic::member::{
    ArrayMethodKind, BuiltinMethodKind, EnumMethodKind, MemberInfo, MemberKind, MemberLookupError,
    ReceiverInfo, StringMethodKind, classify_array_receiver,
};
use lyra_semantic::symbols::{GlobalSymbolId, SymbolKind};
use lyra_semantic::type_infer::{
    CallableKind, CallablePort, CallableSigRef, ExprType, ExprTypeErrorKind, InferCtx,
    ResolveCallableError, Signedness,
};
use lyra_semantic::types::{ConstInt, Ty};

use crate::callable_queries::{CallableRef, callable_signature, callable_signature_raw};
use crate::const_eval::{ConstExprRef, eval_const_int};
use crate::enum_queries::{EnumRef, enum_sem};
use crate::module_sig::CallableKind as DbCallableKind;
use crate::pipeline::{ast_id_map, parse_file};
use crate::record_queries::{ModportRef, RecordRef, modport_sem, record_fields_raw, record_sem};
use crate::semantic::{base_resolve_index, def_index_file, resolve_index_file};
use crate::type_queries::{SymbolRef, type_of_symbol, type_of_symbol_raw};
use crate::{CompilationUnit, source_file_by_id};

/// Identifies an expression for type inference.
///
/// Includes the compilation unit because name resolution is unit-scoped.
#[salsa::interned]
pub struct ExprRef<'db> {
    pub unit: CompilationUnit,
    pub expr_ast_id: lyra_ast::ErasedAstId,
}

/// Infer the type of an expression (Salsa-tracked).
#[salsa::tracked]
pub fn type_of_expr<'db>(db: &'db dyn salsa::Database, expr_ref: ExprRef<'db>) -> ExprType {
    let unit = expr_ref.unit(db);
    let expr_ast_id = expr_ref.expr_ast_id(db);
    let file_id = expr_ast_id.file();

    let Some(source_file) = source_file_by_id(db, unit, file_id) else {
        return ExprType::error(ExprTypeErrorKind::Unresolved);
    };

    let parse = parse_file(db, source_file);
    let map = ast_id_map(db, source_file);

    let Some(node) = map.get_node(&parse.syntax(), expr_ast_id) else {
        return ExprType::error(ExprTypeErrorKind::Unresolved);
    };

    let ctx = DbInferCtx {
        db,
        unit,
        source_file,
        ast_id_map: map,
    };

    let Some(expr) = Expr::cast(node) else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    lyra_semantic::type_infer::infer_expr(&expr, &ctx, None)
}

/// Infer the type of an expression in statement context (Salsa-tracked).
///
/// Unlike `type_of_expr`, void methods are legal here.
#[salsa::tracked]
pub fn type_of_expr_stmt<'db>(db: &'db dyn salsa::Database, expr_ref: ExprRef<'db>) -> ExprType {
    let unit = expr_ref.unit(db);
    let expr_ast_id = expr_ref.expr_ast_id(db);
    let file_id = expr_ast_id.file();

    let Some(source_file) = source_file_by_id(db, unit, file_id) else {
        return ExprType::error(ExprTypeErrorKind::Unresolved);
    };

    let parse = parse_file(db, source_file);
    let map = ast_id_map(db, source_file);

    let Some(node) = map.get_node(&parse.syntax(), expr_ast_id) else {
        return ExprType::error(ExprTypeErrorKind::Unresolved);
    };

    let ctx = DbInferCtx {
        db,
        unit,
        source_file,
        ast_id_map: map,
    };

    let Some(expr) = Expr::cast(node) else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    lyra_semantic::type_infer::infer_expr_stmt(&expr, &ctx)
}

/// Salsa-interned key for an integral context (width, signedness, four-state).
#[salsa::interned]
pub struct IntegralCtxKey<'db> {
    pub width: Option<u32>,
    pub signed: Signedness,
    pub four_state: bool,
}

impl IntegralCtxKey<'_> {
    pub fn to_ctx(self, db: &dyn salsa::Database) -> IntegralCtx {
        IntegralCtx {
            width: self.width(db),
            signed: self.signed(db),
            four_state: self.four_state(db),
        }
    }
}

/// Infer the type of an expression under a given integral context (Salsa-tracked).
#[salsa::tracked]
pub fn type_of_expr_in_ctx<'db>(
    db: &'db dyn salsa::Database,
    expr_ref: ExprRef<'db>,
    ctx_key: IntegralCtxKey<'db>,
) -> ExprType {
    let unit = expr_ref.unit(db);
    let expr_ast_id = expr_ref.expr_ast_id(db);
    let file_id = expr_ast_id.file();

    let Some(source_file) = source_file_by_id(db, unit, file_id) else {
        return ExprType::error(ExprTypeErrorKind::Unresolved);
    };

    let parse = parse_file(db, source_file);
    let map = ast_id_map(db, source_file);

    let Some(node) = map.get_node(&parse.syntax(), expr_ast_id) else {
        return ExprType::error(ExprTypeErrorKind::Unresolved);
    };

    let ctx = DbInferCtx {
        db,
        unit,
        source_file,
        ast_id_map: map,
    };

    let integral_ctx = ctx_key.to_ctx(db);
    let Some(expr) = Expr::cast(node) else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    lyra_semantic::type_infer::infer_expr(&expr, &ctx, Some(&integral_ctx))
}

/// Const-eval-safe expression typing (Salsa-tracked with cycle recovery).
///
/// This query must NOT call `resolve_index_file`, `type_of_symbol`,
/// or `record_sem`. It uses only the core/raw paths:
/// - `base_resolve_index` for name resolution
/// - `type_of_symbol_raw` for symbol types (unevaluated dims)
/// - `record_fields_raw` for record member lookup (no const-eval)
/// - `callable_signature_raw` for callable return types (no normalized dims)
/// - `eval_const_int` only via `InferCtx::const_eval` (already uses base resolve)
#[salsa::tracked(recovery_fn = type_of_expr_raw_recover)]
pub fn type_of_expr_raw<'db>(db: &'db dyn salsa::Database, expr_ref: ExprRef<'db>) -> ExprType {
    let unit = expr_ref.unit(db);
    let expr_ast_id = expr_ref.expr_ast_id(db);
    let file_id = expr_ast_id.file();

    let Some(source_file) = source_file_by_id(db, unit, file_id) else {
        return ExprType::error(ExprTypeErrorKind::Unresolved);
    };

    let parse = parse_file(db, source_file);
    let map = ast_id_map(db, source_file);

    let Some(node) = map.get_node(&parse.syntax(), expr_ast_id) else {
        return ExprType::error(ExprTypeErrorKind::Unresolved);
    };

    let ctx = DbInferCtxRaw {
        db,
        unit,
        source_file,
        ast_id_map: map,
    };

    let Some(expr) = Expr::cast(node) else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    lyra_semantic::type_infer::infer_expr(&expr, &ctx, None)
}

fn type_of_expr_raw_recover<'db>(
    _db: &'db dyn salsa::Database,
    _cycle: &salsa::Cycle,
    _expr_ref: ExprRef<'db>,
) -> ExprType {
    ExprType::error(ExprTypeErrorKind::Unresolved)
}

/// Database-backed implementation of `InferCtx`.
struct DbInferCtx<'a> {
    db: &'a dyn salsa::Database,
    unit: CompilationUnit,
    source_file: crate::SourceFile,
    ast_id_map: &'a lyra_ast::AstIdMap,
}

impl InferCtx for DbInferCtx<'_> {
    fn file_id(&self) -> lyra_source::FileId {
        self.source_file.file_id(self.db)
    }

    fn ast_id_map(&self) -> &AstIdMap {
        self.ast_id_map
    }

    fn type_of_name(&self, name_expr: &Expr) -> ExprType {
        type_of_name_impl(
            self.db,
            self.unit,
            self.ast_id_map,
            name_expr.syntax(),
            resolve_index_file(self.db, self.source_file, self.unit),
            &|db, unit, sym_id| {
                let sym_ref = SymbolRef::new(db, unit, sym_id);
                type_of_symbol(db, sym_ref)
            },
        )
    }

    fn const_eval(&self, expr: &Expr) -> ConstInt {
        const_eval_impl(self.db, self.unit, self.ast_id_map, expr.syntax())
    }

    fn resolve_callable(&self, callee: &Expr) -> Result<GlobalSymbolId, ResolveCallableError> {
        let resolve = resolve_index_file(self.db, self.source_file, self.unit);
        resolve_callable_impl(
            self.db,
            self.unit,
            self.ast_id_map,
            callee.syntax(),
            resolve,
        )
    }

    fn callable_sig(&self, sym: GlobalSymbolId) -> Option<CallableSigRef> {
        let callable_ref = CallableRef::new(self.db, self.unit, sym);
        let sig = callable_signature(self.db, callable_ref);
        let eval = |expr_ast_id: lyra_ast::ErasedAstId| -> ConstInt {
            let expr_ref = ConstExprRef::new(self.db, self.unit, expr_ast_id);
            eval_const_int(self.db, expr_ref)
        };
        let normalize = |ty: &lyra_semantic::types::Ty| -> lyra_semantic::types::Ty {
            lyra_semantic::normalize_ty(ty, &eval)
        };
        let ports: Vec<CallablePort> = sig
            .ports
            .iter()
            .map(|p| CallablePort {
                name: p.name.clone(),
                ty: normalize(&p.ty),
                has_default: p.has_default,
            })
            .collect();
        Some(CallableSigRef {
            kind: db_to_infer_callable_kind(sig.kind),
            return_ty: normalize(&sig.return_ty),
            ports: ports.into(),
        })
    }

    fn enum_integral_view(
        &self,
        id: &lyra_semantic::enum_def::EnumId,
    ) -> Option<lyra_semantic::type_infer::BitVecType> {
        enum_integral_view_impl(self.db, self.unit, id)
    }

    fn member_lookup(&self, ty: &Ty, member_name: &str) -> Result<MemberInfo, MemberLookupError> {
        if let Some(result) = builtin_member_lookup(ty, member_name)? {
            return Ok(result);
        }
        match ty {
            Ty::Record(id) => {
                let rref = RecordRef::new(self.db, self.unit, *id);
                let sem = record_sem(self.db, rref);
                match sem.field_by_name(member_name) {
                    Some((idx, field)) => Ok(MemberInfo {
                        ty: field.ty.clone(),
                        kind: MemberKind::Field { index: idx },
                        receiver: None,
                    }),
                    None => Err(MemberLookupError::UnknownMember),
                }
            }
            Ty::Interface(iface_ty) => interface_member_lookup(
                self.db,
                self.unit,
                iface_ty,
                member_name,
                &|db, unit, gsym| {
                    let sym_ref = SymbolRef::new(db, unit, gsym);
                    type_of_symbol(db, sym_ref)
                },
                &|db, unit, expr_id| {
                    let expr_ref = ExprRef::new(db, unit, expr_id);
                    type_of_expr(db, expr_ref)
                },
            ),
            _ => Err(MemberLookupError::NoMembersOnType),
        }
    }

    fn resolve_type_arg(&self, utr: &lyra_semantic::UserTypeRef) -> Option<Ty> {
        crate::resolve_helpers::resolve_type_arg_impl(
            self.db,
            self.unit,
            self.source_file,
            self.ast_id_map,
            utr,
        )
    }
}

/// Const-eval-safe implementation of `InferCtx`.
///
/// Uses `base_resolve_index` + `type_of_symbol_raw` instead of
/// `resolve_index_file` + `type_of_symbol`. Uses `record_fields_raw`
/// instead of `record_sem` for member lookup.
struct DbInferCtxRaw<'a> {
    db: &'a dyn salsa::Database,
    unit: CompilationUnit,
    source_file: crate::SourceFile,
    ast_id_map: &'a lyra_ast::AstIdMap,
}

impl InferCtx for DbInferCtxRaw<'_> {
    fn file_id(&self) -> lyra_source::FileId {
        self.source_file.file_id(self.db)
    }

    fn ast_id_map(&self) -> &AstIdMap {
        self.ast_id_map
    }

    fn type_of_name(&self, name_expr: &Expr) -> ExprType {
        type_of_name_impl(
            self.db,
            self.unit,
            self.ast_id_map,
            name_expr.syntax(),
            base_resolve_index(self.db, self.source_file, self.unit),
            &|db, unit, sym_id| {
                let sym_ref = SymbolRef::new(db, unit, sym_id);
                let raw = type_of_symbol_raw(db, sym_ref);
                lyra_semantic::normalize_symbol_type(&raw, &|expr_ast_id| {
                    let expr_ref = ConstExprRef::new(db, unit, expr_ast_id);
                    eval_const_int(db, expr_ref)
                })
            },
        )
    }

    fn const_eval(&self, expr: &Expr) -> ConstInt {
        const_eval_impl(self.db, self.unit, self.ast_id_map, expr.syntax())
    }

    fn resolve_callable(&self, callee: &Expr) -> Result<GlobalSymbolId, ResolveCallableError> {
        let resolve = base_resolve_index(self.db, self.source_file, self.unit);
        resolve_callable_impl(
            self.db,
            self.unit,
            self.ast_id_map,
            callee.syntax(),
            resolve,
        )
    }

    fn callable_sig(&self, sym: GlobalSymbolId) -> Option<CallableSigRef> {
        let sig = callable_signature_raw(self.db, self.unit, sym);
        let eval = |expr_ast_id: lyra_ast::ErasedAstId| -> ConstInt {
            let expr_ref = ConstExprRef::new(self.db, self.unit, expr_ast_id);
            eval_const_int(self.db, expr_ref)
        };
        let normalize = |ty: &Ty| -> Ty { lyra_semantic::normalize_ty(ty, &eval) };
        let ports: Vec<CallablePort> = sig
            .ports
            .iter()
            .map(|p| CallablePort {
                name: p.name.clone(),
                ty: normalize(&p.ty),
                has_default: p.has_default,
            })
            .collect();
        Some(CallableSigRef {
            kind: db_to_infer_callable_kind(sig.kind),
            return_ty: normalize(&sig.return_ty),
            ports: ports.into(),
        })
    }

    fn enum_integral_view(
        &self,
        id: &lyra_semantic::enum_def::EnumId,
    ) -> Option<lyra_semantic::type_infer::BitVecType> {
        enum_integral_view_impl(self.db, self.unit, id)
    }

    fn member_lookup(&self, ty: &Ty, member_name: &str) -> Result<MemberInfo, MemberLookupError> {
        if let Some(result) = builtin_member_lookup(ty, member_name)? {
            return Ok(result);
        }
        let normalize_field = |ty: Ty| -> Ty {
            lyra_semantic::normalize_ty(&ty, &|expr_ast_id| {
                let expr_ref = ConstExprRef::new(self.db, self.unit, expr_ast_id);
                eval_const_int(self.db, expr_ref)
            })
        };
        match ty {
            Ty::Record(id) => {
                let rref = RecordRef::new(self.db, self.unit, *id);
                let fields = record_fields_raw(self.db, rref);
                let (idx, field) = fields
                    .iter()
                    .enumerate()
                    .find(|(_, f)| f.name == member_name)
                    .ok_or(MemberLookupError::UnknownMember)?;
                Ok(MemberInfo {
                    ty: normalize_field(field.ty.clone()),
                    kind: MemberKind::Field { index: idx as u32 },
                    receiver: None,
                })
            }
            Ty::Interface(iface_ty) => {
                interface_member_lookup_raw(self.db, self.unit, iface_ty, member_name)
            }
            _ => Err(MemberLookupError::NoMembersOnType),
        }
    }

    fn resolve_type_arg(&self, utr: &lyra_semantic::UserTypeRef) -> Option<Ty> {
        resolve_type_arg_raw_impl(self.db, self.unit, self.source_file, self.ast_id_map, utr)
    }
}

// Shared helpers used by both DbInferCtx and DbInferCtxRaw

fn interface_member_lookup_raw(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    iface_ty: &lyra_semantic::types::InterfaceType,
    member_name: &str,
) -> Result<MemberInfo, MemberLookupError> {
    interface_member_lookup(
        db,
        unit,
        iface_ty,
        member_name,
        &|db, unit, gsym| {
            let sym_ref = SymbolRef::new(db, unit, gsym);
            let raw = type_of_symbol_raw(db, sym_ref);
            lyra_semantic::normalize_symbol_type(&raw, &|expr_ast_id| {
                let expr_ref = ConstExprRef::new(db, unit, expr_ast_id);
                eval_const_int(db, expr_ref)
            })
        },
        &|db, unit, expr_id| {
            let expr_ref = ExprRef::new(db, unit, expr_id);
            type_of_expr_raw(db, expr_ref)
        },
    )
}

/// Shared builtin member lookup for Enum, Array, and String types.
///
/// `Ok(Some(info))` = resolved successfully.
/// `Err(_)` = tried and failed (unknown method, wrong receiver).
/// `Ok(None)` = not a builtin-method type (caller handles Record/Interface).
fn builtin_member_lookup(
    ty: &Ty,
    member_name: &str,
) -> Result<Option<MemberInfo>, MemberLookupError> {
    match ty {
        Ty::Enum(enum_id) => {
            let method =
                EnumMethodKind::from_name(member_name).ok_or(MemberLookupError::UnknownMember)?;
            Ok(Some(MemberInfo {
                ty: method.return_ty(*enum_id),
                kind: MemberKind::BuiltinMethod(BuiltinMethodKind::Enum(method)),
                receiver: None,
            }))
        }
        Ty::Array { .. } => {
            let recv = classify_array_receiver(ty).ok_or(MemberLookupError::NoMembersOnType)?;
            let method =
                ArrayMethodKind::from_name(member_name).ok_or(MemberLookupError::UnknownMember)?;
            method
                .allowed_on(&recv)
                .map_err(MemberLookupError::MethodNotValidOnReceiver)?;
            let ret_ty = method.return_ty(&recv);
            Ok(Some(MemberInfo {
                ty: ret_ty,
                kind: MemberKind::BuiltinMethod(BuiltinMethodKind::Array(method)),
                receiver: Some(ReceiverInfo::Array(recv)),
            }))
        }
        Ty::String => {
            let method =
                StringMethodKind::from_name(member_name).ok_or(MemberLookupError::UnknownMember)?;
            let sig = method.sig();
            Ok(Some(MemberInfo {
                ty: sig.ret.to_ty(),
                kind: MemberKind::BuiltinMethod(BuiltinMethodKind::String(method)),
                receiver: None,
            }))
        }
        _ => Ok(None),
    }
}

fn type_of_name_impl(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    ast_id_map: &lyra_ast::AstIdMap,
    name_node: &lyra_parser::SyntaxNode,
    resolve: &lyra_semantic::resolve_index::ResolveIndex,
    type_of_sym: &dyn Fn(
        &dyn salsa::Database,
        CompilationUnit,
        GlobalSymbolId,
    ) -> lyra_semantic::types::SymbolType,
) -> ExprType {
    let Some(name_site_id) = ast_id_map.erased_ast_id(name_node) else {
        return ExprType::error(ExprTypeErrorKind::Unresolved);
    };
    let Some(resolution) = resolve.resolutions.get(&name_site_id) else {
        return ExprType::error(ExprTypeErrorKind::Unresolved);
    };
    match &resolution.target {
        lyra_semantic::resolve_index::ResolvedTarget::Symbol(sym_id) => {
            let sym_type = type_of_sym(db, unit, *sym_id);
            ExprType::from_symbol_type(&sym_type)
        }
        lyra_semantic::resolve_index::ResolvedTarget::Def(def_id) => {
            let ty = crate::ty_resolve::def_target_ty(db, unit, *def_id);
            ExprType::from_ty(&ty)
        }
        lyra_semantic::resolve_index::ResolvedTarget::EnumVariant(target) => {
            ExprType::from_ty(&Ty::Enum(target.enum_id))
        }
    }
}

fn interface_member_lookup(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    iface_ty: &lyra_semantic::types::InterfaceType,
    member_name: &str,
    resolve_sym_type: &dyn Fn(
        &dyn salsa::Database,
        CompilationUnit,
        GlobalSymbolId,
    ) -> lyra_semantic::types::SymbolType,
    resolve_expr_type: &dyn Fn(
        &dyn salsa::Database,
        CompilationUnit,
        lyra_ast::ErasedAstId,
    ) -> ExprType,
) -> Result<MemberInfo, MemberLookupError> {
    let iface_def_id = iface_ty.iface.global_def();
    let file_id = iface_def_id.ast_id().file();
    let src = source_file_by_id(db, unit, file_id).ok_or(MemberLookupError::NoMembersOnType)?;
    let def = def_index_file(db, src);
    let entry = def
        .def_entry(iface_def_id)
        .ok_or(MemberLookupError::NoMembersOnType)?;
    let lyra_semantic::def_entry::DefScope::Owned(iface_scope) = entry.scope else {
        return Err(MemberLookupError::NoMembersOnType);
    };

    if let Some(mp_id) = iface_ty.modport {
        let ctx = IfaceLookupCtx {
            db,
            unit,
            file_id,
            def,
            iface_scope,
            resolve_sym_type,
            resolve_expr_type,
        };
        return modport_member_lookup(&ctx, iface_ty, mp_id, member_name);
    }
    // No modport: resolve directly in interface scope
    if let Some(sym) = def.scopes.resolve(
        &def.symbols,
        iface_scope,
        lyra_semantic::symbols::Namespace::Value,
        member_name,
    ) {
        let global_member = GlobalSymbolId {
            file: file_id,
            local: sym,
        };
        let ty = match resolve_sym_type(db, unit, global_member) {
            lyra_semantic::types::SymbolType::Value(ty) => ty,
            lyra_semantic::types::SymbolType::Net(net) => net.data.clone(),
            _ => return Err(MemberLookupError::UnknownMember),
        };
        return Ok(MemberInfo {
            ty,
            kind: MemberKind::InterfaceMember { member: sym },
            receiver: None,
        });
    }
    if let Some(mp_def) = def.modport_by_name(iface_ty.iface, member_name) {
        let mp_ty = Ty::Interface(lyra_semantic::types::InterfaceType {
            iface: iface_ty.iface,
            modport: Some(mp_def.id),
        });
        return Ok(MemberInfo {
            ty: mp_ty,
            kind: MemberKind::Modport,
            receiver: None,
        });
    }
    Err(MemberLookupError::UnknownMember)
}

struct IfaceLookupCtx<'a> {
    db: &'a dyn salsa::Database,
    unit: CompilationUnit,
    file_id: lyra_source::FileId,
    def: &'a lyra_semantic::def_index::DefIndex,
    iface_scope: lyra_semantic::scopes::ScopeId,
    resolve_sym_type: &'a dyn Fn(
        &dyn salsa::Database,
        CompilationUnit,
        GlobalSymbolId,
    ) -> lyra_semantic::types::SymbolType,
    resolve_expr_type:
        &'a dyn Fn(&dyn salsa::Database, CompilationUnit, lyra_ast::ErasedAstId) -> ExprType,
}

fn modport_member_lookup(
    ctx: &IfaceLookupCtx<'_>,
    iface_ty: &lyra_semantic::types::InterfaceType,
    mp_id: lyra_semantic::modport_def::ModportDefId,
    member_name: &str,
) -> Result<MemberInfo, MemberLookupError> {
    let mref = ModportRef::new(ctx.db, ctx.unit, mp_id);
    let sem = modport_sem(ctx.db, mref);
    if let Some(entry) = sem.view.lookup(member_name) {
        let (ty, kind) = match &entry.target {
            lyra_semantic::types::ModportViewTarget::Member(sym_id) => {
                let global_member = GlobalSymbolId {
                    file: ctx.file_id,
                    local: *sym_id,
                };
                let ty = match (ctx.resolve_sym_type)(ctx.db, ctx.unit, global_member) {
                    lyra_semantic::types::SymbolType::Value(ty) => ty,
                    lyra_semantic::types::SymbolType::Net(net) => net.data.clone(),
                    _ => return Err(MemberLookupError::UnknownMember),
                };
                let kind = MemberKind::ModportPort {
                    port_id: entry.port_id,
                    target: lyra_semantic::member::ModportPortTarget::Member(*sym_id),
                };
                (ty, kind)
            }
            lyra_semantic::types::ModportViewTarget::Expr(expr_id) => {
                let expr_type = (ctx.resolve_expr_type)(ctx.db, ctx.unit, *expr_id);
                let kind = MemberKind::ModportPort {
                    port_id: entry.port_id,
                    target: lyra_semantic::member::ModportPortTarget::Expr(*expr_id),
                };
                (expr_type.ty, kind)
            }
            lyra_semantic::types::ModportViewTarget::Empty => {
                let kind = MemberKind::ModportPort {
                    port_id: entry.port_id,
                    target: lyra_semantic::member::ModportPortTarget::Empty,
                };
                (Ty::Error, kind)
            }
        };
        return Ok(MemberInfo {
            ty,
            kind,
            receiver: None,
        });
    }
    let exists_in_iface = ctx
        .def
        .scopes
        .resolve(
            &ctx.def.symbols,
            ctx.iface_scope,
            lyra_semantic::symbols::Namespace::Value,
            member_name,
        )
        .is_some()
        || ctx
            .def
            .modport_by_name(iface_ty.iface, member_name)
            .is_some();
    if exists_in_iface {
        return Err(MemberLookupError::NotInModport);
    }
    Err(MemberLookupError::UnknownMember)
}

fn const_eval_impl(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    ast_id_map: &lyra_ast::AstIdMap,
    expr_node: &lyra_parser::SyntaxNode,
) -> ConstInt {
    let Some(ast_id) = ast_id_map.erased_ast_id(expr_node) else {
        return ConstInt::Error(lyra_semantic::types::ConstEvalError::Unsupported);
    };
    let expr_ref = ConstExprRef::new(db, unit, ast_id);
    eval_const_int(db, expr_ref)
}

fn resolve_callable_impl(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    ast_id_map: &lyra_ast::AstIdMap,
    callee_node: &lyra_parser::SyntaxNode,
    resolve: &lyra_semantic::resolve_index::ResolveIndex,
) -> Result<GlobalSymbolId, ResolveCallableError> {
    let Some(ast_id) = ast_id_map.erased_ast_id(callee_node) else {
        return Err(ResolveCallableError::NotFound);
    };
    let Some(res) = resolve.resolutions.get(&ast_id) else {
        return Err(ResolveCallableError::NotFound);
    };
    let target_id = match &res.target {
        lyra_semantic::resolve_index::ResolvedTarget::Symbol(s) => *s,
        lyra_semantic::resolve_index::ResolvedTarget::Def(_) => {
            return Err(ResolveCallableError::NotFound);
        }
        lyra_semantic::resolve_index::ResolvedTarget::EnumVariant(_) => {
            return Err(ResolveCallableError::NotACallable(SymbolKind::EnumMember));
        }
    };
    let Some(target_file) = source_file_by_id(db, unit, target_id.file) else {
        return Err(ResolveCallableError::NotFound);
    };
    let target_def = def_index_file(db, target_file);
    let target_info = target_def.symbols.get(target_id.local);
    match target_info.kind {
        SymbolKind::Function | SymbolKind::Task => Ok(target_id),
        other => Err(ResolveCallableError::NotACallable(other)),
    }
}

fn enum_integral_view_impl(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    id: &lyra_semantic::enum_def::EnumId,
) -> Option<lyra_semantic::type_infer::BitVecType> {
    let eref = EnumRef::new(db, unit, *id);
    let sem = enum_sem(db, eref);
    sem.base_int
}

fn db_to_infer_callable_kind(kind: DbCallableKind) -> CallableKind {
    match kind {
        DbCallableKind::Function => CallableKind::Function,
        DbCallableKind::Task => CallableKind::Task,
    }
}

/// Resolve a `UserTypeRef` as a type using raw (base) resolution.
fn resolve_type_arg_raw_impl(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    source_file: crate::SourceFile,
    ast_id_map: &lyra_ast::AstIdMap,
    utr: &lyra_semantic::UserTypeRef,
) -> Option<Ty> {
    let name_node = crate::resolve_helpers::utr_syntax(utr);
    let ast_id = ast_id_map.erased_ast_id(name_node)?;
    let resolve = base_resolve_index(db, source_file, unit);
    let resolution = resolve.resolutions.get(&ast_id)?;
    let sym_id = match &resolution.target {
        lyra_semantic::resolve_index::ResolvedTarget::Symbol(s) => *s,
        lyra_semantic::resolve_index::ResolvedTarget::Def(def_id) => {
            return Some(crate::ty_resolve::def_target_ty(db, unit, *def_id));
        }
        lyra_semantic::resolve_index::ResolvedTarget::EnumVariant(target) => {
            return Some(Ty::Enum(target.enum_id));
        }
    };
    let sym_ref = SymbolRef::new(db, unit, sym_id);
    let sym_type = type_of_symbol_raw(db, sym_ref);
    match &sym_type {
        lyra_semantic::types::SymbolType::TypeAlias(ty) => Some(ty.clone()),
        _ => None,
    }
}
