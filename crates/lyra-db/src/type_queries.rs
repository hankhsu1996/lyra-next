use lyra_ast::{TypeDeclSite, UnpackedDimSource};
use lyra_semantic::UserTypeRef;
use lyra_semantic::record::{RecordKind, SymbolOrigin};
use lyra_semantic::resolve_index::CoreResolveResult;
use lyra_semantic::symbols::GlobalSymbolId;
use lyra_semantic::type_infer::BitWidth;
use lyra_semantic::types::{ConstInt, InterfaceType, SymbolType, Ty, UnpackedDim};

use crate::const_eval::{ConstExprRef, eval_const_int};
use crate::enum_queries::{EnumRef, enum_sem};
use crate::pipeline::{ast_id_map, parse_file};
use crate::record_queries::{RecordRef, record_field_tys};
use crate::semantic::{def_index_file, resolve_core_file, resolve_index_file};
use crate::{CompilationUnit, SourceFile, source_file_by_id};

/// Identifies a symbol for type extraction.
///
/// Includes the compilation unit because typedef resolution and
/// const-eval depend on unit-scoped name resolution.
#[salsa::interned]
pub struct SymbolRef<'db> {
    pub unit: CompilationUnit,
    pub symbol: GlobalSymbolId,
}

/// Salsa-interned type identity. Structural: same `Ty` content = same `TyRef`.
///
/// No `unit` in the key -- type identity is structural across compilation
/// units. All IDs embedded in `Ty` variants (`EnumId`, `RecordId`,
/// `InterfaceDefId`) are globally unique (contain `FileId`).
#[salsa::interned]
pub struct TyRef<'db> {
    pub ty: lyra_semantic::types::Ty,
}

/// Total bit width of a type (Salsa-tracked with cycle recovery).
///
/// Returns `Some(w)` when all sub-widths are known, `None` otherwise.
/// Handles Integral, Real, Enum (via base type), and Record (struct=sum, union=max).
#[salsa::tracked(recovery_fn = bit_width_total_recover)]
pub fn bit_width_total<'db>(
    db: &'db dyn salsa::Database,
    unit: CompilationUnit,
    ty_ref: TyRef<'db>,
) -> Option<u32> {
    let ty = ty_ref.ty(db);
    match &ty {
        Ty::Integral(i) => i.try_packed_width(),
        Ty::Real(rk) => Some(rk.bit_width()),
        Ty::Enum(id) => {
            let eref = EnumRef::new(db, unit, *id);
            let sem = enum_sem(db, eref);
            sem.base_int.and_then(|bi| match bi.width {
                BitWidth::Known(w) => Some(w),
                _ => None,
            })
        }
        Ty::Record(id) => {
            let rref = RecordRef::new(db, unit, *id);
            record_width(db, unit, rref)
        }
        Ty::Array { elem, dim } => {
            let len = dim.fixed_len()?;
            let elem_ref = TyRef::new(db, elem.as_ref().clone());
            let elem_width = bit_width_total(db, unit, elem_ref)?;
            len.checked_mul(elem_width)
        }
        Ty::String | Ty::Chandle | Ty::Event | Ty::Void | Ty::Interface(_) | Ty::Error => None,
    }
}

fn bit_width_total_recover<'db>(
    _db: &'db dyn salsa::Database,
    _cycle: &salsa::Cycle,
    _unit: CompilationUnit,
    _ty_ref: TyRef<'db>,
) -> Option<u32> {
    None
}

/// Compute the total bit-stream width of a record (struct or union).
///
/// Struct: field concatenation in declared order (sum of field widths).
/// Union (hard or soft): one active field (max of field widths).
/// Tagged unions return `None`.
fn record_width<'db>(
    db: &'db dyn salsa::Database,
    unit: CompilationUnit,
    rref: RecordRef<'db>,
) -> Option<u32> {
    let record_id = rref.record_id(db);
    let file_id = record_id.file();

    let source_file = source_file_by_id(db, unit, file_id)?;
    let def = def_index_file(db, source_file);

    let record_def = def.record_def_by_id(record_id)?;

    if record_def.kind == RecordKind::TaggedUnion {
        return None;
    }

    let lowered = record_field_tys(db, rref);
    if lowered.is_empty() {
        return Some(0);
    }

    let eval = |expr_ast_id: lyra_ast::ErasedAstId| -> ConstInt {
        let expr_ref = ConstExprRef::new(db, unit, expr_ast_id);
        eval_const_int(db, expr_ref)
    };

    let mut widths = Vec::with_capacity(lowered.len());
    for field_ty in &*lowered {
        let normalized = lyra_semantic::normalize_ty(&field_ty.ty, &eval);
        let ty_ref = TyRef::new(db, normalized);
        let w = bit_width_total(db, unit, ty_ref)?;
        widths.push(w);
    }

    match record_def.kind {
        RecordKind::Struct => {
            let mut total: u32 = 0;
            for w in &widths {
                total = total.checked_add(*w)?;
            }
            Some(total)
        }
        RecordKind::Union => widths.iter().copied().max(),
        RecordKind::TaggedUnion => None,
    }
}

/// Extract a symbol's raw type from its declaration AST (Salsa-tracked with cycle recovery).
///
/// Returns `SymbolType` with `Unevaluated` dims -- const-eval is NOT called.
/// May call `resolve_index_file` for single-step typedef expansion.
#[salsa::tracked(recovery_fn = type_of_symbol_raw_recover)]
pub fn type_of_symbol_raw<'db>(
    db: &'db dyn salsa::Database,
    sym_ref: SymbolRef<'db>,
) -> lyra_semantic::types::SymbolType {
    use lyra_semantic::symbols::SymbolKind;
    use lyra_semantic::types::{SymbolType, SymbolTypeError};
    use lyra_semantic::{extract_type_from_container, user_type_ref};

    let unit = sym_ref.unit(db);
    let gsym = sym_ref.symbol(db);

    let Some(source_file) = source_file_by_id(db, unit, gsym.file) else {
        return SymbolType::Error(SymbolTypeError::MissingDecl);
    };

    let def = def_index_file(db, source_file);
    let sym = def.symbols.get(gsym.local);

    // Definition-namespace symbols have no meaningful type
    match sym.kind {
        SymbolKind::Module
        | SymbolKind::Package
        | SymbolKind::Interface
        | SymbolKind::Program
        | SymbolKind::Primitive
        | SymbolKind::Config
        | SymbolKind::Function
        | SymbolKind::Task
        | SymbolKind::Modport => return SymbolType::Error(SymbolTypeError::UnsupportedSymbolKind),
        _ => {}
    }

    // Check SymbolOrigin for enum/struct types
    match sym.origin {
        SymbolOrigin::Enum(idx) => {
            let id = def.enum_id(idx);
            let ty = Ty::Enum(id);
            let parse = parse_file(db, source_file);
            let map = ast_id_map(db, source_file);
            let dim_src = UnpackedDimSource::from_name_site(&parse.syntax(), map, sym.name_site);
            let ty = wrap_unpacked_dims(ty, dim_src.as_ref(), map);
            return classify(ty, sym.kind);
        }
        SymbolOrigin::Record(idx) => {
            let id = def.record_id(idx);
            let ty = Ty::Record(id);
            let parse = parse_file(db, source_file);
            let map = ast_id_map(db, source_file);
            let dim_src = UnpackedDimSource::from_name_site(&parse.syntax(), map, sym.name_site);
            let ty = wrap_unpacked_dims(ty, dim_src.as_ref(), map);
            return classify(ty, sym.kind);
        }
        SymbolOrigin::EnumVariant(enum_idx) => {
            let id = def.enum_id(enum_idx);
            return classify(Ty::Enum(id), sym.kind);
        }
        SymbolOrigin::Instance(idx) => {
            return type_of_instance(db, unit, source_file, idx, sym.kind);
        }
        SymbolOrigin::Error => {
            return classify(Ty::Error, sym.kind);
        }
        SymbolOrigin::TypeSpec => {}
    }

    let decl_site_id = sym.name_site;

    let parse = parse_file(db, source_file);
    let map = ast_id_map(db, source_file);

    let Some(decl_node) = map.get_node(&parse.syntax(), decl_site_id) else {
        return SymbolType::Error(SymbolTypeError::MissingDecl);
    };

    let Some((container, declarator)) = TypeDeclSite::resolve(&decl_node) else {
        return SymbolType::Error(SymbolTypeError::MissingDecl);
    };

    // Check for user-defined type (typedef expansion trigger)
    if let Some(ts) = container.type_spec()
        && let Some(utr) = user_type_ref(&ts)
    {
        let dim_owner = match &container {
            TypeDeclSite::Port(port) => Some(UnpackedDimSource::Port(port.clone())),
            _ => declarator.clone().map(UnpackedDimSource::Declarator),
        };
        return expand_typedef(
            db,
            unit,
            source_file,
            &utr,
            dim_owner.as_ref(),
            map,
            sym.kind,
        );
    }

    // No user-defined type -- pure extraction
    extract_type_from_container(&container, declarator.as_ref(), map)
}

/// Resolve the type of a module/interface instance.
///
/// Looks up the instance's type-name use-site in `resolve_core_file`,
/// checks if the target is an interface, and returns `Ty::Interface`.
/// Module instances return `UnsupportedSymbolKind`.
fn type_of_instance(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    source_file: SourceFile,
    idx: lyra_semantic::instance_decl::InstanceDeclIdx,
    kind: lyra_semantic::symbols::SymbolKind,
) -> lyra_semantic::types::SymbolType {
    use crate::ty_resolve::{DefTargetSem, def_target_sem};
    use lyra_semantic::resolve_index::CoreResolution;
    use lyra_semantic::types::{SymbolType, SymbolTypeError};

    let def = def_index_file(db, source_file);
    let decl = def.instance_decl(idx);
    let core = resolve_core_file(db, source_file, unit);

    let Some(result) = core.resolutions.get(decl.type_use_site_idx as usize) else {
        return SymbolType::Error(SymbolTypeError::UnsupportedSymbolKind);
    };

    let CoreResolveResult::Resolved(CoreResolution::Def { def: def_id }) = result else {
        return SymbolType::Error(SymbolTypeError::UnsupportedSymbolKind);
    };

    match def_target_sem(db, unit, *def_id) {
        Some(DefTargetSem::Interface(iface_def)) => classify(
            Ty::Interface(InterfaceType {
                iface: iface_def,
                modport: None,
            }),
            kind,
        ),
        _ => SymbolType::Error(SymbolTypeError::UnsupportedSymbolKind),
    }
}

/// Resolve a definition-namespace target to its base `Ty` (no dims).
///
/// For interfaces, produces `Ty::Interface(...)`.
/// All other definition kinds produce an error.
/// Dims are applied by the caller (`expand_typedef`), not here.
fn resolve_def_base_ty(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    def_id: lyra_semantic::symbols::GlobalDefId,
    user_type: &UserTypeRef,
) -> Result<Ty, lyra_semantic::types::SymbolType> {
    use crate::ty_resolve::{DefTargetSem, def_target_sem};
    use lyra_semantic::types::{SymbolType, SymbolTypeError};

    let Some(DefTargetSem::Interface(iface_def)) = def_target_sem(db, unit, def_id) else {
        return Err(SymbolType::Error(SymbolTypeError::UserTypeUnresolved));
    };
    let modport = match user_type {
        UserTypeRef::InterfaceModport { modport_name, .. } => {
            let target_file_id = def_id.ast_id().file();
            let Some(target_file) = source_file_by_id(db, unit, target_file_id) else {
                return Err(SymbolType::Error(SymbolTypeError::UserTypeUnresolved));
            };
            let target_def = def_index_file(db, target_file);
            match target_def.modport_by_name(iface_def, modport_name.as_str()) {
                Some(mp_def) => Some(mp_def.id),
                None => return Err(SymbolType::Error(SymbolTypeError::UnknownModport)),
            }
        }
        _ => None,
    };
    Ok(Ty::Interface(InterfaceType {
        iface: iface_def,
        modport,
    }))
}

/// Resolve a symbol-namespace target (typedef) to its base `Ty` (no dims).
///
/// For typedefs, chases one level of typedef indirection and returns the
/// underlying type. Dims are applied by the caller.
fn resolve_symbol_base_ty(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    target_id: lyra_semantic::symbols::GlobalSymbolId,
    user_type: &UserTypeRef,
) -> Result<Ty, lyra_semantic::types::SymbolType> {
    use lyra_semantic::types::{SymbolType, SymbolTypeError};

    let Some(target_file) = source_file_by_id(db, unit, target_id.file) else {
        return Err(SymbolType::Error(SymbolTypeError::UserTypeUnresolved));
    };
    let target_def = def_index_file(db, target_file);
    let target_info = target_def.symbols.get(target_id.local);

    match target_info.kind {
        lyra_semantic::symbols::SymbolKind::Typedef => {
            if matches!(user_type, UserTypeRef::InterfaceModport { .. }) {
                return Err(SymbolType::Error(SymbolTypeError::ModportOnNonInterface));
            }
            let typedef_ref = SymbolRef::new(db, unit, target_id);
            let typedef_type = type_of_symbol_raw(db, typedef_ref);

            match &typedef_type {
                SymbolType::TypeAlias(ty) | SymbolType::Value(ty) => Ok(ty.clone()),
                SymbolType::Error(e) => Err(SymbolType::Error(*e)),
                SymbolType::Net(_) => Err(SymbolType::Error(
                    SymbolTypeError::TypedefUnderlyingUnsupported,
                )),
            }
        }
        _ if matches!(user_type, UserTypeRef::InterfaceModport { .. }) => {
            Err(SymbolType::Error(SymbolTypeError::ModportOnNonInterface))
        }
        _ => Err(SymbolType::Error(SymbolTypeError::UserTypeUnresolved)),
    }
}

/// `dim_owner` carries unpacked dims to merge (Declarator for variables,
/// Port for ports, None for typedefs).
/// `caller_kind` is the symbol kind of the declaration being typed, used
/// to preserve `TypeAlias` classification for typedef symbols.
fn expand_typedef(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    source_file: SourceFile,
    user_type: &UserTypeRef,
    dim_owner: Option<&UnpackedDimSource>,
    id_map: &lyra_ast::AstIdMap,
    caller_kind: lyra_semantic::symbols::SymbolKind,
) -> lyra_semantic::types::SymbolType {
    use lyra_semantic::types::{SymbolType, SymbolTypeError, UnpackedDim};

    let resolve_node = crate::resolve_helpers::utr_syntax(user_type);

    // Look up the name in the resolve index
    let resolve = resolve_index_file(db, source_file, unit);
    let Some(name_site_id) = id_map.erased_ast_id(resolve_node) else {
        return SymbolType::Error(SymbolTypeError::UserTypeUnresolved);
    };
    let Some(resolution) = resolve.resolutions.get(&name_site_id) else {
        return SymbolType::Error(SymbolTypeError::UserTypeUnresolved);
    };

    // Resolve to base Ty (no dims). All target kinds go through this.
    let base_ty = match &resolution.target {
        lyra_semantic::resolve_index::ResolvedTarget::Def(def_id) => {
            match resolve_def_base_ty(db, unit, *def_id, user_type) {
                Ok(ty) => ty,
                Err(e) => return e,
            }
        }
        lyra_semantic::resolve_index::ResolvedTarget::Symbol(s) => {
            match resolve_symbol_base_ty(db, unit, *s, user_type) {
                Ok(ty) => ty,
                Err(e) => return e,
            }
        }
        lyra_semantic::resolve_index::ResolvedTarget::EnumVariant(_) => {
            return SymbolType::Error(SymbolTypeError::UserTypeUnresolved);
        }
    };

    // Apply use-site unpacked dims uniformly
    let use_site_unpacked: Vec<UnpackedDim> = dim_owner
        .map(|owner| {
            owner
                .unpacked_dimensions()
                .map(|dim| lyra_semantic::extract_unpacked_dim(&dim, id_map))
                .collect()
        })
        .unwrap_or_default();

    classify(
        lyra_semantic::wrap_unpacked(base_ty, &use_site_unpacked),
        caller_kind,
    )
}

fn classify(ty: Ty, kind: lyra_semantic::symbols::SymbolKind) -> lyra_semantic::types::SymbolType {
    use lyra_semantic::types::SymbolType;
    if kind == lyra_semantic::symbols::SymbolKind::Typedef {
        SymbolType::TypeAlias(ty)
    } else {
        SymbolType::Value(ty)
    }
}

fn wrap_unpacked_dims(
    ty: Ty,
    owner: Option<&UnpackedDimSource>,
    ast_id_map: &lyra_ast::AstIdMap,
) -> Ty {
    let Some(owner) = owner else { return ty };
    let dims: Vec<lyra_semantic::types::UnpackedDim> = owner
        .unpacked_dimensions()
        .map(|dim| lyra_semantic::extract_unpacked_dim(&dim, ast_id_map))
        .collect();
    if dims.is_empty() {
        return ty;
    }
    lyra_semantic::wrap_unpacked(ty, &dims)
}

fn type_of_symbol_raw_recover<'db>(
    _db: &'db dyn salsa::Database,
    _cycle: &salsa::Cycle,
    _sym_ref: SymbolRef<'db>,
) -> lyra_semantic::types::SymbolType {
    lyra_semantic::types::SymbolType::Error(lyra_semantic::types::SymbolTypeError::TypedefCycle)
}

/// Extract the normalized type of a symbol (Salsa-tracked).
///
/// Calls `type_of_symbol_raw` then normalizes all `Unevaluated` dims
/// via const-eval.
#[salsa::tracked]
pub fn type_of_symbol<'db>(
    db: &'db dyn salsa::Database,
    sym_ref: SymbolRef<'db>,
) -> lyra_semantic::types::SymbolType {
    let raw = type_of_symbol_raw(db, sym_ref);
    let unit = sym_ref.unit(db);
    lyra_semantic::normalize_symbol_type(&raw, &|expr_ast_id| {
        let expr_ref = ConstExprRef::new(db, unit, expr_ast_id);
        eval_const_int(db, expr_ref)
    })
}

/// An internal-error fact produced by type extraction.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InternalErrorFact {
    pub site: lyra_semantic::Site,
    pub detail: smol_str::SmolStr,
}

/// Derived query: extract `MissingSite` errors from a symbol's normalized type.
///
/// Depends on `type_of_symbol` (no re-extraction). Cached by Salsa.
#[salsa::tracked(return_ref)]
pub fn type_of_symbol_internal_errors<'db>(
    db: &'db dyn salsa::Database,
    sym_ref: SymbolRef<'db>,
) -> Vec<InternalErrorFact> {
    let sym_type = type_of_symbol(db, sym_ref);
    let gsym = sym_ref.symbol(db);
    let unit = sym_ref.unit(db);
    let Some(source_file) = source_file_by_id(db, unit, gsym.file) else {
        return Vec::new();
    };
    let def = def_index_file(db, source_file);
    let fallback = def.symbols.get(gsym.local).name_site;
    collect_missing_site_errors(&sym_type, fallback)
}

fn collect_missing_site_errors(
    sym: &SymbolType,
    fallback: lyra_semantic::Site,
) -> Vec<InternalErrorFact> {
    let ty = match sym {
        SymbolType::Value(ty) | SymbolType::TypeAlias(ty) => ty,
        SymbolType::Net(net) => &net.data,
        SymbolType::Error(_) => return Vec::new(),
    };
    let mut out = Vec::new();
    collect_missing_site_ty(ty, fallback, &mut out);
    out
}

fn collect_missing_site_ty(
    ty: &Ty,
    fallback: lyra_semantic::Site,
    out: &mut Vec<InternalErrorFact>,
) {
    match ty {
        Ty::Integral(i) => {
            for d in i.packed.iter() {
                collect_missing_site_const(&d.msb, fallback, out);
                collect_missing_site_const(&d.lsb, fallback, out);
            }
        }
        Ty::Array { elem, dim } => {
            collect_missing_site_dim(dim, fallback, out);
            collect_missing_site_ty(elem, fallback, out);
        }
        _ => {}
    }
}

fn collect_missing_site_dim(
    dim: &UnpackedDim,
    fallback: lyra_semantic::Site,
    out: &mut Vec<InternalErrorFact>,
) {
    match dim {
        UnpackedDim::Range { msb, lsb } => {
            collect_missing_site_const(msb, fallback, out);
            collect_missing_site_const(lsb, fallback, out);
        }
        UnpackedDim::Size(c) => collect_missing_site_const(c, fallback, out),
        UnpackedDim::Queue { bound } => {
            if let Some(c) = bound {
                collect_missing_site_const(c, fallback, out);
            }
        }
        UnpackedDim::Assoc(_) | UnpackedDim::Unsized => {}
    }
}

fn collect_missing_site_const(
    c: &ConstInt,
    fallback: lyra_semantic::Site,
    out: &mut Vec<InternalErrorFact>,
) {
    if let Some(origin) = c.missing_site_origin() {
        out.push(InternalErrorFact {
            site: fallback,
            detail: origin.detail(),
        });
    }
}
