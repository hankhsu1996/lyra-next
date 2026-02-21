use lyra_lexer::SyntaxKind;
use lyra_semantic::UserTypeRef;
use lyra_semantic::interface_id::InterfaceDefId;
use lyra_semantic::record::{Packing, RecordKind, SymbolOrigin};
use lyra_semantic::resolve_index::CoreResolveResult;
use lyra_semantic::symbols::GlobalSymbolId;
use lyra_semantic::type_infer::BitWidth;
use lyra_semantic::types::{ConstEvalError, ConstInt, InterfaceType, Ty, UnpackedDim};

use crate::const_eval::{ConstExprRef, eval_const_int};
use crate::enum_queries::{EnumRef, enum_sem};
use crate::pipeline::{ast_id_map, parse_file};
use crate::record_queries::{RecordRef, record_field_tys};
use crate::semantic::{def_index_file, global_def_index, resolve_core_file, resolve_index_file};
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
            let eref = EnumRef::new(db, unit, id.clone());
            let sem = enum_sem(db, eref);
            sem.base_int.and_then(|bi| match bi.width {
                BitWidth::Known(w) => Some(w),
                _ => None,
            })
        }
        Ty::Record(id) => {
            let rref = RecordRef::new(db, unit, id.clone());
            record_width(db, unit, rref)
        }
        _ => None,
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

/// Compute the total bit width of a packed record (struct or union).
///
/// Struct: sum of field widths. Union (hard or soft): max of field widths.
/// Unpacked records and tagged unions return None.
fn record_width<'db>(
    db: &'db dyn salsa::Database,
    unit: CompilationUnit,
    rref: RecordRef<'db>,
) -> Option<u32> {
    let record_id = rref.record_id(db);
    let file_id = record_id.file;

    let source_file = source_file_by_id(db, unit, file_id)?;
    let def = def_index_file(db, source_file);

    let record_def = def
        .record_defs
        .iter()
        .find(|rd| rd.owner == record_id.owner && rd.ordinal == record_id.ordinal)?;

    match (record_def.kind, record_def.packing) {
        (RecordKind::TaggedUnion, _) | (_, Packing::Unpacked) => return None,
        _ => {}
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
            let decl_ast_id = def.symbol_to_decl.get(gsym.local.index()).and_then(|o| *o);
            let decl_node = decl_ast_id.and_then(|id| map.get_node(&parse.syntax(), id));
            let ty = wrap_unpacked_dims_from_node(ty, decl_node.as_ref(), map);
            return classify(ty, sym.kind);
        }
        SymbolOrigin::Record(idx) => {
            let id = def.record_id(idx);
            let ty = Ty::Record(id);
            let parse = parse_file(db, source_file);
            let map = ast_id_map(db, source_file);
            let decl_ast_id = def.symbol_to_decl.get(gsym.local.index()).and_then(|o| *o);
            let decl_node = decl_ast_id.and_then(|id| map.get_node(&parse.syntax(), id));
            let ty = wrap_unpacked_dims_from_node(ty, decl_node.as_ref(), map);
            return classify(ty, sym.kind);
        }
        SymbolOrigin::EnumVariant { enum_idx, .. } => {
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

    let Some(decl_ast_id) = def.symbol_to_decl.get(gsym.local.index()).and_then(|o| *o) else {
        return SymbolType::Error(SymbolTypeError::MissingDecl);
    };

    let parse = parse_file(db, source_file);
    let map = ast_id_map(db, source_file);

    let Some(decl_node) = map.get_node(&parse.syntax(), decl_ast_id) else {
        return SymbolType::Error(SymbolTypeError::MissingDecl);
    };

    // For Port and TypedefDecl, the decl_ast_id points directly to the node.
    // For Variable/Net/Parameter, it points to a Declarator -- find the container.
    let (container, declarator) = match decl_node.kind() {
        SyntaxKind::Port | SyntaxKind::TypedefDecl => (decl_node.clone(), None),
        SyntaxKind::Declarator => {
            let Some(parent) = closest_decl_container(&decl_node) else {
                return SymbolType::Error(SymbolTypeError::MissingDecl);
            };
            (parent, Some(decl_node.clone()))
        }
        _ => return SymbolType::Error(SymbolTypeError::MissingDecl),
    };

    // Check for user-defined type (typedef expansion trigger)
    let typespec = container
        .children()
        .find(|c| c.kind() == SyntaxKind::TypeSpec);

    if let Some(ref ts) = typespec
        && let Some(utr) = user_type_ref(ts)
    {
        let dim_source = if decl_node.kind() == SyntaxKind::Port {
            Some(&decl_node)
        } else {
            declarator.as_ref()
        };
        return expand_typedef(db, unit, source_file, &utr, dim_source, map, sym.kind);
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
    use lyra_semantic::global_index::DefinitionKind;
    use lyra_semantic::resolve_index::CoreResolution;
    use lyra_semantic::types::{SymbolType, SymbolTypeError};

    let def = def_index_file(db, source_file);
    let decl = def.instance_decl(idx);
    let core = resolve_core_file(db, source_file, unit);

    let Some(result) = core.resolutions.get(decl.type_use_site_idx as usize) else {
        return SymbolType::Error(SymbolTypeError::UnsupportedSymbolKind);
    };

    let CoreResolveResult::Resolved(CoreResolution::Global { decl: def_id, .. }) = result else {
        return SymbolType::Error(SymbolTypeError::UnsupportedSymbolKind);
    };

    let global = global_def_index(db, unit);
    match global.def_kind(*def_id) {
        Some(DefinitionKind::Interface) => {
            let Some(iface_def) = InterfaceDefId::try_from_global_index(global, *def_id) else {
                return SymbolType::Error(SymbolTypeError::UnsupportedSymbolKind);
            };
            classify(
                Ty::Interface(InterfaceType {
                    iface: iface_def,
                    modport: None,
                }),
                kind,
            )
        }
        _ => SymbolType::Error(SymbolTypeError::UnsupportedSymbolKind),
    }
}

/// Expand a user-defined type reference via single-step typedef resolution.
///
/// `dim_source` is the node whose children contain unpacked dims to merge
/// (Declarator for variables, Port node for ports, None for typedefs).
/// `caller_kind` is the symbol kind of the declaration being typed, used
/// to preserve `TypeAlias` classification for typedef symbols.
fn expand_typedef(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    source_file: SourceFile,
    user_type: &UserTypeRef,
    dim_source: Option<&lyra_parser::SyntaxNode>,
    id_map: &lyra_ast::AstIdMap,
    caller_kind: lyra_semantic::symbols::SymbolKind,
) -> lyra_semantic::types::SymbolType {
    use lyra_semantic::types::{SymbolType, SymbolTypeError, UnpackedDim};

    let resolve_node = user_type.resolve_node();

    // Look up the name in the resolve index
    let resolve = resolve_index_file(db, source_file, unit);
    let Some(name_ast_id) = id_map.erased_ast_id(resolve_node) else {
        return SymbolType::Error(SymbolTypeError::UserTypeUnresolved);
    };
    let Some(resolution) = resolve.resolutions.get(&name_ast_id) else {
        return SymbolType::Error(SymbolTypeError::UserTypeUnresolved);
    };

    let target_id = resolution.symbol;

    // Look up target symbol kind
    let Some(target_file) = source_file_by_id(db, unit, target_id.file) else {
        return SymbolType::Error(SymbolTypeError::UserTypeUnresolved);
    };
    let target_def = def_index_file(db, target_file);
    let target_info = target_def.symbols.get(target_id.local);

    let ty = match target_info.kind {
        lyra_semantic::symbols::SymbolKind::Interface => {
            let Some(def_id) = target_def.symbol_global_def(target_id.local) else {
                return SymbolType::Error(SymbolTypeError::UserTypeUnresolved);
            };
            let global = global_def_index(db, unit);
            let Some(iface_def) = InterfaceDefId::try_from_global_index(global, def_id) else {
                return SymbolType::Error(SymbolTypeError::UserTypeUnresolved);
            };
            let modport = match &user_type {
                UserTypeRef::InterfaceModport { modport_name, .. } => {
                    match target_def.modport_by_name(iface_def, modport_name.as_str()) {
                        Some(mp_def) => Some(mp_def.id),
                        None => return SymbolType::Error(SymbolTypeError::UnknownModport),
                    }
                }
                _ => None,
            };
            Ty::Interface(InterfaceType {
                iface: iface_def,
                modport,
            })
        }
        lyra_semantic::symbols::SymbolKind::Typedef => {
            if matches!(user_type, UserTypeRef::InterfaceModport { .. }) {
                return SymbolType::Error(SymbolTypeError::ModportOnNonInterface);
            }
            // Recursively get the typedef's type (Salsa cycle detection)
            let typedef_ref = SymbolRef::new(db, unit, target_id);
            let typedef_type = type_of_symbol_raw(db, typedef_ref);

            let underlying_ty = match &typedef_type {
                SymbolType::TypeAlias(ty) | SymbolType::Value(ty) => ty.clone(),
                SymbolType::Error(e) => return SymbolType::Error(*e),
                SymbolType::Net(_) => {
                    return SymbolType::Error(SymbolTypeError::TypedefUnderlyingUnsupported);
                }
            };

            // Collect use-site unpacked dims from dim source node
            let use_site_unpacked: Vec<UnpackedDim> = dim_source
                .map(|d| extract_unpacked_dims_raw(d, id_map))
                .unwrap_or_default();

            let wrap = if caller_kind == lyra_semantic::symbols::SymbolKind::Typedef {
                SymbolType::TypeAlias
            } else {
                SymbolType::Value
            };
            return wrap(lyra_semantic::wrap_unpacked(
                underlying_ty,
                &use_site_unpacked,
            ));
        }
        _ if matches!(user_type, UserTypeRef::InterfaceModport { .. }) => {
            return SymbolType::Error(SymbolTypeError::ModportOnNonInterface);
        }
        _ => return SymbolType::Error(SymbolTypeError::UserTypeUnresolved),
    };

    // Common wrapping + classification for non-typedef type targets
    let use_site_unpacked: Vec<UnpackedDim> = dim_source
        .map(|d| extract_unpacked_dims_raw(d, id_map))
        .unwrap_or_default();
    let wrap = if caller_kind == lyra_semantic::symbols::SymbolKind::Typedef {
        SymbolType::TypeAlias
    } else {
        SymbolType::Value
    };
    wrap(lyra_semantic::wrap_unpacked(ty, &use_site_unpacked))
}

/// Extract unpacked dims from a node (used during typedef expansion in db crate).
fn extract_unpacked_dims_raw(
    node: &lyra_parser::SyntaxNode,
    ast_id_map: &lyra_ast::AstIdMap,
) -> Vec<UnpackedDim> {
    let mut dims = Vec::new();
    for child in node.children() {
        if child.kind() == SyntaxKind::UnpackedDimension {
            let exprs: Vec<_> = child
                .children()
                .filter(|c| is_expression_kind(c.kind()))
                .collect();
            match exprs.len() {
                2 => {
                    let msb = expr_to_const_int(&exprs[0], ast_id_map);
                    let lsb = expr_to_const_int(&exprs[1], ast_id_map);
                    dims.push(UnpackedDim::Range { msb, lsb });
                }
                1 => {
                    let size = expr_to_const_int(&exprs[0], ast_id_map);
                    dims.push(UnpackedDim::Size(size));
                }
                _ => {
                    dims.push(UnpackedDim::Size(ConstInt::Error(
                        ConstEvalError::Unsupported,
                    )));
                }
            }
        }
    }
    dims
}

fn expr_to_const_int(expr: &lyra_parser::SyntaxNode, ast_id_map: &lyra_ast::AstIdMap) -> ConstInt {
    match ast_id_map.erased_ast_id(expr) {
        Some(id) => ConstInt::Unevaluated(id),
        None => ConstInt::Error(ConstEvalError::Unsupported),
    }
}

pub(crate) fn is_expression_kind(kind: SyntaxKind) -> bool {
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
            | SyntaxKind::NameRef
            | SyntaxKind::Literal
            | SyntaxKind::QualifiedName
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

// Extract unpacked dims from a declaration node and wrap a type.
fn wrap_unpacked_dims_from_node(
    ty: Ty,
    node: Option<&lyra_parser::SyntaxNode>,
    ast_id_map: &lyra_ast::AstIdMap,
) -> Ty {
    let Some(node) = node else { return ty };
    if !matches!(
        node.kind(),
        SyntaxKind::Declarator | SyntaxKind::TypedefDecl
    ) {
        return ty;
    }
    let dims = extract_unpacked_dims_raw(node, ast_id_map);
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

/// Find the closest declaration container above a Declarator.
fn closest_decl_container(node: &lyra_parser::SyntaxNode) -> Option<lyra_parser::SyntaxNode> {
    node.ancestors().find(|n| {
        matches!(
            n.kind(),
            SyntaxKind::VarDecl | SyntaxKind::NetDecl | SyntaxKind::ParamDecl
        )
    })
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
