use lyra_ast::{AstNode, NameRef, QualifiedName};
use lyra_lexer::SyntaxKind;
use lyra_semantic::resolve_index::{CoreResolution, CoreResolveResult};
use lyra_semantic::symbols::{GlobalSymbolId, Namespace};
use lyra_semantic::system_call_view::SystemCallView;
use lyra_semantic::types::{ConstEvalError, ConstInt, SymbolType, Ty};
use smol_str::SmolStr;

use crate::expr_queries::{ExprRef, type_of_expr_raw};
use crate::pipeline::{ast_id_map, parse_file};
use crate::semantic::{
    base_resolve_index, compilation_unit_env, def_index_file, global_def_index, name_graph_file,
    package_scope_index,
};
use crate::type_queries::{SymbolRef, TyRef, bit_width_total, type_of_symbol_raw};
use crate::{CompilationUnit, SourceFile, source_file_by_id};

/// Identifies a constant expression for evaluation.
///
/// Const-eval depends on the compilation unit because name resolution
/// (imports, global definitions, package visibility) is unit-scoped.
#[salsa::interned]
pub struct ConstExprRef<'db> {
    pub unit: CompilationUnit,
    pub expr_ast_id: lyra_ast::ErasedAstId,
}

/// System functions that can be evaluated at const-eval time.
enum ConstIntrinsic {
    Bits,
    Left,
    Right,
    Low,
    High,
    Size,
    Increment,
    Dimensions,
    UnpackedDimensions,
}

impl ConstIntrinsic {
    fn from_name(name: &str) -> Option<Self> {
        match name {
            "$bits" => Some(Self::Bits),
            "$left" => Some(Self::Left),
            "$right" => Some(Self::Right),
            "$low" => Some(Self::Low),
            "$high" => Some(Self::High),
            "$size" => Some(Self::Size),
            "$increment" => Some(Self::Increment),
            "$dimensions" => Some(Self::Dimensions),
            "$unpacked_dimensions" => Some(Self::UnpackedDimensions),
            _ => None,
        }
    }
}

/// Evaluate a constant integer expression (Salsa-tracked with cycle recovery).
///
/// Returns `ConstInt::Known(v)` on success, `ConstInt::Error(e)` on failure.
/// Cycles (e.g. `parameter A = B; parameter B = A;`) are detected by Salsa
/// and recovered via `const_eval_recover`.
#[salsa::tracked(recovery_fn = const_eval_recover)]
pub fn eval_const_int<'db>(db: &'db dyn salsa::Database, expr_ref: ConstExprRef<'db>) -> ConstInt {
    let unit = expr_ref.unit(db);
    let expr_ast_id = expr_ref.expr_ast_id(db);
    let file_id = expr_ast_id.file();

    let Some(source_file) = source_file_by_id(db, unit, file_id) else {
        return ConstInt::Error(ConstEvalError::Unresolved);
    };

    let parse = parse_file(db, source_file);
    let map = ast_id_map(db, source_file);

    let Some(node) = map.get_node(&parse.syntax(), expr_ast_id) else {
        return ConstInt::Error(ConstEvalError::Unresolved);
    };

    let resolve_name = |name_node: &lyra_parser::SyntaxNode| -> Result<i64, ConstEvalError> {
        let name_ast_id = map
            .erased_ast_id(name_node)
            .ok_or(ConstEvalError::Unresolved)?;

        let resolve = base_resolve_index(db, source_file, unit);
        let resolution = resolve
            .resolutions
            .get(&name_ast_id)
            .ok_or(ConstEvalError::Unresolved)?;

        let sym = match &resolution.target {
            lyra_semantic::resolve_index::ResolvedTarget::Symbol(s) => s,
            lyra_semantic::resolve_index::ResolvedTarget::EnumVariant(_) => {
                return Err(ConstEvalError::NonConstant);
            }
        };
        let target_file_id = sym.file;
        let target_local = sym.local;
        let target_file =
            source_file_by_id(db, unit, target_file_id).ok_or(ConstEvalError::Unresolved)?;
        let target_def = def_index_file(db, target_file);
        let target_sym = target_def.symbols.get(target_local);

        if target_sym.kind != lyra_semantic::symbols::SymbolKind::Parameter {
            return Err(ConstEvalError::NonConstant);
        }

        let decl_ast_id = target_sym.name_ast;

        let init_ast_id = target_def
            .name_ast_to_init_expr
            .get(&decl_ast_id)
            .ok_or(ConstEvalError::Unresolved)?
            .ok_or(ConstEvalError::Unresolved)?;

        let init_ref = ConstExprRef::new(db, unit, init_ast_id);
        match eval_const_int(db, init_ref) {
            ConstInt::Known(v) => Ok(v),
            ConstInt::Error(e) => Err(e),
            ConstInt::Unevaluated(_) => Err(ConstEvalError::Unsupported),
        }
    };

    let eval_call = |call_node: &lyra_parser::SyntaxNode| -> Option<Result<i64, ConstEvalError>> {
        let view = SystemCallView::try_from_node(call_node)?;
        let intrinsic = ConstIntrinsic::from_name(view.name())?;
        let result = match intrinsic {
            ConstIntrinsic::Bits => eval_bits_intrinsic(db, &view, unit, source_file, map),
            ConstIntrinsic::Left
            | ConstIntrinsic::Right
            | ConstIntrinsic::Low
            | ConstIntrinsic::High
            | ConstIntrinsic::Size
            | ConstIntrinsic::Increment
            | ConstIntrinsic::Dimensions
            | ConstIntrinsic::UnpackedDimensions => {
                eval_dim_intrinsic(db, &view, unit, source_file, map, &intrinsic, &resolve_name)
            }
        };
        Some(match result {
            ConstInt::Known(v) => Ok(v),
            ConstInt::Error(e) => Err(e),
            ConstInt::Unevaluated(_) => Err(ConstEvalError::Unsupported),
        })
    };

    match lyra_semantic::const_eval::eval_const_expr_full(&node, &resolve_name, &eval_call) {
        Ok(v) => ConstInt::Known(v),
        Err(e) => ConstInt::Error(e),
    }
}

fn const_eval_recover<'db>(
    _db: &'db dyn salsa::Database,
    _cycle: &salsa::Cycle,
    _expr_ref: ConstExprRef<'db>,
) -> ConstInt {
    ConstInt::Error(ConstEvalError::Cycle)
}

/// Evaluate `$bits(arg)` at const-eval time.
///
/// Classification follows the same logic as `classify_bits_arg`:
/// - `TypeSpec` -> type-form (extract base type from typespec)
/// - `NameRef`/`QualifiedName` resolved to `TypeAlias` -> type-form
/// - Everything else -> expr-form (infer via `type_of_expr_raw`)
fn eval_bits_intrinsic(
    db: &dyn salsa::Database,
    view: &SystemCallView,
    unit: CompilationUnit,
    source_file: SourceFile,
    map: &lyra_ast::AstIdMap,
) -> ConstInt {
    if view.arg_count() != 1 {
        return ConstInt::Error(ConstEvalError::InvalidArgument);
    }
    let Some(arg) = view.nth_arg(0) else {
        return ConstInt::Error(ConstEvalError::InvalidArgument);
    };

    let ty = classify_bits_arg_for_const(db, unit, source_file, map, &arg);
    bits_from_ty(db, unit, &ty)
}

/// Classify a `$bits` argument and resolve to a `Ty`.
///
/// Type-form: `TypeSpec` or `NameRef`/`QualifiedName` that resolves to a `TypeAlias`.
/// Expr-form: everything else -- infer type via `type_of_expr_raw`.
fn classify_bits_arg_for_const(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    source_file: SourceFile,
    map: &lyra_ast::AstIdMap,
    arg: &lyra_parser::SyntaxNode,
) -> Ty {
    if arg.kind() == SyntaxKind::TypeSpec {
        // Check for user-defined type reference
        if let Some(ty) = resolve_type_from_typespec(db, unit, source_file, map, arg) {
            return ty;
        }
        return lyra_semantic::extract_base_ty_from_typespec(arg, map);
    }

    if matches!(arg.kind(), SyntaxKind::NameRef | SyntaxKind::QualifiedName)
        && let Some(ty) = resolve_as_type(db, unit, source_file, map, arg)
    {
        return ty;
    }

    // Expr-form: infer type via type_of_expr_raw
    let Some(ast_id) = map.erased_ast_id(arg) else {
        return Ty::Error;
    };
    let expr_ref = ExprRef::new(db, unit, ast_id);
    let et = type_of_expr_raw(db, expr_ref);
    et.ty
}

/// Resolve a user-defined type reference inside a `TypeSpec` for `$bits`.
fn resolve_type_from_typespec(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    source_file: SourceFile,
    map: &lyra_ast::AstIdMap,
    typespec: &lyra_parser::SyntaxNode,
) -> Option<Ty> {
    let utr = lyra_semantic::user_type_ref(typespec)?;
    let name_ast_id = map.erased_ast_id(utr.resolve_node())?;
    let resolve = base_resolve_index(db, source_file, unit);
    let res = resolve.resolutions.get(&name_ast_id)?;
    let sym_id = match &res.target {
        lyra_semantic::resolve_index::ResolvedTarget::Symbol(s) => *s,
        lyra_semantic::resolve_index::ResolvedTarget::EnumVariant(ev) => {
            return Some(Ty::Enum(ev.enum_id));
        }
    };
    let sym_ref = SymbolRef::new(db, unit, sym_id);
    let sym_type = type_of_symbol_raw(db, sym_ref);
    match sym_type {
        SymbolType::TypeAlias(ty) | SymbolType::Value(ty) => Some(ty),
        _ => None,
    }
}

/// Resolve a `NameRef`/`QualifiedName` as a type (`TypeAlias`) for `$bits`.
///
/// The pre-built resolve index only has Value-namespace entries for bare
/// `NameRef`s in expression positions. Typedefs live in the Type namespace,
/// so we do ad-hoc Type-namespace resolution when the pre-built index
/// doesn't have the name.
fn resolve_as_type(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    source_file: SourceFile,
    map: &lyra_ast::AstIdMap,
    name_node: &lyra_parser::SyntaxNode,
) -> Option<Ty> {
    // First try the pre-built resolve index (handles Value-namespace hits)
    let ast_id = map.erased_ast_id(name_node)?;
    let resolve = base_resolve_index(db, source_file, unit);
    if let Some(res) = resolve.resolutions.get(&ast_id) {
        let sym_id = match &res.target {
            lyra_semantic::resolve_index::ResolvedTarget::Symbol(s) => *s,
            lyra_semantic::resolve_index::ResolvedTarget::EnumVariant(ev) => {
                return Some(Ty::Enum(ev.enum_id));
            }
        };
        let sym_ref = SymbolRef::new(db, unit, sym_id);
        let sym_type = type_of_symbol_raw(db, sym_ref);
        if let SymbolType::TypeAlias(ty) = sym_type {
            return Some(ty);
        }
        return None;
    }

    // Not in pre-built index -- do ad-hoc Type-namespace resolution
    resolve_name_as_type(db, unit, source_file, map, name_node)
}

/// Ad-hoc Type-namespace resolution for a `NameRef` or `QualifiedName`.
///
/// Used when the pre-built resolve index (which only has Value-namespace
/// entries for expression-position `NameRef`s) doesn't contain the name.
fn resolve_name_as_type(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    source_file: SourceFile,
    map: &lyra_ast::AstIdMap,
    name_node: &lyra_parser::SyntaxNode,
) -> Option<Ty> {
    let expected = lyra_semantic::def_index::ExpectedNs::Exact(Namespace::Type);
    let global = global_def_index(db, unit);
    let pkg_scope = package_scope_index(db, unit);

    let result = if let Some(nr) = NameRef::cast(name_node.clone()) {
        let ident = nr.ident()?;
        let name = ident.text();
        let def = def_index_file(db, source_file);
        let scope = find_use_site_scope(def, name_node, map)?;
        let graph = name_graph_file(db, source_file);
        let cu_env = compilation_unit_env(db, unit);
        lyra_semantic::resolve_name_in_scope(
            graph, global, pkg_scope, cu_env, scope, name, expected,
        )
    } else if let Some(qn) = QualifiedName::cast(name_node.clone()) {
        let segments: Vec<SmolStr> = qn.segments().map(|t| SmolStr::new(t.text())).collect();
        if segments.len() < 2 {
            return None;
        }
        lyra_semantic::resolve_qualified_name(&segments, global, pkg_scope, expected)
    } else {
        return None;
    };

    core_resolution_to_ty(db, unit, source_file, &result)
}

/// Find the scope for a node by looking up its `use_site` in the def index.
fn find_use_site_scope(
    def: &lyra_semantic::def_index::DefIndex,
    node: &lyra_parser::SyntaxNode,
    map: &lyra_ast::AstIdMap,
) -> Option<lyra_semantic::scopes::ScopeId> {
    let ast_id = map.erased_ast_id(node)?;
    def.use_sites
        .iter()
        .find(|us| us.name_ref_ast == ast_id)
        .map(|us| us.scope)
}

/// Convert a [`CoreResolveResult`] to a [`Ty`] via `type_of_symbol_raw`.
fn core_resolution_to_ty(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    source_file: SourceFile,
    result: &CoreResolveResult,
) -> Option<Ty> {
    match result {
        CoreResolveResult::Resolved(CoreResolution::Local { symbol, .. }) => {
            let def = def_index_file(db, source_file);
            let sym_id = GlobalSymbolId {
                file: def.file,
                local: *symbol,
            };
            let sym_ref = SymbolRef::new(db, unit, sym_id);
            match type_of_symbol_raw(db, sym_ref) {
                SymbolType::TypeAlias(ty) => Some(ty),
                _ => None,
            }
        }
        CoreResolveResult::Resolved(CoreResolution::Def { def }) => {
            let sym_id = crate::semantic::symbol_at_name_ast(db, unit, def.ast_id())?;
            let sym_ref = SymbolRef::new(db, unit, sym_id);
            match type_of_symbol_raw(db, sym_ref) {
                SymbolType::TypeAlias(ty) => Some(ty),
                _ => None,
            }
        }
        CoreResolveResult::Resolved(CoreResolution::Pkg { name_ast, .. }) => {
            let sym_id = crate::semantic::symbol_at_name_ast(db, unit, *name_ast)?;
            let sym_ref = SymbolRef::new(db, unit, sym_id);
            match type_of_symbol_raw(db, sym_ref) {
                SymbolType::TypeAlias(ty) => Some(ty),
                _ => None,
            }
        }
        CoreResolveResult::Resolved(CoreResolution::EnumVariant(target)) => {
            Some(Ty::Enum(target.enum_id))
        }
        CoreResolveResult::Unresolved(_) => None,
    }
}

/// Evaluate an array query intrinsic ($left, $right, etc.) at const-eval time.
fn eval_dim_intrinsic(
    db: &dyn salsa::Database,
    view: &SystemCallView,
    unit: CompilationUnit,
    source_file: SourceFile,
    map: &lyra_ast::AstIdMap,
    intrinsic: &ConstIntrinsic,
    resolve_name: &dyn Fn(&lyra_parser::SyntaxNode) -> Result<i64, ConstEvalError>,
) -> ConstInt {
    let arg_count = view.arg_count();
    let is_counting = matches!(
        intrinsic,
        ConstIntrinsic::Dimensions | ConstIntrinsic::UnpackedDimensions
    );

    if arg_count < 1 || (is_counting && arg_count > 1) || (!is_counting && arg_count > 2) {
        return ConstInt::Error(ConstEvalError::InvalidArgument);
    }

    let Some(first_arg) = view.nth_arg(0) else {
        return ConstInt::Error(ConstEvalError::InvalidArgument);
    };

    let ty = classify_bits_arg_for_const(db, unit, source_file, map, &first_arg);
    let normalized = normalize_for_dim(db, unit, &ty);

    if matches!(normalized, Ty::Error) {
        return ConstInt::Error(ConstEvalError::Unsupported);
    }

    if is_counting {
        let (unpacked, packed) = crate::dim_model::dim_counts(&normalized);
        return match intrinsic {
            ConstIntrinsic::Dimensions => ConstInt::Known(i64::from(unpacked + packed)),
            ConstIntrinsic::UnpackedDimensions => ConstInt::Known(i64::from(unpacked)),
            _ => ConstInt::Error(ConstEvalError::Unsupported),
        };
    }

    // Per-dimension queries: resolve optional second arg (default 1)
    let dim_num = if arg_count >= 2 {
        let Some(dim_arg) = view.nth_arg(1) else {
            return ConstInt::Error(ConstEvalError::InvalidArgument);
        };
        match lyra_semantic::const_eval::eval_const_expr_full(&dim_arg, resolve_name, &|_| None) {
            Ok(v) => match u32::try_from(v) {
                Ok(0) | Err(_) => {
                    return ConstInt::Error(ConstEvalError::InvalidArgument);
                }
                Ok(n) => n,
            },
            Err(e) => return ConstInt::Error(e),
        }
    } else {
        1
    };

    let Some(shape) = crate::dim_model::select_dim(&normalized, dim_num) else {
        return ConstInt::Error(ConstEvalError::InvalidArgument);
    };

    match intrinsic {
        ConstIntrinsic::Left => ConstInt::Known(shape.left()),
        ConstIntrinsic::Right => match shape.right() {
            Some(v) => ConstInt::Known(v),
            None => ConstInt::Error(ConstEvalError::NonConstant),
        },
        ConstIntrinsic::Low => match shape.low() {
            Some(v) => ConstInt::Known(v),
            None => ConstInt::Error(ConstEvalError::NonConstant),
        },
        ConstIntrinsic::High => match shape.high() {
            Some(v) => ConstInt::Known(v),
            None => ConstInt::Error(ConstEvalError::NonConstant),
        },
        ConstIntrinsic::Size => match shape.size() {
            Some(v) => ConstInt::Known(v),
            None => ConstInt::Error(ConstEvalError::NonConstant),
        },
        ConstIntrinsic::Increment => ConstInt::Known(shape.increment()),
        _ => ConstInt::Error(ConstEvalError::Unsupported),
    }
}

/// Normalize a `Ty` for dimension queries (resolve unevaluated const ints).
fn normalize_for_dim(db: &dyn salsa::Database, unit: CompilationUnit, ty: &Ty) -> Ty {
    let eval = |expr_ast_id: lyra_ast::ErasedAstId| -> ConstInt {
        let expr_ref = ConstExprRef::new(db, unit, expr_ast_id);
        eval_const_int(db, expr_ref)
    };
    lyra_semantic::normalize_ty(ty, &eval)
}

/// Convert a [`Ty`] to a bit width via `normalize_ty` + `bit_width_total`.
fn bits_from_ty(db: &dyn salsa::Database, unit: CompilationUnit, ty: &Ty) -> ConstInt {
    let eval = |expr_ast_id: lyra_ast::ErasedAstId| -> ConstInt {
        let expr_ref = ConstExprRef::new(db, unit, expr_ast_id);
        eval_const_int(db, expr_ref)
    };
    let normalized = lyra_semantic::normalize_ty(ty, &eval);
    let ty_ref = TyRef::new(db, normalized);
    match bit_width_total(db, unit, ty_ref) {
        Some(w) => ConstInt::Known(i64::from(w)),
        None => ConstInt::Error(ConstEvalError::Unsupported),
    }
}
