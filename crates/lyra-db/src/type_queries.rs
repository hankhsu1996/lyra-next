use lyra_lexer::SyntaxKind;
use lyra_semantic::aggregate::TypeOrigin;
use lyra_semantic::symbols::GlobalSymbolId;
use lyra_semantic::types::{ConstEvalError, ConstInt, Ty, UnpackedDim};

use crate::const_eval::{ConstExprRef, eval_const_int};
use crate::pipeline::{ast_id_map, parse_file};
use crate::semantic::{def_index_file, resolve_index_file};
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
    use lyra_semantic::{extract_type_from_container, typespec_name_ref};

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

    // Check TypeOrigin for enum/struct types
    match sym.type_origin {
        TypeOrigin::Enum(idx) => {
            let id = def.enum_id(idx);
            let ty = Ty::Enum(id);
            let parse = parse_file(db, source_file);
            let map = ast_id_map(db, source_file);
            let decl_ast_id = def.symbol_to_decl.get(gsym.local.index()).and_then(|o| *o);
            let decl_node = decl_ast_id.and_then(|id| map.get_node(&parse.syntax(), id));
            let ty = wrap_unpacked_dims_from_node(ty, decl_node.as_ref(), map);
            return classify(ty, sym.kind);
        }
        TypeOrigin::Struct(idx) => {
            let id = def.struct_id(idx);
            let ty = Ty::Struct(id);
            let parse = parse_file(db, source_file);
            let map = ast_id_map(db, source_file);
            let decl_ast_id = def.symbol_to_decl.get(gsym.local.index()).and_then(|o| *o);
            let decl_node = decl_ast_id.and_then(|id| map.get_node(&parse.syntax(), id));
            let ty = wrap_unpacked_dims_from_node(ty, decl_node.as_ref(), map);
            return classify(ty, sym.kind);
        }
        TypeOrigin::TypeSpec => {}
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
        && typespec_name_ref(ts).is_some()
    {
        let dim_source = if decl_node.kind() == SyntaxKind::Port {
            Some(&decl_node)
        } else {
            declarator.as_ref()
        };
        return expand_typedef(db, unit, source_file, ts, dim_source, map, sym.kind);
    }

    // No user-defined type -- pure extraction
    extract_type_from_container(&container, declarator.as_ref(), map)
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
    typespec: &lyra_parser::SyntaxNode,
    dim_source: Option<&lyra_parser::SyntaxNode>,
    id_map: &lyra_ast::AstIdMap,
    caller_kind: lyra_semantic::symbols::SymbolKind,
) -> lyra_semantic::types::SymbolType {
    use lyra_semantic::types::{SymbolType, SymbolTypeError, UnpackedDim};
    use lyra_semantic::typespec_name_ref;

    let Some(name_node) = typespec_name_ref(typespec) else {
        return SymbolType::Error(SymbolTypeError::UserTypeUnresolved);
    };

    // Look up the name in the resolve index
    let resolve = resolve_index_file(db, source_file, unit);
    let Some(name_ast_id) = id_map.erased_ast_id(&name_node) else {
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

    if target_info.kind != lyra_semantic::symbols::SymbolKind::Typedef {
        return SymbolType::Error(SymbolTypeError::UserTypeUnresolved);
    }

    // Recursively get the typedef's type (Salsa cycle detection)
    let typedef_ref = SymbolRef::new(db, unit, target_id);
    let typedef_type = type_of_symbol_raw(db, typedef_ref);

    // Extract the underlying Ty from the typedef result
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

    // Wrap the result in the correct classification
    let wrap = if caller_kind == lyra_semantic::symbols::SymbolKind::Typedef {
        SymbolType::TypeAlias
    } else {
        SymbolType::Value
    };

    wrap(lyra_semantic::wrap_unpacked(
        underlying_ty,
        &use_site_unpacked,
    ))
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
