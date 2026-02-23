use std::sync::Arc;

use lyra_ast::{AstNode, FunctionDecl, TaskDecl};
use lyra_lexer::SyntaxKind;
use lyra_semantic::UserTypeRef;
use lyra_semantic::interface_id::InterfaceDefId;
use lyra_semantic::symbols::{GlobalSymbolId, SymbolKind};
use lyra_semantic::types::{InterfaceType, SymbolType, Ty};
use smol_str::SmolStr;

use crate::module_sig::{CallableKind, CallableSig, PortDirection, TfPortSig};
use crate::pipeline::{ast_id_map, parse_file};
use crate::semantic::{base_resolve_index, def_index_file, global_def_index, resolve_index_file};
use crate::type_queries::{SymbolRef, type_of_symbol, type_of_symbol_raw};
use crate::{CompilationUnit, source_file_by_id};

/// Identifies a callable (function/task) for signature extraction.
#[salsa::interned]
pub struct CallableRef<'db> {
    pub unit: CompilationUnit,
    pub symbol: GlobalSymbolId,
}

/// Resolve a `TypeSpec` to a [`Ty`] via a resolve index and symbol-type query.
///
/// Shared between normal and raw signature extraction paths. The caller
/// provides the resolve index and type-of-symbol function appropriate
/// for their context (normal uses `resolve_index_file` + `type_of_symbol`,
/// raw uses `base_resolve_index` + `type_of_symbol_raw`).
fn resolve_typespec_ty_with(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    id_map: &lyra_ast::AstIdMap,
    ts: &lyra_parser::SyntaxNode,
    resolve: &lyra_semantic::resolve_index::ResolveIndex,
    type_of_sym: &dyn Fn(&dyn salsa::Database, CompilationUnit, GlobalSymbolId) -> SymbolType,
) -> Ty {
    let Some(utr) = lyra_semantic::user_type_ref(ts) else {
        return lyra_semantic::extract_base_ty_from_typespec(ts, id_map);
    };
    let Some(name_ast_id) = id_map.erased_ast_id(utr.resolve_node()) else {
        return Ty::Error;
    };
    let Some(res) = resolve.resolutions.get(&name_ast_id) else {
        return Ty::Error;
    };
    let target = match &res.target {
        lyra_semantic::resolve_index::ResolvedTarget::Symbol(s) => *s,
        lyra_semantic::resolve_index::ResolvedTarget::EnumVariant(ev) => {
            return Ty::Enum(ev.enum_id);
        }
    };
    let Some(target_file) = source_file_by_id(db, unit, target.file) else {
        return Ty::Error;
    };
    let target_def = def_index_file(db, target_file);
    let target_info = target_def.symbols.get(target.local);

    if target_info.kind == SymbolKind::Interface {
        return resolve_interface_ty(db, unit, &utr, target_def, target.local);
    }

    let sym_type = type_of_sym(db, unit, target);
    match sym_type {
        SymbolType::TypeAlias(ty) | SymbolType::Value(ty) => ty,
        _ => Ty::Error,
    }
}

fn resolve_interface_ty(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    utr: &UserTypeRef,
    target_def: &lyra_semantic::def_index::DefIndex,
    local_sym: lyra_semantic::symbols::SymbolId,
) -> Ty {
    let Some(def_id) = target_def.symbol_global_def(local_sym) else {
        return Ty::Error;
    };
    let global = global_def_index(db, unit);
    let Some(iface_def) = InterfaceDefId::try_from_global_index(global, def_id) else {
        return Ty::Error;
    };
    let modport = match utr {
        UserTypeRef::InterfaceModport { modport_name, .. } => {
            match target_def.modport_by_name(iface_def, modport_name.as_str()) {
                Some(mp_def) => Some(mp_def.id),
                None => return Ty::Error,
            }
        }
        _ => None,
    };
    Ty::Interface(InterfaceType {
        iface: iface_def,
        modport,
    })
}

/// Extract the signature of a callable (Salsa-tracked).
///
/// Returns `Arc<CallableSig>` for cheap sharing. Cached by Salsa.
#[salsa::tracked]
pub fn callable_signature<'db>(
    db: &'db dyn salsa::Database,
    callable_ref: CallableRef<'db>,
) -> Arc<CallableSig> {
    let unit = callable_ref.unit(db);
    let gsym = callable_ref.symbol(db);
    let resolve_ty = |ts: &lyra_parser::SyntaxNode,
                      source_file: crate::SourceFile,
                      id_map: &lyra_ast::AstIdMap|
     -> Ty {
        let resolve = resolve_index_file(db, source_file, unit);
        resolve_typespec_ty_with(db, unit, id_map, ts, resolve, &normal_type_of_sym)
    };
    extract_callable_sig_impl(db, unit, gsym, &resolve_ty)
}

/// Extract a callable signature using raw (const-eval-safe) resolution.
///
/// Uses `base_resolve_index` + `type_of_symbol_raw` instead of
/// `resolve_index_file` + `type_of_symbol`. Not a Salsa query --
/// called internally by `DbInferCtxRaw`.
pub(crate) fn callable_signature_raw(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    gsym: GlobalSymbolId,
) -> Arc<CallableSig> {
    let resolve_ty = |ts: &lyra_parser::SyntaxNode,
                      source_file: crate::SourceFile,
                      id_map: &lyra_ast::AstIdMap|
     -> Ty {
        let resolve = base_resolve_index(db, source_file, unit);
        resolve_typespec_ty_with(db, unit, id_map, ts, resolve, &raw_type_of_sym)
    };
    extract_callable_sig_impl(db, unit, gsym, &resolve_ty)
}

fn normal_type_of_sym(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    target: GlobalSymbolId,
) -> SymbolType {
    let sym_ref = SymbolRef::new(db, unit, target);
    type_of_symbol(db, sym_ref)
}

fn raw_type_of_sym(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    target: GlobalSymbolId,
) -> SymbolType {
    let sym_ref = SymbolRef::new(db, unit, target);
    type_of_symbol_raw(db, sym_ref)
}

/// Shared implementation for callable signature extraction.
fn extract_callable_sig_impl(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    gsym: GlobalSymbolId,
    resolve_ty: &dyn Fn(&lyra_parser::SyntaxNode, crate::SourceFile, &lyra_ast::AstIdMap) -> Ty,
) -> Arc<CallableSig> {
    let Some(source_file) = source_file_by_id(db, unit, gsym.file) else {
        return CallableSig::new(
            SmolStr::default(),
            CallableKind::Function,
            Ty::Error,
            vec![],
        );
    };

    let def = def_index_file(db, source_file);
    let sym = def.symbols.get(gsym.local);

    let kind = match sym.kind {
        SymbolKind::Function => CallableKind::Function,
        SymbolKind::Task => CallableKind::Task,
        _ => {
            return CallableSig::new(sym.name.clone(), CallableKind::Function, Ty::Error, vec![]);
        }
    };

    let Some(decl_ast_id) = def.symbol_to_decl.get(gsym.local.index()).and_then(|o| *o) else {
        return CallableSig::new(sym.name.clone(), kind, Ty::Error, vec![]);
    };

    let parse = parse_file(db, source_file);
    let map = ast_id_map(db, source_file);

    let Some(decl_node) = map.get_node(&parse.syntax(), decl_ast_id) else {
        return CallableSig::new(sym.name.clone(), kind, Ty::Error, vec![]);
    };

    let typespec_resolver =
        |ts: &lyra_parser::SyntaxNode| -> Ty { resolve_ty(ts, source_file, map) };

    match kind {
        CallableKind::Function => extract_function_sig(&typespec_resolver, &decl_node, &sym.name),
        CallableKind::Task => extract_task_sig(&typespec_resolver, &decl_node, &sym.name),
    }
}

/// Implicit return type for functions without explicit type (LRM 13.4.1): logic (1-bit unsigned).
fn implicit_return_ty() -> Ty {
    Ty::simple_logic()
}

fn extract_function_sig(
    resolve_ty: &dyn Fn(&lyra_parser::SyntaxNode) -> Ty,
    node: &lyra_parser::SyntaxNode,
    name: &SmolStr,
) -> Arc<CallableSig> {
    let Some(func) = FunctionDecl::cast(node.clone()) else {
        return CallableSig::new(name.clone(), CallableKind::Function, Ty::Error, vec![]);
    };

    let return_ty = func
        .type_spec()
        .map_or_else(implicit_return_ty, |ts| resolve_ty(ts.syntax()));

    let ports = extract_tf_ports(resolve_ty, func.syntax());
    CallableSig::new(name.clone(), CallableKind::Function, return_ty, ports)
}

fn extract_task_sig(
    resolve_ty: &dyn Fn(&lyra_parser::SyntaxNode) -> Ty,
    node: &lyra_parser::SyntaxNode,
    name: &SmolStr,
) -> Arc<CallableSig> {
    let Some(_task) = TaskDecl::cast(node.clone()) else {
        return CallableSig::new(name.clone(), CallableKind::Task, Ty::Void, vec![]);
    };

    let ports = extract_tf_ports(resolve_ty, node);
    CallableSig::new(name.clone(), CallableKind::Task, Ty::Void, ports)
}

fn extract_tf_ports(
    resolve_ty: &dyn Fn(&lyra_parser::SyntaxNode) -> Ty,
    decl_node: &lyra_parser::SyntaxNode,
) -> Vec<TfPortSig> {
    let mut ports = Vec::new();
    let mut current_dir = PortDirection::Input;

    for child in decl_node.children() {
        if child.kind() != SyntaxKind::TfPortDecl {
            continue;
        }

        let has_explicit_dir = child.children_with_tokens().any(|el| {
            el.as_token().is_some_and(|tok| {
                matches!(
                    tok.kind(),
                    SyntaxKind::InputKw
                        | SyntaxKind::OutputKw
                        | SyntaxKind::InoutKw
                        | SyntaxKind::RefKw
                )
            })
        });

        if has_explicit_dir {
            current_dir = direction_from_node(&child);
        }

        let base_ty = child
            .children()
            .find(|c| c.kind() == SyntaxKind::TypeSpec)
            .map_or_else(Ty::simple_logic, |ts| resolve_ty(&ts));

        let decl_range = child.text_range();
        for decl_child in child.children() {
            if decl_child.kind() != SyntaxKind::Declarator {
                continue;
            }
            let name_tok = decl_child
                .children_with_tokens()
                .filter_map(lyra_parser::SyntaxElement::into_token)
                .find(|tok| matches!(tok.kind(), SyntaxKind::Ident | SyntaxKind::EscapedIdent));

            let Some(name_tok) = name_tok else { continue };

            let has_default = decl_child.children_with_tokens().any(|el| {
                el.as_token()
                    .is_some_and(|tok| tok.kind() == SyntaxKind::Assign)
            });

            ports.push(TfPortSig {
                name: SmolStr::new(name_tok.text()),
                direction: current_dir,
                ty: base_ty.clone(),
                has_default,
                name_range: name_tok.text_range(),
                decl_range,
            });
        }
    }

    ports
}

fn direction_from_node(node: &lyra_parser::SyntaxNode) -> PortDirection {
    for el in node.children_with_tokens() {
        if let Some(tok) = el.as_token() {
            match tok.kind() {
                SyntaxKind::InputKw => return PortDirection::Input,
                SyntaxKind::OutputKw => return PortDirection::Output,
                SyntaxKind::InoutKw => return PortDirection::Inout,
                SyntaxKind::RefKw => return PortDirection::Ref,
                _ => {}
            }
        }
    }
    PortDirection::Input
}
