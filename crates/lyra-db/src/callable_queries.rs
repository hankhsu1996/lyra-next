use std::sync::Arc;

use lyra_ast::{AstNode, FunctionDecl, TaskDecl};
use lyra_lexer::SyntaxKind;
use lyra_semantic::UserTypeRef;
use lyra_semantic::record::InterfaceDefId;
use lyra_semantic::symbols::{GlobalSymbolId, SymbolKind};
use lyra_semantic::types::{InterfaceType, SymbolType, Ty};
use smol_str::SmolStr;

use crate::module_sig::{CallableKind, CallableSig, PortDirection, TfPortSig};
use crate::pipeline::{ast_id_map, parse_file};
use crate::semantic::{def_index_file, global_def_index, resolve_index_file};
use crate::type_queries::{SymbolRef, type_of_symbol};
use crate::{CompilationUnit, source_file_by_id};

/// Identifies a callable (function/task) for signature extraction.
#[salsa::interned]
pub struct CallableRef<'db> {
    pub unit: CompilationUnit,
    pub symbol: GlobalSymbolId,
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

    let ctx = SigCtx {
        db,
        unit,
        source_file,
        id_map: map,
    };

    match kind {
        CallableKind::Function => extract_function_sig(&ctx, &decl_node, &sym.name),
        CallableKind::Task => extract_task_sig(&ctx, &decl_node, &sym.name),
    }
}

/// Context for signature extraction -- provides DB access for typedef resolution.
struct SigCtx<'a> {
    db: &'a dyn salsa::Database,
    unit: CompilationUnit,
    source_file: crate::SourceFile,
    id_map: &'a lyra_ast::AstIdMap,
}

impl SigCtx<'_> {
    /// Resolve a `TypeSpec` to a `Ty`. Handles both keyword types and user-defined
    /// types (typedefs, enums, structs) by going through the DB resolve path.
    fn resolve_typespec_ty(&self, ts: &lyra_parser::SyntaxNode) -> Ty {
        // Check for user-defined type (NameRef/QualifiedName/DottedName inside TypeSpec)
        let Some(utr) = lyra_semantic::user_type_ref(ts) else {
            // Keyword-based type
            return lyra_semantic::extract_base_ty_from_typespec(ts, self.id_map);
        };
        let Some(name_ast_id) = self.id_map.erased_ast_id(utr.resolve_node()) else {
            return Ty::Error;
        };
        let resolve = resolve_index_file(self.db, self.source_file, self.unit);
        let Some(res) = resolve.resolutions.get(&name_ast_id) else {
            return Ty::Error;
        };
        let target = res.symbol;
        let Some(target_file) = source_file_by_id(self.db, self.unit, target.file) else {
            return Ty::Error;
        };
        let target_def = def_index_file(self.db, target_file);
        let target_info = target_def.symbols.get(target.local);

        if target_info.kind == SymbolKind::Interface {
            return self.resolve_interface_ty(&utr, target_def, target.local);
        }

        let sym_ref = SymbolRef::new(self.db, self.unit, target);
        let sym_type = type_of_symbol(self.db, sym_ref);
        match sym_type {
            SymbolType::TypeAlias(ty) | SymbolType::Value(ty) => ty,
            _ => Ty::Error,
        }
    }

    fn resolve_interface_ty(
        &self,
        utr: &UserTypeRef,
        target_def: &lyra_semantic::def_index::DefIndex,
        local_sym: lyra_semantic::symbols::SymbolId,
    ) -> Ty {
        let Some(def_id) = target_def.symbol_global_def(local_sym) else {
            return Ty::Error;
        };
        let global = global_def_index(self.db, self.unit);
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
}

/// Implicit return type for functions without explicit type (LRM 13.4.1): logic (1-bit unsigned).
fn implicit_return_ty() -> Ty {
    Ty::simple_logic()
}

fn extract_function_sig(
    ctx: &SigCtx<'_>,
    node: &lyra_parser::SyntaxNode,
    name: &SmolStr,
) -> Arc<CallableSig> {
    let Some(func) = FunctionDecl::cast(node.clone()) else {
        return CallableSig::new(name.clone(), CallableKind::Function, Ty::Error, vec![]);
    };

    // Extract return type from TypeSpec child (if present; implicit = logic)
    let return_ty = func.type_spec().map_or_else(implicit_return_ty, |ts| {
        ctx.resolve_typespec_ty(ts.syntax())
    });

    let ports = extract_tf_ports(ctx, func.syntax());
    CallableSig::new(name.clone(), CallableKind::Function, return_ty, ports)
}

fn extract_task_sig(
    ctx: &SigCtx<'_>,
    node: &lyra_parser::SyntaxNode,
    name: &SmolStr,
) -> Arc<CallableSig> {
    let Some(_task) = TaskDecl::cast(node.clone()) else {
        return CallableSig::new(name.clone(), CallableKind::Task, Ty::Void, vec![]);
    };

    let ports = extract_tf_ports(ctx, node);
    CallableSig::new(name.clone(), CallableKind::Task, Ty::Void, ports)
}

fn extract_tf_ports(ctx: &SigCtx<'_>, decl_node: &lyra_parser::SyntaxNode) -> Vec<TfPortSig> {
    let mut ports = Vec::new();
    let mut current_dir = PortDirection::Input;

    for child in decl_node.children() {
        if child.kind() != SyntaxKind::TfPortDecl {
            continue;
        }

        // Check for explicit direction
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

        // Extract base type from TypeSpec (handles both keywords and user-defined types).
        // Implicit type when omitted: logic (1-bit unsigned), per LRM 13.3/13.4.
        let base_ty = child
            .children()
            .find(|c| c.kind() == SyntaxKind::TypeSpec)
            .map_or_else(Ty::simple_logic, |ts| ctx.resolve_typespec_ty(&ts));

        // Extract each declarator as a port
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

            // Check for default value (= expr)
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
