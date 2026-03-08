use lyra_semantic::Site;
use lyra_semantic::def_entry;
use lyra_semantic::diagnostic::{DiagSpan, SemanticDiag, SemanticDiagKind};
use lyra_semantic::modport_def::ModportDefId;
use lyra_semantic::modport_def::ModportTarget;
use lyra_semantic::symbols::Namespace;
use lyra_semantic::types::{ModportTfViewEntry, ModportView, ModportViewEntry, ModportViewTarget};
use lyra_source::DeclSpan;
use smol_str::SmolStr;

use crate::semantic::def_index_file;
use crate::{CompilationUnit, source_file_by_id};

/// Resolved modport: the member view plus diagnostics.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModportSem {
    pub view: ModportView,
    pub diags: Box<[SemanticDiag]>,
}

/// Identifies a modport for semantic resolution.
#[salsa::interned]
pub struct ModportRef<'db> {
    pub unit: CompilationUnit,
    pub modport_id: ModportDefId,
}

/// Resolve a modport's member entries against the interface scope (Salsa-tracked).
#[salsa::tracked]
pub fn modport_sem<'db>(db: &'db dyn salsa::Database, mref: ModportRef<'db>) -> ModportSem {
    let unit = mref.unit(db);
    let modport_id = mref.modport_id(db);

    let iface_def_id = modport_id.owner.global_def();
    let file_id = iface_def_id.ast_id().file();
    let Some(src) = source_file_by_id(db, unit, file_id) else {
        return empty_modport_sem();
    };
    let def = def_index_file(db, src);

    let Some(modport_def) = def.modport_def(modport_id) else {
        return empty_modport_sem();
    };

    let Some(entry) = def.def_entry(iface_def_id) else {
        return empty_modport_sem();
    };
    let def_entry::DefScope::Owned(iface_scope) = entry.scope else {
        return empty_modport_sem();
    };

    let mut entries = Vec::new();
    let mut diags = Vec::new();
    let mut seen_signal_ports: std::collections::HashMap<SmolStr, (Site, DeclSpan)> =
        std::collections::HashMap::new();

    for entry in &*modport_def.entries {
        let entry_primary = DiagSpan::Site(entry.port_id);
        let entry_label = DiagSpan::Decl(entry.name_span);
        if let Some(&(prev_site, prev_name_span)) = seen_signal_ports.get(&entry.port_name) {
            diags.push(SemanticDiag {
                kind: SemanticDiagKind::DuplicateDefinition {
                    name: entry.port_name.clone(),
                    original_primary: DiagSpan::Site(prev_site),
                    original_label: Some(DiagSpan::Decl(prev_name_span)),
                },
                primary: entry_primary,
                label: Some(entry_label),
            });
            continue;
        }
        seen_signal_ports.insert(entry.port_name.clone(), (entry.port_id, entry.name_span));

        let target = match &entry.target {
            ModportTarget::ImplicitMember { member_name } => {
                let resolved =
                    def.scopes
                        .resolve(&def.symbols, iface_scope, Namespace::Value, member_name);
                let Some(member_sym) = resolved else {
                    diags.push(SemanticDiag {
                        kind: SemanticDiagKind::UnresolvedName {
                            name: member_name.clone(),
                        },
                        primary: entry_primary,
                        label: Some(entry_label),
                    });
                    continue;
                };
                ModportViewTarget::Member(member_sym)
            }
            ModportTarget::Expr(expr_id) => ModportViewTarget::Expr(*expr_id),
            ModportTarget::Empty => ModportViewTarget::Empty,
        };

        entries.push(ModportViewEntry {
            port_name: entry.port_name.clone(),
            direction: entry.direction,
            port_id: entry.port_id,
            target,
        });
    }

    let tf_view_entries =
        resolve_modport_tf_entries(&modport_def.tf_entries, def, iface_scope, &mut diags);

    ModportSem {
        view: ModportView::new(entries, tf_view_entries),
        diags: diags.into_boxed_slice(),
    }
}

fn resolve_modport_tf_entries(
    tf_entries: &[lyra_semantic::modport_def::ModportTfEntry],
    def: &lyra_semantic::def_index::DefIndex,
    iface_scope: lyra_semantic::scopes::ScopeId,
    diags: &mut Vec<SemanticDiag>,
) -> Vec<ModportTfViewEntry> {
    let mut result = Vec::new();
    let mut seen_tf_ports: std::collections::HashMap<SmolStr, (Site, DeclSpan)> =
        std::collections::HashMap::new();
    for tf in tf_entries {
        let tf_primary = DiagSpan::Site(tf.port_site);
        let tf_label = DiagSpan::Decl(tf.name_span);
        if let Some(&(prev_site, prev_name_span)) = seen_tf_ports.get(&tf.name) {
            diags.push(SemanticDiag {
                kind: SemanticDiagKind::DuplicateDefinition {
                    name: tf.name.clone(),
                    original_primary: DiagSpan::Site(prev_site),
                    original_label: Some(DiagSpan::Decl(prev_name_span)),
                },
                primary: tf_primary,
                label: Some(tf_label),
            });
            continue;
        }
        seen_tf_ports.insert(tf.name.clone(), (tf.port_site, tf.name_span));

        let Some(sym_id) =
            def.scopes
                .resolve(&def.symbols, iface_scope, Namespace::Value, &tf.name)
        else {
            diags.push(SemanticDiag {
                kind: SemanticDiagKind::UnresolvedName {
                    name: tf.name.clone(),
                },
                primary: tf_primary,
                label: Some(tf_label),
            });
            continue;
        };

        let sym = def.symbols.get(sym_id);
        if !matches!(
            sym.kind,
            lyra_semantic::symbols::SymbolKind::Function | lyra_semantic::symbols::SymbolKind::Task
        ) {
            diags.push(SemanticDiag {
                kind: SemanticDiagKind::NotASubroutine {
                    name: tf.name.clone(),
                },
                primary: tf_primary,
                label: Some(tf_label),
            });
            continue;
        }

        result.push(ModportTfViewEntry {
            name: tf.name.clone(),
            port_site: tf.port_site,
            target: sym_id,
        });
    }
    result
}

fn empty_modport_sem() -> ModportSem {
    ModportSem {
        view: ModportView::new(Vec::new(), Vec::new()),
        diags: Box::new([]),
    }
}
