use lyra_source::DeclSpan;
use smol_str::SmolStr;

use lyra_source::FileId;

use crate::Site;
use crate::global_index::DefinitionKind;
use crate::scopes::ScopeId;
use crate::symbols::GlobalDefId;

/// A definition-namespace entry (module, package, interface, program, primitive, config).
///
/// First-class storage keyed by `GlobalDefId`. Definition-namespace items do NOT
/// live in the `SymbolTable` -- they are stored in `DefIndex.def_entries` instead.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DefEntry {
    pub kind: DefinitionKind,

    /// Canonical spelling captured in builder for indexing / name-based queries.
    /// Invariant: must match the token text at `name_span`.
    pub name: SmolStr,

    /// Anchor for the whole declaration node (`module_decl` / `package_decl` / ...).
    pub decl_site: Site,

    /// Anchor for the name-introducing node (identifier site).
    /// For definition-namespace items, `name_site == decl_site`.
    pub name_site: Site,

    /// Precise identifier token span for diagnostics / highlights.
    pub name_span: DeclSpan,

    /// Scope owned by the definition, if any.
    pub scope: DefScope,
}

/// Whether a definition owns a scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DefScope {
    Owned(ScopeId),
    None,
}

/// Builder for accumulating `DefEntry` items before freezing.
///
/// Canonical allocator for file-local definition IDs. Each `push`
/// assigns the next ordinal, producing `GlobalDefId(file, ordinal)`.
/// `GlobalDefId.ordinal()` indexes the resulting `DefEntry` sequence.
pub(crate) struct DefEntryBuilder {
    file: FileId,
    entries: Vec<DefEntry>,
}

impl DefEntryBuilder {
    pub(crate) fn new(file: FileId) -> Self {
        Self {
            file,
            entries: Vec::new(),
        }
    }

    pub(crate) fn push(&mut self, entry: DefEntry) -> GlobalDefId {
        let ordinal = self.entries.len() as u32;
        self.entries.push(entry);
        GlobalDefId::new(self.file, ordinal)
    }

    pub(crate) fn freeze(self) -> Box<[DefEntry]> {
        self.entries.into_boxed_slice()
    }
}
