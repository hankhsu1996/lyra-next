use crate::Site;
use lyra_source::FileId;

use crate::def_index::DefIndex;
use crate::global_index::{DefinitionKind, GlobalDefIndex};
use crate::symbols::GlobalDefId;

/// Typed identity for an interface definition.
///
/// Field is private. Construction requires proving the `GlobalDefId`
/// refers to an interface (via `DefinitionKind` check).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InterfaceDefId(GlobalDefId);

impl InterfaceDefId {
    /// Construct from a `DefIndex` (per-file, used after symbol table freeze).
    pub fn try_from_def_index(def_index: &DefIndex, def: GlobalDefId) -> Option<Self> {
        let entry = def_index.def_entry(def)?;
        match entry.kind {
            DefinitionKind::Interface => Some(Self(def)),
            _ => None,
        }
    }

    /// Construct from a `GlobalDefIndex` (cross-file, used by query layer).
    pub fn try_from_global_index(global: &GlobalDefIndex, def: GlobalDefId) -> Option<Self> {
        match global.def_kind(def) {
            Some(DefinitionKind::Interface) => Some(Self(def)),
            _ => None,
        }
    }

    /// Unwrap to the underlying `GlobalDefId`.
    pub fn global_def(self) -> GlobalDefId {
        self.0
    }

    /// Builder-internal placeholder. Replaced during finalization.
    pub(crate) fn placeholder() -> Self {
        Self(GlobalDefId::new(Site::placeholder(FileId(u32::MAX))))
    }
}
