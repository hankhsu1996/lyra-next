use lyra_semantic::symbols::GlobalDefId;
use lyra_source::{FileId, Span, TextRange};
use smol_str::SmolStr;
use std::collections::HashMap;

/// Instance identity keyed by the instance name token's text range.
/// Each instance name in the source has a unique offset, even within
/// multi-instance statements like `leaf u1(...), u2(...);`.
#[derive(Clone, Copy, Hash, Eq, PartialEq, Debug)]
pub(crate) struct InstanceKey {
    pub(crate) file: FileId,
    pub(crate) name_range: TextRange,
}

/// A node in the elaboration instance tree.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct InstanceNode {
    pub(crate) key: InstanceKey,
    pub(crate) parent: Option<InstanceKey>,
    pub(crate) module_def: GlobalDefId,
    pub(crate) instance_name: SmolStr,
    pub(crate) children: Vec<InstanceKey>,
}

/// The elaborated instance tree rooted at a top module.
///
/// The top module is a node in `nodes` with `parent: None`.
/// Children are stored in source order of instantiation appearance.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ElabTree {
    pub(crate) top: Option<InstanceKey>,
    pub(crate) nodes: HashMap<InstanceKey, InstanceNode>,
    pub(crate) diagnostics: Vec<ElabDiag>,
}

/// Elaboration-specific diagnostic, before lowering to `lyra_diag::Diagnostic`.
///
/// Stores `Span` (file + range) so diagnostics map to the correct file
/// in multi-file compilation units.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ElabDiag {
    UnresolvedModuleInst {
        name: SmolStr,
        span: Span,
    },
    NotAModule {
        name: SmolStr,
        span: Span,
    },
    UnknownPort {
        port: SmolStr,
        module: SmolStr,
        span: Span,
    },
    DuplicatePortConn {
        port: SmolStr,
        span: Span,
    },
    TooManyPositionalPorts {
        expected: usize,
        got: usize,
        span: Span,
    },
    MissingPortConn {
        port: SmolStr,
        module: SmolStr,
        span: Span,
    },
    RecursionLimit {
        span: Span,
    },
}
