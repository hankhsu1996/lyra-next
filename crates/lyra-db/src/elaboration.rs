use lyra_semantic::symbols::GlobalDefId;
use lyra_semantic::types::ConstInt;
use lyra_source::{FileId, Span, TextRange};
use smol_str::SmolStr;
use std::collections::HashMap;
use std::sync::Arc;

/// Instance identity keyed by the instance name token's text range
/// plus the enclosing generate scope (if any).
///
/// Outside generate-for, each instance name has a unique offset.
/// Inside generate-for, the same AST node is visited per iteration,
/// so `parent_gen` disambiguates via the enclosing `GenScopeKey`.
#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub(crate) struct InstanceKey {
    pub(crate) file: FileId,
    pub(crate) name_range: TextRange,
    pub(crate) parent_gen: Option<GenScopeKey>,
}

/// Identity for a generate scope (if/for/case branch).
#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub(crate) struct GenScopeKey {
    pub(crate) file: FileId,
    pub(crate) offset: TextRange,
    pub(crate) iter: Option<i64>,
}

/// An item in a scope's children list.
#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub(crate) enum ElabItemKey {
    Inst(InstanceKey),
    GenScope(GenScopeKey),
}

/// Unified key for any scope in the elaboration tree.
#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub(crate) enum ScopeKey {
    Instance(InstanceKey),
    GenScope(GenScopeKey),
}

/// Evaluated parameter environment for one module instance.
///
/// Values are aligned with `ModuleSig.params` order. O(1) by index.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ParamEnv {
    values: Box<[ConstInt]>,
}

impl ParamEnv {
    pub(crate) fn new(values: Vec<ConstInt>) -> Self {
        Self {
            values: values.into_boxed_slice(),
        }
    }

    pub(crate) fn empty() -> Arc<Self> {
        Arc::new(Self {
            values: Box::new([]),
        })
    }

    pub(crate) fn values_slice(&self) -> &[ConstInt] {
        &self.values
    }
}

/// What kind of generate scope this is.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum GenScopeKind {
    If,
    ForIteration {
        genvar_name: SmolStr,
        genvar_value: i64,
    },
    CaseItem,
}

/// A node in the elaboration instance tree.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct InstanceNode {
    pub(crate) key: InstanceKey,
    pub(crate) parent: Option<ScopeKey>,
    pub(crate) module_def: GlobalDefId,
    pub(crate) instance_name: SmolStr,
    pub(crate) param_env: Arc<ParamEnv>,
    pub(crate) children: Vec<ElabItemKey>,
}

/// A generate scope node.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct GenScopeNode {
    pub(crate) key: GenScopeKey,
    pub(crate) parent: ScopeKey,
    pub(crate) name: Option<SmolStr>,
    pub(crate) kind: GenScopeKind,
    pub(crate) children: Vec<ElabItemKey>,
}

/// The elaborated instance tree rooted at a top module.
///
/// The top module is a node in `nodes` with `parent: None`.
/// Children are stored in source order of instantiation appearance.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ElabTree {
    pub(crate) top: Option<InstanceKey>,
    pub(crate) nodes: HashMap<InstanceKey, InstanceNode>,
    pub(crate) gen_scopes: HashMap<GenScopeKey, GenScopeNode>,
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
    UnknownParam {
        name: SmolStr,
        module: SmolStr,
        span: Span,
    },
    DuplicateParamOverride {
        name: SmolStr,
        span: Span,
    },
    TooManyPositionalParams {
        expected: usize,
        got: usize,
        span: Span,
    },
    ParamNotConst {
        name: SmolStr,
        span: Span,
    },
    GenCondNotConst {
        span: Span,
    },
    GenvarNotConst {
        span: Span,
    },
}

impl ElabDiag {
    pub(crate) fn span(&self) -> &Span {
        match self {
            Self::UnresolvedModuleInst { span, .. }
            | Self::NotAModule { span, .. }
            | Self::UnknownPort { span, .. }
            | Self::DuplicatePortConn { span, .. }
            | Self::TooManyPositionalPorts { span, .. }
            | Self::MissingPortConn { span, .. }
            | Self::RecursionLimit { span }
            | Self::UnknownParam { span, .. }
            | Self::DuplicateParamOverride { span, .. }
            | Self::TooManyPositionalParams { span, .. }
            | Self::ParamNotConst { span, .. }
            | Self::GenCondNotConst { span }
            | Self::GenvarNotConst { span } => span,
        }
    }
}
