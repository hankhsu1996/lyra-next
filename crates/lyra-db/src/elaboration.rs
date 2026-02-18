use std::collections::HashMap;
use std::hash::{Hash, Hasher};

use lyra_semantic::symbols::GlobalDefId;
use lyra_semantic::types::ConstInt;
use lyra_source::{FileId, Span, TextRange};
use smol_str::SmolStr;

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

/// Interned identity for a parameter environment.
///
/// `ParamEnvId(0)` is always the empty environment (no parameters).
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub(crate) struct ParamEnvId(u32);

impl ParamEnvId {
    pub(crate) const EMPTY: Self = Self(0);
}

/// Fingerprint for fast `HashMap` lookup without allocating a `Box<[ConstInt]>` to probe.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
struct EnvFingerprint {
    hash: u64,
    len: usize,
}

fn hash_values(values: &[ConstInt]) -> u64 {
    let mut hasher = std::hash::DefaultHasher::new();
    values.hash(&mut hasher);
    hasher.finish()
}

/// Deduplicating interner for parameter environments.
///
/// Maps value slices to small `ParamEnvId(u32)` IDs. Identical value
/// slices always get the same ID. The interner lives in `ElabCtx`
/// during elaboration and is moved into `ElabTree` for consumers.
#[derive(Debug, Clone)]
pub(crate) struct ParamEnvInterner {
    map: HashMap<EnvFingerprint, Vec<ParamEnvId>>,
    envs: Vec<Box<[ConstInt]>>,
}

impl PartialEq for ParamEnvInterner {
    fn eq(&self, other: &Self) -> bool {
        self.envs == other.envs
    }
}

impl Eq for ParamEnvInterner {}

impl ParamEnvInterner {
    pub(crate) fn new() -> Self {
        let empty: Box<[ConstInt]> = Box::new([]);
        let fp = EnvFingerprint {
            hash: hash_values(&empty),
            len: 0,
        };
        let id = ParamEnvId(0);
        let mut map = HashMap::new();
        map.insert(fp, vec![id]);
        Self {
            map,
            envs: vec![empty],
        }
    }

    pub(crate) fn intern(&mut self, values: Vec<ConstInt>) -> ParamEnvId {
        let fp = EnvFingerprint {
            hash: hash_values(&values),
            len: values.len(),
        };
        if let Some(candidates) = self.map.get(&fp) {
            for &cand_id in candidates {
                if self.envs[cand_id.0 as usize].as_ref() == values.as_slice() {
                    return cand_id;
                }
            }
        }
        let id = ParamEnvId(self.envs.len() as u32);
        self.envs.push(values.into_boxed_slice());
        self.map.entry(fp).or_default().push(id);
        id
    }

    pub(crate) fn values(&self, id: ParamEnvId) -> &[ConstInt] {
        &self.envs[id.0 as usize]
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
    pub(crate) param_env: ParamEnvId,
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
    pub(crate) envs: ParamEnvInterner,
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
