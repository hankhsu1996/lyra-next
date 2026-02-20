use std::collections::HashMap;
use std::hash::{Hash, Hasher};

use lyra_ast::ErasedAstId;
use lyra_semantic::symbols::GlobalDefId;
use lyra_semantic::types::ConstInt;
use lyra_source::{FileId, Span, TextRange};
use smol_str::SmolStr;

// Arena IDs

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub(crate) struct InstId(u32);

impl InstId {
    pub(crate) fn index(self) -> usize {
        self.0 as usize
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub(crate) struct GenScopeId(u32);

impl GenScopeId {
    pub(crate) fn index(self) -> usize {
        self.0 as usize
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub(crate) enum ElabNodeId {
    Inst(InstId),
    GenScope(GenScopeId),
}

// Origin keys (stable identity data on nodes)

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub(crate) struct InstOrigin {
    pub(crate) parent_inst: Option<InstId>,
    pub(crate) inst_stmt_ast: ErasedAstId,
    pub(crate) inst_ordinal: u32,
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub(crate) struct GenScopeOrigin {
    pub(crate) parent_inst: InstId,
    pub(crate) scope_ast: ErasedAstId,
    pub(crate) iter: Option<ConstInt>,
}

// Node types

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct InstanceNode {
    pub(crate) origin: InstOrigin,
    pub(crate) parent: Option<ElabNodeId>,
    pub(crate) module_def: GlobalDefId,
    pub(crate) instance_name: SmolStr,
    pub(crate) param_env: ParamEnvId,
    pub(crate) children: Vec<ElabNodeId>,
    pub(crate) source_file: FileId,
    pub(crate) name_range: TextRange,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum GenScopeKind {
    If,
    ForIteration {
        genvar_name: SmolStr,
        genvar_value: ConstInt,
    },
    CaseItem,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct GenScopeNode {
    pub(crate) origin: GenScopeOrigin,
    pub(crate) parent: ElabNodeId,
    pub(crate) name: Option<SmolStr>,
    pub(crate) kind: GenScopeKind,
    pub(crate) children: Vec<ElabNodeId>,
    pub(crate) source_file: FileId,
    pub(crate) offset: TextRange,
}

// ElabTree

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ElabTree {
    pub(crate) top: Option<InstId>,
    pub(crate) instances: Vec<InstanceNode>,
    pub(crate) gen_scopes: Vec<GenScopeNode>,
    pub(crate) diagnostics: Vec<ElabDiag>,
    pub(crate) envs: ParamEnvInterner,
    pub(crate) genvar_envs: GenvarEnvInterner,
}

impl ElabTree {
    pub(crate) fn push_instance(&mut self, node: InstanceNode) -> InstId {
        let id = InstId(self.instances.len() as u32);
        self.instances.push(node);
        id
    }

    pub(crate) fn push_gen_scope(&mut self, node: GenScopeNode) -> GenScopeId {
        let id = GenScopeId(self.gen_scopes.len() as u32);
        self.gen_scopes.push(node);
        id
    }

    pub(crate) fn inst(&self, id: InstId) -> &InstanceNode {
        &self.instances[id.index()]
    }

    pub(crate) fn inst_mut(&mut self, id: InstId) -> &mut InstanceNode {
        &mut self.instances[id.index()]
    }

    #[cfg(test)]
    pub(crate) fn gen_scope(&self, id: GenScopeId) -> &GenScopeNode {
        &self.gen_scopes[id.index()]
    }

    pub(crate) fn gen_scope_mut(&mut self, id: GenScopeId) -> &mut GenScopeNode {
        &mut self.gen_scopes[id.index()]
    }

    pub(crate) fn add_child(&mut self, parent: ElabNodeId, child: ElabNodeId) {
        match parent {
            ElabNodeId::Inst(id) => self.inst_mut(id).children.push(child),
            ElabNodeId::GenScope(id) => self.gen_scope_mut(id).children.push(child),
        }
    }
}

// ParamEnvId and interner

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub(crate) struct ParamEnvId(u32);

impl ParamEnvId {
    pub(crate) const EMPTY: Self = Self(0);
}

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

// GenvarEnvId and interner

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub(crate) struct GenvarEnvId(u32);

impl GenvarEnvId {
    pub(crate) const EMPTY: Self = Self(0);
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
struct GenvarEnvFingerprint {
    parent: GenvarEnvId,
    name_hash: u64,
    value: ConstInt,
}

#[derive(Debug, Clone)]
pub(crate) struct GenvarEnvInterner {
    map: HashMap<GenvarEnvFingerprint, Vec<GenvarEnvId>>,
    envs: Vec<Box<[(SmolStr, ConstInt)]>>,
}

impl PartialEq for GenvarEnvInterner {
    fn eq(&self, other: &Self) -> bool {
        self.envs == other.envs
    }
}

impl Eq for GenvarEnvInterner {}

impl GenvarEnvInterner {
    pub(crate) fn new() -> Self {
        let empty: Box<[(SmolStr, ConstInt)]> = Box::new([]);
        let mut map = HashMap::new();
        let fp = GenvarEnvFingerprint {
            parent: GenvarEnvId(0),
            name_hash: 0,
            value: ConstInt::Known(0),
        };
        map.insert(fp, vec![GenvarEnvId(0)]);
        Self {
            map,
            envs: vec![empty],
        }
    }

    pub(crate) fn push(
        &mut self,
        parent: GenvarEnvId,
        name: SmolStr,
        value: ConstInt,
    ) -> GenvarEnvId {
        let name_hash = {
            let mut h = std::hash::DefaultHasher::new();
            name.hash(&mut h);
            h.finish()
        };
        let fp = GenvarEnvFingerprint {
            parent,
            name_hash,
            value: value.clone(),
        };
        if let Some(candidates) = self.map.get(&fp) {
            for &cand_id in candidates {
                let cand = &self.envs[cand_id.0 as usize];
                let parent_vals = &self.envs[parent.0 as usize];
                if cand.len() == parent_vals.len() + 1
                    && cand[..parent_vals.len()] == parent_vals[..]
                    && cand.last().map(|(n, v)| (n, v)) == Some((&name, &value))
                {
                    return cand_id;
                }
            }
        }
        let parent_vals = self.envs[parent.0 as usize].to_vec();
        let mut new_vals = parent_vals;
        new_vals.push((name, value));
        let id = GenvarEnvId(self.envs.len() as u32);
        self.envs.push(new_vals.into_boxed_slice());
        self.map.entry(fp).or_default().push(id);
        id
    }

    pub(crate) fn values(&self, id: GenvarEnvId) -> &[(SmolStr, ConstInt)] {
        &self.envs[id.0 as usize]
    }
}

// Condition cache key

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub(crate) enum CondSiteKind {
    GenIf,
    GenCase,
}

pub(crate) type CondCacheKey = (ErasedAstId, CondSiteKind, ParamEnvId, GenvarEnvId);

// Elaboration diagnostics

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ElabDiag {
    UnresolvedModuleInst {
        name: SmolStr,
        span: Span,
    },
    NotInstantiable {
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
    GenerateIterationLimit {
        span: Span,
        limit: usize,
    },
}

impl ElabDiag {
    pub(crate) fn span(&self) -> &Span {
        match self {
            Self::UnresolvedModuleInst { span, .. }
            | Self::NotInstantiable { span, .. }
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
            | Self::GenvarNotConst { span }
            | Self::GenerateIterationLimit { span, .. } => span,
        }
    }
}
