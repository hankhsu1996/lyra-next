use lyra_semantic::types::Ty;
use lyra_source::TextRange;
use smol_str::SmolStr;
use std::collections::HashMap;

/// Port direction in a module signature.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum PortDirection {
    Input,
    Output,
    Inout,
    Ref,
}

/// A port in a module signature, extracted from the module header.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct PortSig {
    pub(crate) name: SmolStr,
    pub(crate) direction: Option<PortDirection>,
    pub(crate) ty: Ty,
    pub(crate) name_range: TextRange,
    pub(crate) decl_range: TextRange,
}

/// Whether a module parameter is a value or type parameter.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ParamKind {
    Value,
    Type,
}

/// A parameter in a module signature, extracted from the parameter port list.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ParamSig {
    pub(crate) name: SmolStr,
    pub(crate) kind: ParamKind,
    pub(crate) has_default: bool,
    pub(crate) default_expr: Option<lyra_ast::ErasedAstId>,
    pub(crate) name_range: TextRange,
}

/// A module's signature: ports and parameters in declaration order.
///
/// Built from the module header AST. Independent of who instantiates the module.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct ModuleSig {
    pub(crate) name: SmolStr,
    pub(crate) ports: Box<[PortSig]>,
    pub(crate) params: Box<[ParamSig]>,
    port_index: HashMap<SmolStr, u32>,
    param_index: HashMap<SmolStr, u32>,
}

impl ModuleSig {
    pub(crate) fn new(name: SmolStr, ports: Vec<PortSig>, params: Vec<ParamSig>) -> Self {
        let port_index = ports
            .iter()
            .enumerate()
            .map(|(i, p)| (p.name.clone(), i as u32))
            .collect();
        let param_index = params
            .iter()
            .enumerate()
            .map(|(i, p)| (p.name.clone(), i as u32))
            .collect();
        Self {
            name,
            ports: ports.into_boxed_slice(),
            params: params.into_boxed_slice(),
            port_index,
            param_index,
        }
    }

    /// O(1) port lookup by name.
    pub(crate) fn port_by_name(&self, name: &str) -> Option<(u32, &PortSig)> {
        let &idx = self.port_index.get(name)?;
        Some((idx, &self.ports[idx as usize]))
    }

    /// O(1) param lookup by name.
    pub(crate) fn param_by_name(&self, name: &str) -> Option<(u32, &ParamSig)> {
        let &idx = self.param_index.get(name)?;
        Some((idx, &self.params[idx as usize]))
    }
}
