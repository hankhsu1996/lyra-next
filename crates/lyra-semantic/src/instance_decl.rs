use lyra_source::TextRange;
use smol_str::SmolStr;

use crate::scopes::ScopeId;
use crate::symbols::SymbolId;

/// Dense file-local index into `DefIndex.instance_decls`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InstanceDeclIdx(pub(crate) u32);

/// An instantiation of a module or interface (e.g., `my_bus sb();`).
///
/// Captures the instance name, the index of the type-name use-site
/// (for resolving whether the instantiated type is an interface), and
/// the symbol id for navigation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InstanceDecl {
    /// Index into `DefIndex.use_sites` for the instantiated type name.
    pub type_use_site_idx: u32,
    /// Symbol created for this instance name.
    pub sym_id: SymbolId,
    /// Instance name.
    pub name: SmolStr,
    /// Source range of the instance name identifier.
    pub name_range: TextRange,
    /// Source range of the instantiated type name.
    pub type_name_range: TextRange,
    /// Lexical scope containing this instance.
    pub scope: ScopeId,
}
