use crate::Site;
use lyra_source::NameSpan;
use smol_str::SmolStr;

use crate::interface_id::InterfaceDefId;

/// Stable cross-file identity for a modport item.
///
/// Ordinal determinism: a single source-order pass over the interface body's
/// children. Count every successfully-parsed `ModportItem` node. Error-recovered
/// items that produced a `ModportItem` node still get an ordinal; items that
/// failed to parse and produced only `Error` nodes do not. Ordinal resets to 0
/// for each interface.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModportDefId {
    pub owner: InterfaceDefId,
    pub ordinal: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModportDef {
    pub id: ModportDefId,
    pub name: SmolStr,
    pub entries: Box<[ModportEntry]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModportEntry {
    pub port_name: SmolStr,
    pub direction: PortDirection,
    pub target: ModportTarget,
    pub port_id: Site,
    pub name_span: NameSpan,
}

/// What a modport port maps to.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModportTarget {
    /// Bare identifier: the port name equals the member name.
    ImplicitMember { member_name: SmolStr },
    /// `.P(expr)`: the expression AST node id.
    Expr(Site),
    /// `.P()`: no connection.
    Empty,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PortDirection {
    Input,
    Output,
    Inout,
    Ref,
}
