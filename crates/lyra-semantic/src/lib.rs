mod builder;
pub mod const_eval;
pub mod def_index;
pub mod diagnostic;
pub mod global_index;
pub mod name_graph;
mod resolve;
pub mod resolve_index;
pub mod scopes;
pub mod symbols;
pub mod types;

pub use builder::build_def_index;
pub use resolve::{build_resolve_core, build_resolve_index};
