pub mod aggregate;
pub(crate) mod builder;
mod builder_stmt;
pub mod const_eval;
pub mod def_index;
pub mod diagnostic;
pub mod global_index;
pub(crate) mod literal;
pub mod name_graph;
mod resolve;
pub mod resolve_index;
pub mod scopes;
pub mod symbols;
pub(crate) mod syntax_helpers;
pub mod type_check;
mod type_extract;
pub mod type_infer;
pub mod types;

pub use builder::build_def_index;
pub use resolve::{build_resolve_core, build_resolve_index};
pub use type_extract::{
    extract_base_ty_from_typespec, extract_type_from_container, normalize_symbol_type,
    normalize_ty, typespec_name_ref,
};
pub use types::wrap_unpacked;
