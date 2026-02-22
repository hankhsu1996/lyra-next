pub(crate) mod builder;
mod builder_order;
mod builder_stmt;
mod builder_types;
pub(crate) mod builtin_methods;
pub mod coerce;
pub mod const_eval;
pub mod def_index;
pub mod diagnostic;
pub mod enum_def;
pub mod expr_helpers;
pub mod global_index;
pub mod instance_decl;
pub mod interface_id;

pub(crate) mod literal;
pub mod member;
pub mod modport_def;
pub mod modport_facts;
pub mod name_graph;
pub mod record;
mod resolve;
pub mod resolve_index;
pub mod scopes;
pub mod symbols;
pub(crate) mod syntax_helpers;
pub mod system_call_view;
pub(crate) mod system_functions;
pub mod type_check;
mod type_extract;
pub mod type_infer;
pub mod types;

pub use builder::build_def_index;
pub use resolve::{
    build_resolve_core, build_resolve_index, detect_import_conflicts, resolve_name_in_scope,
    resolve_qualified_name,
};
pub use type_extract::{
    UserTypeRef, extract_base_ty_from_typespec, extract_type_from_container, extract_unpacked_dims,
    normalize_symbol_type, normalize_ty, user_type_ref,
};
pub use types::wrap_unpacked;
