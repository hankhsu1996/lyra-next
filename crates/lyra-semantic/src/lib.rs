pub(crate) mod builder;
mod builder_items;
mod builder_order;
mod builder_stmt;
mod builder_types;
pub(crate) mod builtin_methods;
pub mod case_check;
pub mod coerce;
pub mod const_eval;
pub mod def_entry;
pub mod def_index;
pub mod diagnostic;
pub mod enum_def;
pub mod foreach_check;
pub mod global_index;
pub mod instance_decl;
pub mod interface_id;
pub mod lhs;

pub(crate) mod literal;
pub mod member;
pub mod modport_def;
pub mod modport_facts;
pub mod name_graph;
pub mod name_lowering;
pub mod nettype_def;
pub mod record;
mod resolve;
pub mod resolve_index;
pub mod scopes;
pub mod site;
pub(crate) mod streaming;
pub mod symbols;
pub(crate) mod system_functions;
pub mod time_scale;
pub mod type_check;
pub(crate) mod type_check_array_query;
pub(crate) mod type_check_dim;
pub(crate) mod type_check_expr;
pub(crate) mod type_check_system_call;
mod type_extract;
pub mod type_infer;
pub mod types;

pub use site::Site;
pub use streaming::fixed_stream_width_bits_of_expr_type;

pub use builder::build_def_index;
pub use global_index::{build_cu_scope_index, build_file_scope_index};
pub use name_lowering::{QualifiedPath, lower_qualified_name};
pub use resolve::{
    ResolveEnv, build_resolve_core, build_resolve_index, detect_import_conflicts,
    resolve_name_in_scope, resolve_qualified_path,
};
pub use type_extract::{
    UserTypeRef, extract_base_ty_from_type_ref, extract_base_ty_from_typespec,
    extract_type_from_container, extract_unpacked_dim, normalize_symbol_type, normalize_ty,
    user_type_ref, user_type_ref_from_expr, user_type_ref_from_type_ref,
};
pub use types::{DataTyView, wrap_unpacked};
