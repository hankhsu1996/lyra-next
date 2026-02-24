use lyra_semantic::resolve_index::ResolvedTarget;
use lyra_semantic::symbols::GlobalSymbolId;
use lyra_semantic::type_infer::ExprType;
use lyra_semantic::types::{SymbolType, Ty};
use smol_str::SmolStr;

use crate::expr_queries::ExprRef;
use crate::pipeline::{ast_id_map, parse_file};
use crate::semantic::{def_index_file, global_def_index, resolve_index_file};
use crate::type_queries::{SymbolRef, type_of_symbol};
use crate::{CompilationUnit, SourceFile, source_file_by_id};

/// Resolve the name at a cursor position.
///
/// Finds the nearest `NameRef` at `offset`, looks up its `AstId`,
/// and returns the resolved `GlobalSymbolId` if found.
/// Also handles module instantiation type names (e.g. `adder` in
/// `adder u1(...)`) and qualified names (`pkg::sym`).
pub fn resolve_at(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
    offset: lyra_source::TextSize,
) -> Option<GlobalSymbolId> {
    let parse = parse_file(db, file);
    let ast_map = ast_id_map(db, file);
    let resolve = resolve_index_file(db, file, unit);

    // Try NameRef first (common case)
    if let Some(name_ref) = find_name_ref_at(&parse.syntax(), offset)
        && let Some(ast_id) = ast_map.ast_id(&name_ref)
        && let Some(resolution) = resolve.resolutions.get(&ast_id.erase())
    {
        return resolution_to_symbol(&resolution.target);
    }

    // Try qualified name (pkg::sym)
    if let Some(result) = find_qualified_name_at(db, file, unit, &parse.syntax(), offset) {
        return Some(result);
    }

    // Fallback: module instantiation type name
    if let Some(inst) = find_module_instantiation_name_at(&parse.syntax(), offset)
        && let Some(ast_id) = ast_map.ast_id(&inst)
        && let Some(resolution) = resolve.resolutions.get(&ast_id.erase())
    {
        return resolution_to_symbol(&resolution.target);
    }

    None
}

/// Look up a symbol by its global id within a compilation unit.
pub fn symbol_global(
    db: &dyn salsa::Database,
    unit: CompilationUnit,
    id: GlobalSymbolId,
) -> Option<&lyra_semantic::symbols::Symbol> {
    let file = source_file_by_id(db, unit, id.file)?;
    Some(def_index_file(db, file).symbols.get(id.local))
}

/// Find a `NameRef` node at or near the given offset.
fn find_name_ref_at(
    root: &lyra_parser::SyntaxNode,
    offset: lyra_source::TextSize,
) -> Option<lyra_ast::NameRef> {
    use lyra_ast::AstNode;
    let token = root.token_at_offset(offset).right_biased()?;
    token.parent_ancestors().find_map(lyra_ast::NameRef::cast)
}

/// Find a qualified name at the cursor position and resolve it.
///
/// When on the package name part: resolve to the package declaration.
/// When on the member name part: look up the `QualifiedName`'s `AstId`
/// in the resolve index.
fn find_qualified_name_at(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
    root: &lyra_parser::SyntaxNode,
    offset: lyra_source::TextSize,
) -> Option<GlobalSymbolId> {
    use lyra_ast::AstNode;
    let token = root.token_at_offset(offset).right_biased()?;
    let qn = token
        .parent_ancestors()
        .find_map(lyra_ast::QualifiedName::cast)?;

    let segments: Vec<_> = qn.segments().collect();
    if segments.len() < 2 {
        return None;
    }

    // Determine if cursor is on the package name or member name
    let on_package = segments[0].text_range().contains(offset);

    if on_package {
        // Resolve to the package declaration
        let pkg_name = segments[0].text();
        let global = global_def_index(db, unit);
        let def_id = global.resolve_package(pkg_name.as_ref())?;
        let target_file = source_file_by_id(db, unit, def_id.file())?;
        let target_def = def_index_file(db, target_file);
        let local = target_def
            .name_ast_to_symbol
            .get(&def_id.ast_id())
            .copied()?;
        Some(GlobalSymbolId {
            file: def_id.file(),
            local,
        })
    } else {
        // Resolve via the QualifiedName's AstId in the resolve index
        let ast_map = ast_id_map(db, file);
        let resolve = resolve_index_file(db, file, unit);
        let ast_id = ast_map.ast_id(&qn)?;
        let resolution = resolve.resolutions.get(&ast_id.erase())?;
        resolution_to_symbol(&resolution.target)
    }
}

fn resolution_to_symbol(target: &ResolvedTarget) -> Option<GlobalSymbolId> {
    match target {
        ResolvedTarget::Symbol(sym) => Some(*sym),
        ResolvedTarget::EnumVariant(_) => None,
    }
}

/// Find a `ModuleInstantiation` node where the cursor is on the module
/// type name (not the instance name).
fn find_module_instantiation_name_at(
    root: &lyra_parser::SyntaxNode,
    offset: lyra_source::TextSize,
) -> Option<lyra_ast::ModuleInstantiation> {
    use lyra_ast::AstNode;
    let token = root.token_at_offset(offset).right_biased()?;
    let inst = token
        .parent_ancestors()
        .find_map(lyra_ast::ModuleInstantiation::cast)?;
    // Only match if offset is within the module name token
    let name_token = inst.module_name()?;
    if name_token.text_range().contains(offset) {
        Some(inst)
    } else {
        None
    }
}

/// Result of a `type_at` query: either a symbol's declared type or
/// an expression's inferred type.
pub enum TypeAtResult {
    Expr(ExprType),
    Symbol(SymbolType),
    SymbolEnriched { st: SymbolType, pretty: SmolStr },
}

impl TypeAtResult {
    pub fn pretty(&self) -> SmolStr {
        match self {
            TypeAtResult::Expr(et) => et.pretty(),
            TypeAtResult::Symbol(st) => st.pretty(),
            TypeAtResult::SymbolEnriched { pretty, .. } => pretty.clone(),
        }
    }
}

/// Lossless type formatter with DB-powered name enrichment.
///
/// `Ty::pretty()` is always lossless but cannot resolve aggregate names
/// (enum/record) because `lyra-semantic` has no DB access. `TyFmt` adds
/// name lookup by querying def indexes through Salsa, producing output
/// like `enum color_t` or `struct packed pixel_t` instead of bare keywords.
pub(crate) struct TyFmt<'db> {
    db: &'db dyn salsa::Database,
    unit: CompilationUnit,
}

impl<'db> TyFmt<'db> {
    pub(crate) fn new(db: &'db dyn salsa::Database, unit: CompilationUnit) -> Self {
        Self { db, unit }
    }

    pub(crate) fn symbol_type(&self, st: &SymbolType) -> SmolStr {
        match st {
            SymbolType::Value(ty) => self.ty(ty),
            SymbolType::TypeAlias(ty) => SmolStr::new(format!("type = {}", self.ty(ty))),
            SymbolType::Net(net) => {
                SmolStr::new(format!("{} {}", net.kind.keyword_str(), self.ty(&net.data)))
            }
            SymbolType::Error(_) => SmolStr::new_static("<error>"),
        }
    }

    pub(crate) fn ty(&self, ty: &Ty) -> SmolStr {
        use lyra_semantic::types::collect_array_dims;

        let (base, dims) = collect_array_dims(ty);
        let base_str = match base {
            Ty::Enum(id) => self.enum_name(id),
            Ty::Record(id) => self.record_name(id),
            Ty::Interface(iface_ty) => self.interface_name(iface_ty),
            other => other.pretty().to_string(),
        };

        if dims.is_empty() {
            return SmolStr::new(base_str);
        }
        let mut s = base_str;
        for dim in &dims {
            s.push(' ');
            fmt_unpacked_dim(&mut s, dim);
        }
        SmolStr::new(s)
    }

    fn enum_name(&self, id: &lyra_semantic::enum_def::EnumId) -> String {
        let Some(src) = source_file_by_id(self.db, self.unit, id.file()) else {
            return "enum".to_string();
        };
        let def = def_index_file(self.db, src);
        match def.enum_def_by_id(*id).and_then(|d| d.name.clone()) {
            Some(n) => format!("enum {n}"),
            None => "enum".to_string(),
        }
    }

    fn interface_name(&self, iface_ty: &lyra_semantic::types::InterfaceType) -> String {
        use crate::semantic::symbol_at_name_ast;

        let Some(gsym) =
            symbol_at_name_ast(self.db, self.unit, iface_ty.iface.global_def().ast_id())
        else {
            return "interface".to_string();
        };
        let Some(src) = source_file_by_id(self.db, self.unit, gsym.file) else {
            return "interface".to_string();
        };
        let def = def_index_file(self.db, src);
        let iface_name = def.symbols.get(gsym.local).name.clone();
        match iface_ty.modport {
            Some(mp_id) => {
                let mp_name = def.modport_defs.get(&mp_id).map(|d| d.name.as_str());
                match mp_name {
                    Some(n) => format!("{iface_name}.{n}"),
                    None => iface_name.to_string(),
                }
            }
            None => iface_name.to_string(),
        }
    }

    fn record_name(&self, id: &lyra_semantic::record::RecordId) -> String {
        use lyra_semantic::record::{Packing, RecordKind};

        let Some(src) = source_file_by_id(self.db, self.unit, id.file()) else {
            return "struct".to_string();
        };
        let def = def_index_file(self.db, src);
        let info = def.record_def_by_id(*id);
        match info {
            Some(d) => {
                let mut s = String::new();
                match d.kind {
                    RecordKind::Union | RecordKind::TaggedUnion => s.push_str("union"),
                    RecordKind::Struct => s.push_str("struct"),
                }
                match d.packing {
                    Packing::SoftPacked => s.push_str(" soft"),
                    Packing::Packed => s.push_str(" packed"),
                    Packing::Unpacked => {}
                }
                if let Some(ref n) = d.name {
                    s.push(' ');
                    s.push_str(n);
                }
                s
            }
            None => "struct".to_string(),
        }
    }
}

fn fmt_unpacked_dim(out: &mut String, dim: &lyra_semantic::types::UnpackedDim) {
    use core::fmt::Write;
    use lyra_semantic::types::{AssocIndex, UnpackedDim};
    match dim {
        UnpackedDim::Range { msb, lsb } => {
            let _ = write!(out, "[{}:{}]", FmtConst(msb), FmtConst(lsb));
        }
        UnpackedDim::Size(c) => {
            let _ = write!(out, "[{}]", FmtConst(c));
        }
        UnpackedDim::Unsized => out.push_str("[]"),
        UnpackedDim::Queue { bound: None } => out.push_str("[$]"),
        UnpackedDim::Queue { bound: Some(c) } => {
            let _ = write!(out, "[$:{}]", FmtConst(c));
        }
        UnpackedDim::Assoc(AssocIndex::Wildcard) => out.push_str("[*]"),
        UnpackedDim::Assoc(AssocIndex::Typed(ty)) => {
            let _ = write!(out, "[{}]", ty.pretty());
        }
    }
}

struct FmtConst<'a>(&'a lyra_semantic::types::ConstInt);

impl core::fmt::Display for FmtConst<'_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        use lyra_semantic::types::ConstInt;
        match self.0 {
            ConstInt::Known(v) => write!(f, "{v}"),
            ConstInt::Unevaluated(_) => f.write_str("?"),
            ConstInt::Error(_) => f.write_str("!"),
        }
    }
}

/// Return type information at a source position.
///
/// `offset` is in expanded-text space (post-preprocess), the same coordinate
/// system used by `resolve_at` and `parse_file`. Tool layers that receive
/// original-source positions must map through the source map first.
///
/// Not a Salsa query -- convenience function over cached queries.
/// Priority: symbol > smallest enclosing expression > None.
pub fn type_at(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
    offset: lyra_source::TextSize,
) -> Option<TypeAtResult> {
    let parse = parse_file(db, file);
    let ast_map = ast_id_map(db, file);

    // Priority 1: Symbol (NameRef / QualifiedName that resolves)
    if let Some(gsym) = resolve_at(db, file, unit, offset) {
        let sym_ref = SymbolRef::new(db, unit, gsym);
        let st = type_of_symbol(db, sym_ref);
        let fmt = TyFmt::new(db, unit);
        let pretty = fmt.symbol_type(&st);
        return Some(TypeAtResult::SymbolEnriched { st, pretty });
    }

    // Priority 2: Smallest enclosing "real" expression
    let token = parse.syntax().token_at_offset(offset).right_biased()?;
    for ancestor in token.parent_ancestors() {
        if is_real_expression_kind(ancestor.kind()) {
            let expr_ast_id = ast_map.erased_ast_id(&ancestor)?;
            let expr_ref = ExprRef::new(db, unit, expr_ast_id);
            let et = crate::expr_queries::type_of_expr(db, expr_ref);
            return Some(TypeAtResult::Expr(et));
        }
    }

    None
}

/// Expression kinds that are "real" (not transparent wrappers).
fn is_real_expression_kind(kind: lyra_lexer::SyntaxKind) -> bool {
    use lyra_lexer::SyntaxKind;
    matches!(
        kind,
        SyntaxKind::BinExpr
            | SyntaxKind::PrefixExpr
            | SyntaxKind::CondExpr
            | SyntaxKind::ConcatExpr
            | SyntaxKind::ReplicExpr
            | SyntaxKind::IndexExpr
            | SyntaxKind::CallExpr
            | SyntaxKind::Literal
            | SyntaxKind::NameRef
            | SyntaxKind::QualifiedName
    )
}
