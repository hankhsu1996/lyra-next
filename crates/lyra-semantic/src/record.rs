use lyra_ast::{AstIdMap, AstNode, NameRef, QualifiedName};

use crate::Site;
use lyra_lexer::SyntaxKind;
use lyra_parser::SyntaxNode;
use lyra_source::{FileId, NameSpan};
use smol_str::SmolStr;

use crate::enum_def::EnumDefIdx;
use crate::instance_decl::InstanceDeclIdx;
use crate::scopes::ScopeId;
use crate::type_extract::extract_base_ty_from_typespec;
use crate::types::Ty;

// Index types for def tables (file-local dense indices)

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct RecordDefIdx(pub(crate) u32);

/// Stable identity for a record (struct/union) type definition.
///
/// Anchored to the `StructType` CST node's `Site`. Invariants:
/// - Anchor is always a `SyntaxKind::StructType` node.
/// - Builder collects exactly once per definition via `collect_record_def`.
/// - File identity derived via `Site::file()`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct RecordId(Site);

impl RecordId {
    pub fn new(def: Site) -> Self {
        Self(def)
    }

    pub fn file(self) -> FileId {
        self.0.file()
    }

    pub fn as_erased(self) -> Site {
        self.0
    }
}

/// Whether a record is a struct, union, or tagged union.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RecordKind {
    Struct,
    Union,
    TaggedUnion,
}

/// Whether a record is packed or unpacked.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Packing {
    Packed,
    SoftPacked,
    Unpacked,
}

// TypeRef: preserves name + anchor for record fields and enum base type.
// Gap-1 contract: TypeRef stores Site anchors, not TextRange/Span.

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeRef {
    Resolved(Ty),
    Named {
        name: SmolStr,
        type_site: Site,
    },
    Qualified {
        segments: Box<[SmolStr]>,
        type_site: Site,
    },
}

// Def tables

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecordDef {
    pub name: Option<SmolStr>,
    pub record_type_site: Site,
    pub kind: RecordKind,
    pub packing: Packing,
    pub scope: ScopeId,
    pub fields: Box<[RecordField]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecordField {
    pub name: SmolStr,
    pub name_site: Site,
    pub name_span: NameSpan,
    pub ty: TypeRef,
}

/// A resolved record field with its semantic type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldSem {
    pub name: SmolStr,
    pub ty: Ty,
    pub type_site: Site,
}

/// Resolved record type information.
///
/// Contains resolved field types and a sorted lookup table for
/// efficient field-by-name access. Deterministic and compact --
/// suitable as a Salsa query return value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecordSem {
    pub fields: Box<[FieldSem]>,
    pub field_lookup: Box<[(SmolStr, u32)]>,
    pub diags: Box<[crate::diagnostic::SemanticDiag]>,
}

impl RecordSem {
    pub fn field_by_name(&self, name: &str) -> Option<(u32, &FieldSem)> {
        let i = self
            .field_lookup
            .binary_search_by(|(n, _)| n.as_str().cmp(name))
            .ok()?;
        let idx = self.field_lookup[i].1;
        Some((idx, &self.fields[idx as usize]))
    }
}

// SymbolOrigin: binding from symbol to its type source

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolOrigin {
    /// Type derived from the `TypeSpec` AST node in the declaration.
    TypeSpec,
    /// Symbol IS an enum type (typedef or inline enum variable).
    Enum(EnumDefIdx),
    /// Symbol IS a record type (struct/union typedef or inline).
    Record(RecordDefIdx),
    /// Symbol is an enum variant (value-namespace constant).
    EnumVariant(EnumDefIdx),
    /// Symbol is a module/interface instance (e.g., `my_bus sb();`).
    Instance(InstanceDeclIdx),
    /// Poisoned origin: type extraction failed or was suppressed.
    Error,
}

// Extract a TypeRef from a TypeSpec syntax node.
pub(crate) fn extract_typeref_from_typespec(
    typespec: &SyntaxNode,
    ast_id_map: &AstIdMap,
) -> TypeRef {
    // Check for NameRef or QualifiedName child (user-defined type)
    for child in typespec.children() {
        if child.kind() == SyntaxKind::NameRef
            && let Some(name_ref) = NameRef::cast(child.clone())
            && let Some(ident) = name_ref.ident()
            && let Some(ast_id) = ast_id_map.ast_id(&name_ref)
        {
            return TypeRef::Named {
                name: SmolStr::new(ident.text()),
                type_site: ast_id.erase(),
            };
        } else if child.kind() == SyntaxKind::QualifiedName
            && let Some(qn) = QualifiedName::cast(child.clone())
            && let Some(ast_id) = ast_id_map.ast_id(&qn)
        {
            let segments: Box<[SmolStr]> = qn.segments().map(|s| SmolStr::new(s.text())).collect();
            if !segments.is_empty() {
                return TypeRef::Qualified {
                    segments,
                    type_site: ast_id.erase(),
                };
            }
        }
    }

    // Keyword base type with packed dimensions
    let ty = extract_base_ty_from_typespec(typespec, ast_id_map);
    TypeRef::Resolved(ty)
}
