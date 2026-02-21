use lyra_ast::{AstIdMap, AstNode, NameRef, QualifiedName};
use lyra_lexer::SyntaxKind;
use lyra_parser::SyntaxNode;
use lyra_source::{FileId, Span};
use smol_str::SmolStr;

use crate::enum_def::EnumDefIdx;
use crate::instance_decl::InstanceDeclIdx;
use crate::scopes::ScopeId;
use crate::type_extract::extract_base_ty_from_typespec;
use crate::types::Ty;

// Index types for def tables (file-local dense indices)

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct RecordDefIdx(pub(crate) u32);

// Global IDs (offset-independent, scope-owner-local ordinal)

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RecordId {
    pub file: FileId,
    pub owner: Option<SmolStr>,
    pub ordinal: u32,
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

// TypeRef: preserves span + name for record fields and enum base type

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeRef {
    Resolved(Ty),
    Named {
        name: SmolStr,
        span: Span,
    },
    Qualified {
        segments: Box<[SmolStr]>,
        span: Span,
    },
}

// Def tables

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecordDef {
    pub name: Option<SmolStr>,
    pub owner: Option<SmolStr>,
    pub ordinal: u32,
    pub kind: RecordKind,
    pub packing: Packing,
    pub scope: ScopeId,
    pub fields: Box<[RecordField]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecordField {
    pub name: SmolStr,
    pub name_range: lyra_source::TextRange,
    pub ty: TypeRef,
}

/// A resolved record field with its semantic type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldSem {
    pub name: SmolStr,
    pub ty: Ty,
    pub span: Span,
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
    EnumVariant {
        enum_idx: EnumDefIdx,
        variant_ordinal: u32,
    },
    /// Symbol is a module/interface instance (e.g., `my_bus sb();`).
    Instance(InstanceDeclIdx),
    /// Poisoned origin: type extraction failed or was suppressed.
    Error,
}

// Extract a TypeRef from a TypeSpec syntax node.
pub(crate) fn extract_typeref_from_typespec(
    typespec: &SyntaxNode,
    file: FileId,
    ast_id_map: &AstIdMap,
) -> TypeRef {
    // Check for NameRef or QualifiedName child (user-defined type)
    for child in typespec.children() {
        if child.kind() == SyntaxKind::NameRef
            && let Some(name_ref) = NameRef::cast(child.clone())
            && let Some(ident) = name_ref.ident()
        {
            let range = name_ref.text_range();
            return TypeRef::Named {
                name: SmolStr::new(ident.text()),
                span: Span { file, range },
            };
        } else if child.kind() == SyntaxKind::QualifiedName
            && let Some(qn) = QualifiedName::cast(child.clone())
        {
            let segments: Box<[SmolStr]> = qn.segments().map(|s| SmolStr::new(s.text())).collect();
            if !segments.is_empty() {
                let range = qn.text_range();
                return TypeRef::Qualified {
                    segments,
                    span: Span { file, range },
                };
            }
        }
    }

    // Keyword base type with packed dimensions
    let ty = extract_base_ty_from_typespec(typespec, ast_id_map);
    TypeRef::Resolved(ty)
}
