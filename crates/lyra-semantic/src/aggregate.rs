use lyra_ast::{AstIdMap, AstNode, ErasedAstId, NameRef, QualifiedName};
use lyra_lexer::SyntaxKind;
use lyra_parser::SyntaxNode;
use lyra_source::{FileId, Span};
use smol_str::SmolStr;

use crate::scopes::ScopeId;
use crate::type_extract::extract_base_ty_from_typespec;
use crate::types::Ty;

// Index types for def tables (file-local dense indices)

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct EnumDefIdx(pub(crate) u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StructDefIdx(pub(crate) u32);

// Global IDs (offset-independent, scope-owner-local ordinal)

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumId {
    pub file: FileId,
    pub owner: Option<SmolStr>,
    pub ordinal: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructId {
    pub file: FileId,
    pub owner: Option<SmolStr>,
    pub ordinal: u32,
}

// TypeRef: preserves span + name for struct fields and enum base type

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
pub struct EnumDef {
    pub name: Option<SmolStr>,
    pub owner: Option<SmolStr>,
    pub ordinal: u32,
    pub base: TypeRef,
    pub variants: Box<[EnumVariant]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumVariant {
    pub name: SmolStr,
    pub init: Option<ErasedAstId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructDef {
    pub name: Option<SmolStr>,
    pub owner: Option<SmolStr>,
    pub ordinal: u32,
    pub packed: bool,
    pub is_union: bool,
    pub scope: ScopeId,
    pub fields: Box<[StructField]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructField {
    pub name: SmolStr,
    pub ty: TypeRef,
}

// Resolved struct semantic information

/// A resolved struct field with its semantic type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldSem {
    pub name: SmolStr,
    pub ty: Ty,
    pub span: Span,
}

/// Resolved struct type information.
///
/// Contains resolved field types and a sorted lookup table for
/// efficient field-by-name access. Deterministic and compact --
/// suitable as a Salsa query return value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructSem {
    pub fields: Box<[FieldSem]>,
    pub field_lookup: Box<[(SmolStr, u32)]>,
    pub diags: Box<[crate::diagnostic::SemanticDiag]>,
}

impl StructSem {
    pub fn field_by_name(&self, name: &str) -> Option<(u32, &FieldSem)> {
        let i = self
            .field_lookup
            .binary_search_by(|(n, _)| n.as_str().cmp(name))
            .ok()?;
        let idx = self.field_lookup[i].1;
        Some((idx, &self.fields[idx as usize]))
    }
}

// TypeOrigin: binding from symbol to its type source

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeOrigin {
    TypeSpec,
    Enum(EnumDefIdx),
    Struct(StructDefIdx),
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
