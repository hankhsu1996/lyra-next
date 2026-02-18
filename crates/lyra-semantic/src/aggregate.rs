use lyra_ast::{AstNode, ErasedAstId, NameRef, QualifiedName};
use lyra_lexer::SyntaxKind;
use lyra_parser::SyntaxNode;
use lyra_source::{FileId, Span};
use smol_str::SmolStr;

use crate::type_extract::keyword_to_ty_pub;
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
    pub fields: Box<[StructField]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructField {
    pub name: SmolStr,
    pub ty: TypeRef,
}

// TypeOrigin: binding from symbol to its type source

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeOrigin {
    TypeSpec,
    Enum(EnumDefIdx),
    Struct(StructDefIdx),
}

// Extract a TypeRef from a TypeSpec syntax node.
pub(crate) fn extract_typeref_from_typespec(typespec: &SyntaxNode, file: FileId) -> TypeRef {
    use lyra_parser::SyntaxElement;

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

    // Try keyword base type
    for el in typespec.children_with_tokens() {
        if let SyntaxElement::Token(tok) = el
            && let Some(ty) = keyword_to_ty_pub(tok.kind())
        {
            return TypeRef::Resolved(ty);
        }
    }

    TypeRef::Resolved(Ty::Error)
}
