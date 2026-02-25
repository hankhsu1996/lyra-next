use lyra_lexer::SyntaxKind;
use lyra_parser::{SyntaxNode, SyntaxToken};

use crate::node::is_expression_kind;
use crate::nodes::{ParenExpr, TypeSpec};

/// Typed handle for any expression-kind syntax node.
///
/// `Expr` wraps a `SyntaxNode` whose kind passes `is_expression_kind`.
/// It is the typed boundary for all expression-position returns in the AST.
#[derive(Debug, Clone)]
pub struct Expr {
    syntax: SyntaxNode,
}

impl Expr {
    pub fn can_cast(kind: SyntaxKind) -> bool {
        is_expression_kind(kind)
    }

    pub fn cast(node: SyntaxNode) -> Option<Expr> {
        if Self::can_cast(node.kind()) {
            Some(Expr { syntax: node })
        } else {
            None
        }
    }

    pub fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }

    pub fn kind(&self) -> SyntaxKind {
        self.syntax.kind()
    }

    /// Strip `ParenExpr` wrappers to find the inner expression.
    ///
    /// Consumes self. Loops: if the current node is a `ParenExpr`, replace
    /// with its inner expression and repeat. Only unwraps `ParenExpr` nodes.
    #[must_use]
    pub fn unwrap_parens(self) -> Expr {
        let mut current = self;
        loop {
            use crate::node::AstNode;
            let Some(paren) = ParenExpr::cast(current.syntax.clone()) else {
                return current;
            };
            let Some(inner) = paren.inner() else {
                return current;
            };
            current = inner;
        }
    }
}

/// Thin newtype over a `TypeSpec`-kind `SyntaxNode` for the `TfArg` boundary.
#[derive(Debug, Clone)]
pub struct TypeRef {
    syntax: SyntaxNode,
}

impl TypeRef {
    pub fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::TypeSpec
    }

    pub fn cast(node: SyntaxNode) -> Option<TypeRef> {
        if Self::can_cast(node.kind()) {
            Some(TypeRef { syntax: node })
        } else {
            None
        }
    }

    pub fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }

    pub fn into_type_spec(self) -> TypeSpec {
        use crate::node::AstNode;
        TypeSpec::cast(self.syntax).expect("TypeRef always wraps a TypeSpec-kind node")
    }
}

/// Syntactic category for system task/function arguments.
///
/// The parser produces both expression-kind nodes and `TypeSpec` nodes inside
/// `SystemTfArgList` (e.g. `$bits(logic [7:0])` has a `TypeSpec` child).
/// This sum type models that reality.
pub enum TfArg {
    Expr(Expr),
    Type(TypeRef),
    Unknown(SyntaxNode),
}

/// Structured classification of a `Literal` node's token content.
#[derive(Debug, Clone)]
pub enum LiteralKind {
    Int {
        token: SyntaxToken,
    },
    Based {
        size_token: Option<SyntaxToken>,
        base_token: SyntaxToken,
    },
    UnbasedUnsized {
        token: SyntaxToken,
    },
    Real {
        token: SyntaxToken,
    },
    String {
        token: SyntaxToken,
    },
    Time {
        token: SyntaxToken,
    },
    Unknown {
        token: SyntaxToken,
    },
}
