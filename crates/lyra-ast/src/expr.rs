use lyra_lexer::SyntaxKind;
use lyra_parser::{SyntaxNode, SyntaxToken};

use crate::node::is_expression_kind;
use crate::nodes::{Expression, ParenExpr, TypeSpec};

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

    /// Strip `Expression` and `ParenExpr` syntactic wrappers.
    ///
    /// Casts `node` to `Expr`, then loops: if the current expression is
    /// an `Expression` or `ParenExpr` wrapper, replaces with its inner
    /// child. Returns `None` if `node` is not an expression kind.
    /// If a wrapper has no inner child (malformed), returns the wrapper.
    pub fn peel(node: &SyntaxNode) -> Option<Expr> {
        use crate::node::AstNode;
        let mut current = Expr::cast(node.clone())?;
        loop {
            let inner = match current.kind() {
                SyntaxKind::Expression => {
                    Expression::cast(current.syntax().clone()).and_then(|w| w.inner())
                }
                SyntaxKind::ParenExpr => {
                    ParenExpr::cast(current.syntax().clone()).and_then(|p| p.inner())
                }
                _ => return Some(current),
            };
            match inner {
                Some(expr) => current = expr,
                None => return Some(current),
            }
        }
    }
}

/// Thin newtype over `TypeSpec` for the `TfArg` boundary.
#[derive(Debug, Clone)]
pub struct TypeRef {
    inner: TypeSpec,
}

impl TypeRef {
    pub fn can_cast(kind: SyntaxKind) -> bool {
        kind == SyntaxKind::TypeSpec
    }

    pub fn cast(node: SyntaxNode) -> Option<TypeRef> {
        use crate::node::AstNode;
        TypeSpec::cast(node).map(|ts| TypeRef { inner: ts })
    }

    pub fn syntax(&self) -> &SyntaxNode {
        use crate::node::AstNode;
        self.inner.syntax()
    }

    pub fn into_type_spec(self) -> TypeSpec {
        self.inner
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
