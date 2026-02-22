use lyra_lexer::SyntaxKind;
use lyra_parser::{SyntaxNode, SyntaxToken};

use crate::expr_helpers::is_expression_kind;
use crate::syntax_helpers::{system_tf_args, system_tf_name};

/// Borrowed view over a system call node.
///
/// Provides access to the call name and arguments without requiring
/// the caller to know whether the node is a `SystemTfCall` or `CallExpr`.
pub struct SystemCallView {
    node: SyntaxNode,
    name_token: SyntaxToken,
}

impl SystemCallView {
    pub fn try_from_node(node: &SyntaxNode) -> Option<Self> {
        let tok = system_tf_name(node)?;
        Some(Self {
            node: node.clone(),
            name_token: tok,
        })
    }

    pub fn name(&self) -> &str {
        self.name_token.text()
    }

    pub fn nth_arg(&self, n: usize) -> Option<SyntaxNode> {
        system_tf_args(&self.node).and_then(|a| iter_args(&a).nth(n))
    }

    pub fn arg_count(&self) -> usize {
        system_tf_args(&self.node).map_or(0, |a| iter_args(&a).count())
    }
}

/// Iterate argument nodes in a system task/function argument list.
///
/// Accepts `TypeSpec` (type-form args), `NameRef`/`QualifiedName`, and
/// expression nodes. Policy-specific to system call arg extraction.
fn iter_args(arg_list: &SyntaxNode) -> impl Iterator<Item = SyntaxNode> + '_ {
    arg_list.children().filter(|c| {
        matches!(
            c.kind(),
            SyntaxKind::TypeSpec | SyntaxKind::NameRef | SyntaxKind::QualifiedName
        ) || is_expression_kind(c.kind())
    })
}
