use lyra_lexer::SyntaxKind;
use lyra_parser::SyntaxNode;

use crate::expr_helpers::is_expression_kind;
use crate::syntax_helpers::{system_tf_args, system_tf_name};
use crate::type_infer::{ExprType, ExprTypeErrorKind, InferCtx, infer_expr_type};

enum SystemFnKind {
    Clog2,
}

struct SystemFnEntry {
    name: &'static str,
    min_args: u8,
    max_args: Option<u8>,
    kind: SystemFnKind,
}

static BUILTINS: &[SystemFnEntry] = &[SystemFnEntry {
    name: "$clog2",
    min_args: 1,
    max_args: Some(1),
    kind: SystemFnKind::Clog2,
}];

/// Iterate argument nodes in a system task/function argument list.
///
/// Accepts `TypeSpec` (type-form args), `NameRef`/`QualifiedName`, and
/// expression nodes. Reuses `is_expression_kind` as the single maintained
/// predicate for expression node kinds.
pub(crate) fn iter_args(arg_list: &SyntaxNode) -> impl Iterator<Item = SyntaxNode> + '_ {
    arg_list.children().filter(|c| {
        matches!(
            c.kind(),
            SyntaxKind::TypeSpec | SyntaxKind::NameRef | SyntaxKind::QualifiedName
        ) || is_expression_kind(c.kind())
    })
}

fn arg_count(arg_list: &SyntaxNode) -> usize {
    iter_args(arg_list).count()
}

pub(crate) fn nth_arg(arg_list: &SyntaxNode, n: usize) -> Option<SyntaxNode> {
    iter_args(arg_list).nth(n)
}

fn check_arity(arg_list: &SyntaxNode, entry: &SystemFnEntry) -> Option<ExprType> {
    let count = arg_count(arg_list);
    if count < entry.min_args as usize {
        return Some(ExprType::error(ExprTypeErrorKind::UnsupportedExprKind));
    }
    if let Some(max) = entry.max_args
        && count > max as usize
    {
        return Some(ExprType::error(ExprTypeErrorKind::UnsupportedExprKind));
    }
    None
}

pub(crate) fn infer_system_call(node: &SyntaxNode, ctx: &dyn InferCtx) -> ExprType {
    let Some(tok) = system_tf_name(node) else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let name = tok.text();
    let Some(entry) = BUILTINS.iter().find(|e| e.name == name) else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedSystemCall);
    };
    let Some(args) = system_tf_args(node) else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    if let Some(err) = check_arity(&args, entry) {
        return err;
    }
    match entry.kind {
        SystemFnKind::Clog2 => infer_clog2(&args, ctx),
    }
}

fn infer_clog2(args: &SyntaxNode, ctx: &dyn InferCtx) -> ExprType {
    use crate::type_infer::{BitVecType, BitWidth, Signedness};
    if let Some(arg) = nth_arg(args, 0) {
        let _ = infer_expr_type(&arg, ctx, None);
    }
    ExprType::bitvec(BitVecType {
        width: BitWidth::Known(32),
        signed: Signedness::Signed,
        four_state: false,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::Ty;

    fn parse_system_call(src: &str) -> SyntaxNode {
        let tokens = lyra_lexer::lex(src);
        let pp = lyra_preprocess::preprocess_identity(lyra_source::FileId(0), &tokens, src);
        let parse = lyra_parser::parse(&pp.tokens, &pp.expanded_text);
        let root = parse.syntax();
        find_system_tf_call(&root).expect("should find SystemTfCall")
    }

    fn find_system_tf_call(node: &SyntaxNode) -> Option<SyntaxNode> {
        if node.kind() == SyntaxKind::SystemTfCall {
            return Some(node.clone());
        }
        for child in node.children() {
            if let Some(found) = find_system_tf_call(&child) {
                return Some(found);
            }
        }
        None
    }

    fn arg_list_from(src: &str) -> SyntaxNode {
        let call = parse_system_call(src);
        system_tf_args(&call).expect("should have arg list")
    }

    #[test]
    fn iter_args_three() {
        let al = arg_list_from("module m; initial $foo(a, b, c); endmodule");
        assert_eq!(iter_args(&al).count(), 3);
    }

    #[test]
    fn iter_args_single_nameref() {
        let al = arg_list_from("module m; initial $foo(x); endmodule");
        let args: Vec<_> = iter_args(&al).collect();
        assert_eq!(args.len(), 1);
        assert_eq!(args[0].kind(), SyntaxKind::NameRef);
    }

    #[test]
    fn iter_args_complex_exprs() {
        let al = arg_list_from("module m; initial $foo(a+b, f(x), x[i], x.y); endmodule");
        assert_eq!(iter_args(&al).count(), 4);
    }

    #[test]
    fn iter_args_sparse_commas() {
        let al = arg_list_from("module m; initial $foo(a,,b); endmodule");
        let count = iter_args(&al).count();
        assert!(count <= 3, "sparse commas: got {count} args");
    }

    #[test]
    fn ty_hash_eq_contract() {
        use std::hash::{DefaultHasher, Hash, Hasher};
        let t1 = Ty::int();
        let t2 = Ty::int();
        assert_eq!(t1, t2);
        let mut h1 = DefaultHasher::new();
        let mut h2 = DefaultHasher::new();
        t1.hash(&mut h1);
        t2.hash(&mut h2);
        assert_eq!(h1.finish(), h2.finish());
    }
}
