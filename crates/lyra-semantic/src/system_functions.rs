use lyra_ast::{AstIdMap, ExprKind, SystemTfCall, TfArg};

use crate::type_infer::{ExprType, ExprTypeErrorKind, InferCtx, infer_expr};

pub(crate) enum SystemFnKind {
    Clog2,
    Signed,
    Unsigned,
    Bits,
    ReturnsInt,
    ReturnsBit,
    RealMath1,
    RealMath2,
    IntToReal,
    RealToInt,
    RealToBits,
    ShortRealToBits,
    BitsToReal,
    BitsToShortReal,
}

pub(crate) struct SystemFnEntry {
    pub(crate) name: &'static str,
    pub(crate) min_args: u8,
    pub(crate) max_args: Option<u8>,
    pub(crate) kind: SystemFnKind,
}

static BUILTINS: &[SystemFnEntry] = &[
    SystemFnEntry {
        name: "$clog2",
        min_args: 1,
        max_args: Some(1),
        kind: SystemFnKind::Clog2,
    },
    SystemFnEntry {
        name: "$signed",
        min_args: 1,
        max_args: Some(1),
        kind: SystemFnKind::Signed,
    },
    SystemFnEntry {
        name: "$unsigned",
        min_args: 1,
        max_args: Some(1),
        kind: SystemFnKind::Unsigned,
    },
    SystemFnEntry {
        name: "$bits",
        min_args: 1,
        max_args: Some(1),
        kind: SystemFnKind::Bits,
    },
    SystemFnEntry {
        name: "$countones",
        min_args: 1,
        max_args: Some(1),
        kind: SystemFnKind::ReturnsInt,
    },
    SystemFnEntry {
        name: "$countbits",
        min_args: 2,
        max_args: None,
        kind: SystemFnKind::ReturnsInt,
    },
    SystemFnEntry {
        name: "$onehot",
        min_args: 1,
        max_args: Some(1),
        kind: SystemFnKind::ReturnsBit,
    },
    SystemFnEntry {
        name: "$onehot0",
        min_args: 1,
        max_args: Some(1),
        kind: SystemFnKind::ReturnsBit,
    },
    SystemFnEntry {
        name: "$isunknown",
        min_args: 1,
        max_args: Some(1),
        kind: SystemFnKind::ReturnsBit,
    },
    // Real math functions (LRM 20.8.2) -- 1-arg
    SystemFnEntry {
        name: "$ln",
        min_args: 1,
        max_args: Some(1),
        kind: SystemFnKind::RealMath1,
    },
    SystemFnEntry {
        name: "$log10",
        min_args: 1,
        max_args: Some(1),
        kind: SystemFnKind::RealMath1,
    },
    SystemFnEntry {
        name: "$exp",
        min_args: 1,
        max_args: Some(1),
        kind: SystemFnKind::RealMath1,
    },
    SystemFnEntry {
        name: "$sqrt",
        min_args: 1,
        max_args: Some(1),
        kind: SystemFnKind::RealMath1,
    },
    SystemFnEntry {
        name: "$floor",
        min_args: 1,
        max_args: Some(1),
        kind: SystemFnKind::RealMath1,
    },
    SystemFnEntry {
        name: "$ceil",
        min_args: 1,
        max_args: Some(1),
        kind: SystemFnKind::RealMath1,
    },
    SystemFnEntry {
        name: "$sin",
        min_args: 1,
        max_args: Some(1),
        kind: SystemFnKind::RealMath1,
    },
    SystemFnEntry {
        name: "$cos",
        min_args: 1,
        max_args: Some(1),
        kind: SystemFnKind::RealMath1,
    },
    SystemFnEntry {
        name: "$tan",
        min_args: 1,
        max_args: Some(1),
        kind: SystemFnKind::RealMath1,
    },
    SystemFnEntry {
        name: "$asin",
        min_args: 1,
        max_args: Some(1),
        kind: SystemFnKind::RealMath1,
    },
    SystemFnEntry {
        name: "$acos",
        min_args: 1,
        max_args: Some(1),
        kind: SystemFnKind::RealMath1,
    },
    SystemFnEntry {
        name: "$atan",
        min_args: 1,
        max_args: Some(1),
        kind: SystemFnKind::RealMath1,
    },
    SystemFnEntry {
        name: "$sinh",
        min_args: 1,
        max_args: Some(1),
        kind: SystemFnKind::RealMath1,
    },
    SystemFnEntry {
        name: "$cosh",
        min_args: 1,
        max_args: Some(1),
        kind: SystemFnKind::RealMath1,
    },
    SystemFnEntry {
        name: "$tanh",
        min_args: 1,
        max_args: Some(1),
        kind: SystemFnKind::RealMath1,
    },
    SystemFnEntry {
        name: "$asinh",
        min_args: 1,
        max_args: Some(1),
        kind: SystemFnKind::RealMath1,
    },
    SystemFnEntry {
        name: "$acosh",
        min_args: 1,
        max_args: Some(1),
        kind: SystemFnKind::RealMath1,
    },
    SystemFnEntry {
        name: "$atanh",
        min_args: 1,
        max_args: Some(1),
        kind: SystemFnKind::RealMath1,
    },
    // Real math functions (LRM 20.8.2) -- 2-arg
    SystemFnEntry {
        name: "$pow",
        min_args: 2,
        max_args: Some(2),
        kind: SystemFnKind::RealMath2,
    },
    SystemFnEntry {
        name: "$atan2",
        min_args: 2,
        max_args: Some(2),
        kind: SystemFnKind::RealMath2,
    },
    SystemFnEntry {
        name: "$hypot",
        min_args: 2,
        max_args: Some(2),
        kind: SystemFnKind::RealMath2,
    },
    // Array query functions (LRM 20.7)
    SystemFnEntry {
        name: "$left",
        min_args: 1,
        max_args: Some(2),
        kind: SystemFnKind::ReturnsInt,
    },
    SystemFnEntry {
        name: "$right",
        min_args: 1,
        max_args: Some(2),
        kind: SystemFnKind::ReturnsInt,
    },
    SystemFnEntry {
        name: "$low",
        min_args: 1,
        max_args: Some(2),
        kind: SystemFnKind::ReturnsInt,
    },
    SystemFnEntry {
        name: "$high",
        min_args: 1,
        max_args: Some(2),
        kind: SystemFnKind::ReturnsInt,
    },
    SystemFnEntry {
        name: "$size",
        min_args: 1,
        max_args: Some(2),
        kind: SystemFnKind::ReturnsInt,
    },
    SystemFnEntry {
        name: "$increment",
        min_args: 1,
        max_args: Some(2),
        kind: SystemFnKind::ReturnsInt,
    },
    SystemFnEntry {
        name: "$dimensions",
        min_args: 1,
        max_args: Some(1),
        kind: SystemFnKind::ReturnsInt,
    },
    SystemFnEntry {
        name: "$unpacked_dimensions",
        min_args: 1,
        max_args: Some(1),
        kind: SystemFnKind::ReturnsInt,
    },
    // Real conversion functions (LRM 20.5)
    SystemFnEntry {
        name: "$itor",
        min_args: 1,
        max_args: Some(1),
        kind: SystemFnKind::IntToReal,
    },
    SystemFnEntry {
        name: "$rtoi",
        min_args: 1,
        max_args: Some(1),
        kind: SystemFnKind::RealToInt,
    },
    SystemFnEntry {
        name: "$realtobits",
        min_args: 1,
        max_args: Some(1),
        kind: SystemFnKind::RealToBits,
    },
    SystemFnEntry {
        name: "$bitstoreal",
        min_args: 1,
        max_args: Some(1),
        kind: SystemFnKind::BitsToReal,
    },
    SystemFnEntry {
        name: "$shortrealtobits",
        min_args: 1,
        max_args: Some(1),
        kind: SystemFnKind::ShortRealToBits,
    },
    SystemFnEntry {
        name: "$bitstoshortreal",
        min_args: 1,
        max_args: Some(1),
        kind: SystemFnKind::BitsToShortReal,
    },
];

fn check_arity(args: &[TfArg], entry: &SystemFnEntry) -> Option<ExprType> {
    let count = args.len();
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

/// Look up a system function by name. Shared by inference and type-check.
pub(crate) fn lookup_builtin(name: &str) -> Option<&'static SystemFnEntry> {
    BUILTINS.iter().find(|e| e.name == name)
}

pub(crate) fn infer_system_call(stf: &SystemTfCall, ctx: &dyn InferCtx) -> ExprType {
    let Some(tok) = stf.system_name() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let name = tok.text();
    let Some(entry) = lookup_builtin(name) else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedSystemCall);
    };
    let Some(arg_list) = stf.arg_list() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let args: Vec<TfArg> = arg_list.args().collect();
    if let Some(err) = check_arity(&args, entry) {
        return err;
    }
    match entry.kind {
        SystemFnKind::Clog2 => infer_clog2(&args, ctx),
        SystemFnKind::Signed => infer_signedness_cast(&args, ctx, true),
        SystemFnKind::Unsigned => infer_signedness_cast(&args, ctx, false),
        SystemFnKind::Bits => infer_bits(),
        SystemFnKind::ReturnsInt => ExprType::from_ty(&crate::types::Ty::int()),
        SystemFnKind::ReturnsBit => ExprType::from_ty(&crate::types::Ty::bit(
            crate::types::PackedDims::empty(),
            false,
        )),
        SystemFnKind::RealMath1 | SystemFnKind::IntToReal | SystemFnKind::BitsToReal => {
            infer_one_arg_returns_real(&args, ctx)
        }
        SystemFnKind::RealMath2 => infer_two_arg_returns_real(&args, ctx),
        SystemFnKind::RealToInt => infer_one_arg_returns_int(&args, ctx),
        SystemFnKind::RealToBits => infer_one_arg_returns_bits(&args, ctx, 64),
        SystemFnKind::ShortRealToBits => infer_one_arg_returns_bits(&args, ctx, 32),
        SystemFnKind::BitsToShortReal => infer_one_arg_returns_shortreal(&args, ctx),
    }
}

fn infer_clog2(args: &[TfArg], ctx: &dyn InferCtx) -> ExprType {
    use crate::type_infer::{BitVecType, BitWidth, Signedness};
    if let Some(TfArg::Expr(e)) = args.first() {
        let _ = infer_expr(e, ctx, None);
    }
    ExprType::bitvec(BitVecType {
        width: BitWidth::Known(32),
        signed: Signedness::Signed,
        four_state: false,
    })
}

fn infer_bits() -> ExprType {
    use crate::types::Ty;
    ExprType::from_ty(&Ty::int())
}

fn infer_signedness_cast(args: &[TfArg], ctx: &dyn InferCtx, target_signed: bool) -> ExprType {
    use crate::type_infer::ExprView;
    use crate::types::{Integral, Ty};

    let Some(TfArg::Expr(e)) = args.first() else {
        return ExprType::error(ExprTypeErrorKind::UnsupportedExprKind);
    };
    let arg = infer_expr(e, ctx, None);
    if let ExprView::Error(_) = &arg.view {
        return arg;
    }
    let new_ty = match &arg.ty {
        Ty::Integral(i) => Ty::Integral(Integral {
            keyword: i.keyword,
            signed: target_signed,
            packed: i.packed.clone(),
        }),
        other => other.clone(),
    };
    ExprType::from_ty(&new_ty)
}

fn infer_one_arg_returns_real(args: &[TfArg], ctx: &dyn InferCtx) -> ExprType {
    use crate::types::{RealKw, Ty};
    if let Some(TfArg::Expr(e)) = args.first() {
        let _ = infer_expr(e, ctx, None);
    }
    ExprType::from_ty(&Ty::Real(RealKw::Real))
}

fn infer_two_arg_returns_real(args: &[TfArg], ctx: &dyn InferCtx) -> ExprType {
    use crate::types::{RealKw, Ty};
    if let Some(TfArg::Expr(e)) = args.first() {
        let _ = infer_expr(e, ctx, None);
    }
    if let Some(TfArg::Expr(e)) = args.get(1) {
        let _ = infer_expr(e, ctx, None);
    }
    ExprType::from_ty(&Ty::Real(RealKw::Real))
}

fn infer_one_arg_returns_int(args: &[TfArg], ctx: &dyn InferCtx) -> ExprType {
    use crate::types::Ty;
    if let Some(TfArg::Expr(e)) = args.first() {
        let _ = infer_expr(e, ctx, None);
    }
    ExprType::from_ty(&Ty::int())
}

fn infer_one_arg_returns_bits(args: &[TfArg], ctx: &dyn InferCtx, width: u32) -> ExprType {
    use crate::types::Ty;
    if let Some(TfArg::Expr(e)) = args.first() {
        let _ = infer_expr(e, ctx, None);
    }
    ExprType::from_ty(&Ty::bit_n(width))
}

fn infer_one_arg_returns_shortreal(args: &[TfArg], ctx: &dyn InferCtx) -> ExprType {
    use crate::types::{RealKw, Ty};
    if let Some(TfArg::Expr(e)) = args.first() {
        let _ = infer_expr(e, ctx, None);
    }
    ExprType::from_ty(&Ty::Real(RealKw::Short))
}

/// Semantic classification of a `$bits` argument.
pub(crate) enum BitsArgKind {
    Type(crate::types::Ty),
    Expr,
}

/// Classify a `$bits` argument as type-form or expr-form.
///
/// Takes a `TfArg` and a closure for type-namespace resolution so it works from
/// both `InferCtx` (inference) and `TypeCheckCtx` (validation) callers.
pub(crate) fn classify_bits_arg(
    arg: &TfArg,
    ast_id_map: &AstIdMap,
    resolve_type: &dyn Fn(&crate::type_extract::UserTypeRef) -> Option<crate::types::Ty>,
) -> BitsArgKind {
    match arg {
        TfArg::Type(tr) => {
            if let Some(utr) = crate::type_extract::user_type_ref_from_type_ref(tr)
                && let Some(ty) = resolve_type(&utr)
            {
                return BitsArgKind::Type(ty);
            }
            let ty = crate::extract_base_ty_from_type_ref(tr, ast_id_map);
            BitsArgKind::Type(ty)
        }
        TfArg::Expr(e) => {
            if let Some(ek) = e.classify()
                && matches!(ek, ExprKind::NameRef(_) | ExprKind::QualifiedName(_))
                && let Some(utr) = crate::type_extract::user_type_ref_from_expr(e)
                && let Some(ty) = resolve_type(&utr)
            {
                return BitsArgKind::Type(ty);
            }
            BitsArgKind::Expr
        }
        TfArg::Unknown(_) => BitsArgKind::Expr,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::Ty;
    use lyra_ast::AstNode;
    use lyra_lexer::SyntaxKind;
    use lyra_parser::SyntaxNode;

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

    fn arg_list_from(src: &str) -> lyra_ast::SystemTfArgList {
        let call = parse_system_call(src);
        let stf = SystemTfCall::cast(call).expect("should be SystemTfCall");
        stf.arg_list().expect("should have arg list")
    }

    #[test]
    fn iter_args_three() {
        let al = arg_list_from("module m; initial $foo(a, b, c); endmodule");
        assert_eq!(al.args().count(), 3);
    }

    #[test]
    fn iter_args_single_nameref() {
        let al = arg_list_from("module m; initial $foo(x); endmodule");
        let args: Vec<_> = al.args().collect();
        assert_eq!(args.len(), 1);
        assert!(matches!(args[0], TfArg::Expr(ref e) if e.kind() == SyntaxKind::NameRef));
    }

    #[test]
    fn iter_args_complex_exprs() {
        let al = arg_list_from("module m; initial $foo(a+b, f(x), x[i], x.y); endmodule");
        assert_eq!(al.args().count(), 4);
    }

    #[test]
    fn iter_args_sparse_commas() {
        let al = arg_list_from("module m; initial $foo(a,,b); endmodule");
        let count = al.args().count();
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
