use super::*;

fn dummy_ast_id() -> Site {
    let src = "module m; endmodule";
    let tokens = lyra_lexer::lex(src);
    let pp = lyra_preprocess::preprocess_identity(lyra_source::FileId(0), &tokens, src);
    let parse = lyra_parser::parse(&pp.tokens, &pp.expanded_text);
    let map = lyra_ast::AstIdMap::from_root(lyra_source::FileId(0), &parse.syntax());
    let first_child = parse.syntax().first_child().expect("parsed module");
    let ast_node: lyra_ast::ModuleDecl = lyra_ast::AstNode::cast(first_child).expect("module decl");
    map.ast_id(&ast_node).expect("ast_id").erase()
}

#[test]
fn integral_kw_logic() {
    assert!(IntegralKw::Logic.four_state());
    assert!(!IntegralKw::Logic.default_signed());
    assert_eq!(IntegralKw::Logic.base_width(), 1);
}

#[test]
fn integral_kw_reg() {
    assert!(IntegralKw::Reg.four_state());
    assert!(!IntegralKw::Reg.default_signed());
    assert_eq!(IntegralKw::Reg.base_width(), 1);
}

#[test]
fn integral_kw_bit() {
    assert!(!IntegralKw::Bit.four_state());
    assert!(!IntegralKw::Bit.default_signed());
    assert_eq!(IntegralKw::Bit.base_width(), 1);
}

#[test]
fn integral_kw_integer() {
    assert!(IntegralKw::Integer.four_state());
    assert!(IntegralKw::Integer.default_signed());
    assert_eq!(IntegralKw::Integer.base_width(), 32);
}

#[test]
fn integral_kw_int() {
    assert!(!IntegralKw::Int.four_state());
    assert!(IntegralKw::Int.default_signed());
    assert_eq!(IntegralKw::Int.base_width(), 32);
}

#[test]
fn integral_kw_shortint() {
    assert!(!IntegralKw::Shortint.four_state());
    assert!(IntegralKw::Shortint.default_signed());
    assert_eq!(IntegralKw::Shortint.base_width(), 16);
}

#[test]
fn integral_kw_longint() {
    assert!(!IntegralKw::Longint.four_state());
    assert!(IntegralKw::Longint.default_signed());
    assert_eq!(IntegralKw::Longint.base_width(), 64);
}

#[test]
fn integral_kw_byte() {
    assert!(!IntegralKw::Byte.four_state());
    assert!(IntegralKw::Byte.default_signed());
    assert_eq!(IntegralKw::Byte.base_width(), 8);
}

#[test]
fn integral_kw_time() {
    assert!(IntegralKw::Time.four_state());
    assert!(!IntegralKw::Time.default_signed());
    assert_eq!(IntegralKw::Time.base_width(), 64);
}

#[test]
fn const_int_known() {
    let c = ConstInt::Known(7);
    assert_eq!(c, ConstInt::Known(7));
}

#[test]
fn const_int_unevaluated() {
    let c = ConstInt::Unevaluated(dummy_ast_id());
    assert!(matches!(c, ConstInt::Unevaluated(_)));
}

#[test]
fn const_int_error() {
    let c = ConstInt::Error(ConstEvalError::DivideByZero);
    assert!(matches!(c, ConstInt::Error(ConstEvalError::DivideByZero)));
}

#[test]
fn const_int_known_ne_unevaluated() {
    assert_ne!(ConstInt::Known(7), ConstInt::Unevaluated(dummy_ast_id()));
}

#[test]
fn const_eval_error_variants() {
    let _ = ConstEvalError::NonConstant;
    let _ = ConstEvalError::DivideByZero;
    let _ = ConstEvalError::InvalidArgument;
    let _ = ConstEvalError::Overflow;
    let _ = ConstEvalError::Unresolved;
    let _ = ConstEvalError::Cycle;
    let _ = ConstEvalError::Unsupported;
    let _ = ConstEvalError::MissingSite(MissingSiteOrigin::TypeExtract);
    let _ = ConstEvalError::MissingSite(MissingSiteOrigin::ConstEval);
}

#[test]
fn packed_dim_width_normal() {
    let dim = PackedDim {
        msb: ConstInt::Known(7),
        lsb: ConstInt::Known(0),
    };
    assert_eq!(dim.try_width(), Some(8));
}

#[test]
fn packed_dim_width_reversed() {
    let dim = PackedDim {
        msb: ConstInt::Known(0),
        lsb: ConstInt::Known(7),
    };
    assert_eq!(dim.try_width(), Some(8));
}

#[test]
fn packed_dim_width_unevaluated() {
    let dim = PackedDim {
        msb: ConstInt::Unevaluated(dummy_ast_id()),
        lsb: ConstInt::Known(0),
    };
    assert_eq!(dim.try_width(), None);
}

#[test]
fn packed_dim_width_error() {
    let dim = PackedDim {
        msb: ConstInt::Error(ConstEvalError::Overflow),
        lsb: ConstInt::Known(0),
    };
    assert_eq!(dim.try_width(), None);
}

#[test]
fn packed_dim_width_extreme_bounds() {
    let dim = PackedDim {
        msb: ConstInt::Known(i64::MIN),
        lsb: ConstInt::Known(i64::MAX),
    };
    // Difference exceeds u32::MAX, so returns None without panicking.
    assert_eq!(dim.try_width(), None);
}

#[test]
fn unpacked_dim_size() {
    assert_eq!(
        UnpackedDim::Size(ConstInt::Known(256)).try_size(),
        Some(256)
    );
}

#[test]
fn unpacked_dim_range() {
    let dim = UnpackedDim::Range {
        msb: ConstInt::Known(3),
        lsb: ConstInt::Known(0),
    };
    assert_eq!(dim.try_size(), Some(4));
}

#[test]
fn unpacked_dim_unevaluated() {
    assert_eq!(
        UnpackedDim::Size(ConstInt::Unevaluated(dummy_ast_id())).try_size(),
        None
    );
}

#[test]
fn packed_width_single_dim() {
    let ty = Ty::logic(
        PackedDims::from(vec![PackedDim {
            msb: ConstInt::Known(7),
            lsb: ConstInt::Known(0),
        }]),
        false,
    );
    if let Ty::Integral(i) = &ty {
        assert_eq!(i.try_packed_width(), Some(8));
    } else {
        panic!("expected Integral");
    }
}

#[test]
fn packed_width_multi_dim() {
    let ty = Ty::logic(
        PackedDims::from(vec![
            PackedDim {
                msb: ConstInt::Known(7),
                lsb: ConstInt::Known(0),
            },
            PackedDim {
                msb: ConstInt::Known(3),
                lsb: ConstInt::Known(0),
            },
        ]),
        false,
    );
    if let Ty::Integral(i) = &ty {
        assert_eq!(i.try_packed_width(), Some(32));
    } else {
        panic!("expected Integral");
    }
}

#[test]
fn packed_width_no_dims() {
    if let Ty::Integral(i) = &Ty::simple_logic() {
        assert_eq!(i.try_packed_width(), Some(1));
    } else {
        panic!("expected Integral");
    }
}

#[test]
fn packed_width_unevaluated() {
    let ty = Ty::logic(
        PackedDims::from(vec![PackedDim {
            msb: ConstInt::Unevaluated(dummy_ast_id()),
            lsb: ConstInt::Known(0),
        }]),
        false,
    );
    if let Ty::Integral(i) = &ty {
        assert_eq!(i.try_packed_width(), None);
    } else {
        panic!("expected Integral");
    }
}

#[test]
fn pretty_int() {
    assert_eq!(Ty::int().pretty(), "int");
}

#[test]
fn pretty_simple_logic() {
    assert_eq!(Ty::simple_logic().pretty(), "logic");
}

#[test]
fn pretty_logic_unsigned_with_dim() {
    let ty = Ty::logic(
        PackedDims::from(vec![PackedDim {
            msb: ConstInt::Known(7),
            lsb: ConstInt::Known(0),
        }]),
        false,
    );
    assert_eq!(ty.pretty(), "logic [7:0]");
}

#[test]
fn pretty_logic_signed_with_dim() {
    let ty = Ty::logic(
        PackedDims::from(vec![PackedDim {
            msb: ConstInt::Known(7),
            lsb: ConstInt::Known(0),
        }]),
        true,
    );
    assert_eq!(ty.pretty(), "logic signed [7:0]");
}

#[test]
fn pretty_int_unsigned() {
    let ty = Ty::Integral(Integral {
        keyword: IntegralKw::Int,
        signed: false,
        packed: PackedDims::empty(),
    });
    assert_eq!(ty.pretty(), "int unsigned");
}

#[test]
fn pretty_error() {
    assert_eq!(Ty::Error.pretty(), "<error>");
}

#[test]
fn pretty_void() {
    assert_eq!(Ty::Void.pretty(), "void");
}

#[test]
fn pretty_real() {
    assert_eq!(Ty::Real(RealKw::Real).pretty(), "real");
    assert_eq!(Ty::Real(RealKw::Short).pretty(), "shortreal");
    assert_eq!(Ty::Real(RealKw::Time).pretty(), "realtime");
}

#[test]
fn pretty_string_chandle_event() {
    assert_eq!(Ty::String.pretty(), "string");
    assert_eq!(Ty::Chandle.pretty(), "chandle");
    assert_eq!(Ty::Event.pretty(), "event");
}

#[test]
fn pretty_unevaluated_dim() {
    let ty = Ty::logic(
        PackedDims::from(vec![PackedDim {
            msb: ConstInt::Unevaluated(dummy_ast_id()),
            lsb: ConstInt::Known(0),
        }]),
        false,
    );
    assert_eq!(ty.pretty(), "logic [?:0]");
}

#[test]
fn pretty_error_dim() {
    let ty = Ty::logic(
        PackedDims::from(vec![PackedDim {
            msb: ConstInt::Known(7),
            lsb: ConstInt::Error(ConstEvalError::Overflow),
        }]),
        false,
    );
    assert_eq!(ty.pretty(), "logic [7:!]");
}

#[test]
fn ty_int_eq_int() {
    assert_eq!(Ty::int(), Ty::int());
}

#[test]
fn ty_int_ne_integer() {
    assert_ne!(Ty::int(), Ty::integer());
}

#[test]
fn net_type_construct() {
    let _net = NetType {
        kind: NetKind::Wire,
        data: Ty::simple_logic(),
    };
}

#[test]
fn net_kind_ne() {
    assert_ne!(NetKind::Wire, NetKind::Tri);
}

#[test]
fn dimension_nesting_and_print_order() {
    // `int a [2][3]` -- outermost dim is [2], innermost is [3].
    // Nesting: Array(Array(int, Size(3)), Size(2))
    let inner = Ty::int();
    let with_3 = Ty::Array {
        elem: Box::new(inner),
        dim: UnpackedDim::Size(ConstInt::Known(3)),
    };
    let with_2_3 = Ty::Array {
        elem: Box::new(with_3),
        dim: UnpackedDim::Size(ConstInt::Known(2)),
    };
    // wrap_unpacked with outermost-first slice must produce the same nesting
    let from_wrap = wrap_unpacked(
        Ty::int(),
        &[
            UnpackedDim::Size(ConstInt::Known(2)),
            UnpackedDim::Size(ConstInt::Known(3)),
        ],
    );
    assert_eq!(with_2_3, from_wrap);
    // collect_array_dims returns outermost-first
    let (base, dims) = collect_array_dims(&with_2_3);
    assert_eq!(*base, Ty::int());
    assert_eq!(dims.len(), 2);
    assert_eq!(*dims[0], UnpackedDim::Size(ConstInt::Known(2)));
    assert_eq!(*dims[1], UnpackedDim::Size(ConstInt::Known(3)));
    // Pretty output matches source order: "int [2] [3]"
    assert_eq!(with_2_3.pretty(), "int [2] [3]");
}

#[test]
fn try_size_unsized_returns_none() {
    assert_eq!(UnpackedDim::Unsized.try_size(), None);
}

#[test]
fn try_size_queue_returns_none() {
    assert_eq!(UnpackedDim::Queue { bound: None }.try_size(), None);
    assert_eq!(
        UnpackedDim::Queue {
            bound: Some(ConstInt::Known(8))
        }
        .try_size(),
        None
    );
}

#[test]
fn try_size_assoc_wildcard_returns_none() {
    assert_eq!(UnpackedDim::Assoc(AssocIndex::Wildcard).try_size(), None);
}

#[test]
fn try_size_assoc_type_returns_none() {
    assert_eq!(
        UnpackedDim::Assoc(AssocIndex::Typed(Box::new(Ty::String))).try_size(),
        None
    );
}

#[test]
fn pretty_unsized_dim() {
    let ty = Ty::Array {
        elem: Box::new(Ty::int()),
        dim: UnpackedDim::Unsized,
    };
    assert_eq!(ty.pretty(), "int []");
}

#[test]
fn pretty_queue_no_bound() {
    let ty = Ty::Array {
        elem: Box::new(Ty::int()),
        dim: UnpackedDim::Queue { bound: None },
    };
    assert_eq!(ty.pretty(), "int [$]");
}

#[test]
fn pretty_queue_with_bound() {
    let ty = Ty::Array {
        elem: Box::new(Ty::int()),
        dim: UnpackedDim::Queue {
            bound: Some(ConstInt::Known(8)),
        },
    };
    assert_eq!(ty.pretty(), "int [$:8]");
}

#[test]
fn pretty_assoc_wildcard() {
    let ty = Ty::Array {
        elem: Box::new(Ty::int()),
        dim: UnpackedDim::Assoc(AssocIndex::Wildcard),
    };
    assert_eq!(ty.pretty(), "int [*]");
}

#[test]
fn pretty_assoc_type() {
    let ty = Ty::Array {
        elem: Box::new(Ty::int()),
        dim: UnpackedDim::Assoc(AssocIndex::Typed(Box::new(Ty::String))),
    };
    assert_eq!(ty.pretty(), "int [string]");
}

#[test]
fn pretty_assoc_error() {
    let ty = Ty::Array {
        elem: Box::new(Ty::int()),
        dim: UnpackedDim::Assoc(AssocIndex::Typed(Box::new(Ty::Error))),
    };
    assert_eq!(ty.pretty(), "int [<error>]");
}
