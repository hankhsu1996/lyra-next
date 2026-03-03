use super::*;

#[test]
fn type_of_nettype_define_logic() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; nettype logic my_net; my_net x; endmodule",
    );
    let unit = single_file_unit(&db, file);

    let td = get_type(&db, file, unit, "my_net");
    match td {
        SymbolType::TypeAlias(Ty::Integral(ref i)) => {
            assert_eq!(i.keyword, IntegralKw::Logic);
            assert!(i.packed.is_empty());
        }
        other => panic!("expected TypeAlias(Integral(logic)), got {other:?}"),
    }

    let var = get_type(&db, file, unit, "x");
    match var {
        SymbolType::Value(Ty::Integral(ref i)) => {
            assert_eq!(i.keyword, IntegralKw::Logic);
            assert!(i.packed.is_empty());
        }
        other => panic!("expected Value(Integral(logic)) via nettype, got {other:?}"),
    }
}

#[test]
fn type_of_nettype_define_packed() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; nettype logic [7:0] my_byte; my_byte x; endmodule",
    );
    let unit = single_file_unit(&db, file);

    let td = get_type(&db, file, unit, "my_byte");
    match td {
        SymbolType::TypeAlias(Ty::Integral(ref i)) => {
            assert_eq!(i.keyword, IntegralKw::Logic);
            assert_eq!(i.packed.len(), 1);
            assert_eq!(i.packed[0].msb, ConstInt::Known(7));
            assert_eq!(i.packed[0].lsb, ConstInt::Known(0));
        }
        other => panic!("expected TypeAlias(Integral(logic [7:0])), got {other:?}"),
    }

    let var = get_type(&db, file, unit, "x");
    match var {
        SymbolType::Value(Ty::Integral(ref i)) => {
            assert_eq!(i.keyword, IntegralKw::Logic);
            assert_eq!(i.packed.len(), 1);
            assert_eq!(i.packed[0].msb, ConstInt::Known(7));
            assert_eq!(i.packed[0].lsb, ConstInt::Known(0));
        }
        other => panic!("expected Value(Integral(logic [7:0])), got {other:?}"),
    }
}

#[test]
fn type_of_nettype_alias() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; nettype logic [7:0] base_net; nettype base_net alias_net; alias_net x; endmodule",
    );
    let unit = single_file_unit(&db, file);

    let alias_td = get_type(&db, file, unit, "alias_net");
    match alias_td {
        SymbolType::TypeAlias(Ty::Integral(ref i)) => {
            assert_eq!(i.keyword, IntegralKw::Logic);
            assert_eq!(i.packed.len(), 1);
            assert_eq!(i.packed[0].msb, ConstInt::Known(7));
        }
        other => panic!("expected TypeAlias(Integral) via alias chain, got {other:?}"),
    }

    let var = get_type(&db, file, unit, "x");
    match var {
        SymbolType::Value(Ty::Integral(ref i)) => {
            assert_eq!(i.keyword, IntegralKw::Logic);
            assert_eq!(i.packed.len(), 1);
            assert_eq!(i.packed[0].msb, ConstInt::Known(7));
        }
        other => panic!("expected Value(Integral) via nettype alias, got {other:?}"),
    }
}

#[test]
fn type_of_nettype_integer() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; nettype integer my_int_net; my_int_net x; endmodule",
    );
    let unit = single_file_unit(&db, file);

    let var = get_type(&db, file, unit, "x");
    match var {
        SymbolType::Value(Ty::Integral(ref i)) => {
            assert_eq!(i.keyword, IntegralKw::Integer);
        }
        other => panic!("expected Value(Integral(integer)), got {other:?}"),
    }
}

#[test]
fn type_of_nettype_cross_package() {
    let db = LyraDatabase::default();
    let pkg = new_file(
        &db,
        0,
        "package pkg; nettype logic [7:0] my_net; endpackage",
    );
    let mod_file = new_file(&db, 1, "module m; import pkg::*; my_net x; endmodule");
    let unit = new_compilation_unit(&db, vec![pkg, mod_file]);

    let var = get_type(&db, mod_file, unit, "x");
    match var {
        SymbolType::Value(Ty::Integral(ref i)) => {
            assert_eq!(i.keyword, IntegralKw::Logic);
            assert_eq!(i.packed.len(), 1);
            assert_eq!(i.packed[0].msb, ConstInt::Known(7));
            assert_eq!(i.packed[0].lsb, ConstInt::Known(0));
        }
        other => panic!("expected Value(Integral) via cross-file nettype, got {other:?}"),
    }
}

#[test]
fn type_of_interconnect_display() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; interconnect a; endmodule");
    let unit = single_file_unit(&db, file);
    let ty = get_type(&db, file, unit, "a");
    match &ty {
        SymbolType::Net(net) => {
            assert_eq!(net.kind, NetKind::Interconnect);
        }
        other => panic!("expected Net(Interconnect), got {other:?}"),
    }
    assert_eq!(ty.pretty().as_str(), "interconnect");
}
