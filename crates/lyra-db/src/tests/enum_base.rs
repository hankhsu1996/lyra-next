use lyra_semantic::enum_def::EnumId;
use lyra_semantic::type_infer::{BitVecType, BitWidth, ExprView, Signedness};
use lyra_semantic::types::Ty;

use super::*;

fn enum_sem_for_first_enum(
    db: &dyn salsa::Database,
    file: SourceFile,
    unit: CompilationUnit,
) -> lyra_semantic::enum_def::EnumSem {
    let def = def_index_file(db, file);
    let first_enum = &def.enum_defs[0];
    let enum_id = EnumId::new(first_enum.enum_type_ast);
    let eref = EnumRef::new(db, unit, enum_id);
    enum_sem(db, eref)
}

#[test]
fn keyword_base_logic_8bit() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; enum logic [7:0] { A, B } x; endmodule");
    let unit = single_file_unit(&db, file);
    let sem = enum_sem_for_first_enum(&db, file, unit);
    assert_eq!(
        sem.base_int,
        Some(BitVecType {
            width: BitWidth::Known(8),
            signed: Signedness::Unsigned,
            four_state: true,
        })
    );
    assert!(sem.diags.is_empty());
}

#[test]
fn keyword_base_int() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; enum int { A, B } x; endmodule");
    let unit = single_file_unit(&db, file);
    let sem = enum_sem_for_first_enum(&db, file, unit);
    assert_eq!(
        sem.base_int,
        Some(BitVecType {
            width: BitWidth::Known(32),
            signed: Signedness::Signed,
            four_state: false,
        })
    );
    assert!(sem.diags.is_empty());
}

#[test]
fn keyword_base_bit_4() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; enum bit [3:0] { A, B } x; endmodule");
    let unit = single_file_unit(&db, file);
    let sem = enum_sem_for_first_enum(&db, file, unit);
    assert_eq!(
        sem.base_int,
        Some(BitVecType {
            width: BitWidth::Known(4),
            signed: Signedness::Unsigned,
            four_state: false,
        })
    );
    assert!(sem.diags.is_empty());
}

#[test]
fn default_base_is_int() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; enum { P, Q } w; endmodule");
    let unit = single_file_unit(&db, file);
    let sem = enum_sem_for_first_enum(&db, file, unit);
    assert_eq!(
        sem.base_int,
        Some(BitVecType {
            width: BitWidth::Known(32),
            signed: Signedness::Signed,
            four_state: false,
        })
    );
    assert!(sem.diags.is_empty());
}

#[test]
fn enum_ness_preserved_in_expr_type() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module m; enum { A, B } e; parameter P = e; endmodule",
    );
    let unit = single_file_unit(&db, file);

    let def = def_index_file(&db, file);
    let (sym_id, _) = def
        .symbols
        .iter()
        .find(|(_, s)| s.kind == lyra_semantic::symbols::SymbolKind::Parameter)
        .expect("should have a parameter");
    let decl_ast_id = def.symbols.get(sym_id).name_ast;
    let init_ast_id = def
        .name_ast_to_init_expr
        .get(&decl_ast_id)
        .expect("param should be tracked")
        .expect("param should have init");
    let expr_ref = ExprRef::new(&db, unit, init_ast_id);
    let et = type_of_expr(&db, expr_ref);

    assert!(
        matches!(et.view, ExprView::Plain),
        "enum expression should have Plain view, got {:?}",
        et.view
    );
    assert!(
        matches!(et.ty, Ty::Enum(_)),
        "enum expression should have Ty::Enum, got {:?}",
        et.ty
    );
}

#[test]
fn keyword_base_byte() {
    let db = LyraDatabase::default();
    let file = new_file(&db, 0, "module m; enum byte { A, B } x; endmodule");
    let unit = single_file_unit(&db, file);
    let sem = enum_sem_for_first_enum(&db, file, unit);
    assert_eq!(
        sem.base_int,
        Some(BitVecType {
            width: BitWidth::Known(8),
            signed: Signedness::Signed,
            four_state: false,
        })
    );
    assert!(sem.diags.is_empty());
}
