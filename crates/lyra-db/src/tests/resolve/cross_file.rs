use lyra_semantic::resolve_index::{CoreResolution, CoreResolveResult};
use lyra_semantic::symbols::Namespace;

use super::*;

#[test]
fn cross_file_module_instantiation() {
    let db = LyraDatabase::default();
    let file_a = new_file(
        &db,
        0,
        "module adder(input logic a, input logic b, output logic sum); assign sum = a + b; endmodule",
    );
    let file_b = new_file(
        &db,
        1,
        "module top; logic x, y, s; adder u1(.a(x), .b(y), .sum(s)); endmodule",
    );
    let unit = new_compilation_unit(&db, vec![file_a, file_b]);

    // Cursor on 'adder' in file_b's instantiation
    let text_b = file_b.text(&db);
    let adder_pos = text_b.find("adder").expect("should find 'adder'");
    let result = resolve_at(
        &db,
        file_b,
        unit,
        lyra_source::TextSize::new(adder_pos as u32),
    );
    assert!(result.is_some(), "'adder' should resolve cross-file");
    let sym_id = result.expect("checked above");
    assert_eq!(
        sym_id.file,
        lyra_source::FileId(0),
        "should resolve to file_a"
    );
    let sym = symbol_global(&db, unit, sym_id).expect("symbol should exist");
    assert_eq!(sym.name.as_str(), "adder");
    assert_eq!(sym.kind, lyra_semantic::symbols::SymbolKind::Module);
}

#[test]
fn unresolved_module_instantiation() {
    let db = LyraDatabase::default();
    let file = new_file(
        &db,
        0,
        "module top; logic x; nonexistent u1(.a(x)); endmodule",
    );
    let unit = single_file_unit(&db, file);
    let diags = file_diagnostics(&db, file, unit);
    let unresolved: Vec<_> = diags
        .iter()
        .filter(|d| {
            d.render_message().contains("unresolved") && d.render_message().contains("nonexistent")
        })
        .collect();
    assert!(
        !unresolved.is_empty(),
        "should have unresolved name diagnostic for 'nonexistent': {diags:?}"
    );
}

#[test]
fn no_lexical_fallback_for_definition_ns() {
    let db = LyraDatabase::default();
    // Local variable named 'adder' should NOT shadow a module instantiation
    let file = new_file(&db, 0, "module top; logic adder; adder u1(); endmodule");
    let unit = single_file_unit(&db, file);
    let diags = file_diagnostics(&db, file, unit);
    // The instantiation 'adder u1()' should be unresolved (no module 'adder' in unit)
    let unresolved: Vec<_> = diags
        .iter()
        .filter(|d| {
            let msg = d.render_message();
            msg.contains("unresolved") && msg.contains("adder")
        })
        .collect();
    assert!(
        !unresolved.is_empty(),
        "module instantiation should not fall back to lexical scope: {diags:?}"
    );
}

#[test]
fn duplicate_module_definition() {
    let db = LyraDatabase::default();
    let file_a = new_file(&db, 0, "module foo; endmodule");
    let file_b = new_file(&db, 1, "module foo; endmodule");
    let unit = new_compilation_unit(&db, vec![file_a, file_b]);
    let diags = unit_diagnostics(&db, unit);
    let dup_diags: Vec<_> = diags
        .iter()
        .filter(|d| d.render_message().contains("duplicate definition"))
        .collect();
    assert!(
        !dup_diags.is_empty(),
        "should have duplicate definition diagnostic: {diags:?}"
    );
}

#[test]
fn file_addition_triggers_resolution() {
    let db = LyraDatabase::default();
    // Initially, file_b has unresolved 'adder'
    let file_b = new_file(&db, 1, "module top; logic x; adder u1(.a(x)); endmodule");
    let unit = new_compilation_unit(&db, vec![file_b]);
    let diags = file_diagnostics(&db, file_b, unit);
    assert!(
        diags
            .iter()
            .any(|d| d.render_message().contains("unresolved")),
        "adder should be unresolved initially"
    );

    // Add file_a with module adder
    let file_a = new_file(&db, 0, "module adder(input logic a); endmodule");
    let unit2 = new_compilation_unit(&db, vec![file_a, file_b]);
    let diags2 = file_diagnostics(&db, file_b, unit2);
    let unresolved: Vec<_> = diags2
        .iter()
        .filter(|d| {
            let msg = d.render_message();
            msg.contains("unresolved") && msg.contains("adder")
        })
        .collect();
    assert!(
        unresolved.is_empty(),
        "adder should resolve after adding file_a: {diags2:?}"
    );
}

#[test]
fn pkg_member_resolves_to_pkg_variant() {
    let db = LyraDatabase::default();
    let file_a = new_file(&db, 0, "package P; int x; endpackage");
    let file_b = new_file(&db, 1, "module m; import P::x; int y = x; endmodule");
    let unit = new_compilation_unit(&db, vec![file_a, file_b]);

    let core = resolve_core_file(&db, file_b, unit);
    let def_b = def_index_file(&db, file_b);

    // Find the use-site for 'x' in 'int y = x'
    let x_use_idx = def_b
        .use_sites
        .iter()
        .position(|us| us.path.display_name() == "x")
        .expect("should have a use-site for 'x'");

    let result = &core.resolutions[x_use_idx];
    match result {
        CoreResolveResult::Resolved(CoreResolution::Pkg {
            name_ast,
            namespace,
        }) => {
            assert_eq!(
                *namespace,
                Namespace::Value,
                "package member should resolve in value namespace"
            );
            // The anchor should be in file_a (the package file)
            assert_eq!(
                name_ast.file(),
                lyra_source::FileId(0),
                "anchor should point to the package file"
            );
            // Verify name_ast_to_symbol lookup works for this anchor
            let def_a = def_index_file(&db, file_a);
            let local = def_a
                .name_ast_to_symbol
                .get(name_ast)
                .expect("name_ast_to_symbol should find the anchor");
            let sym = def_a.symbols.get(*local);
            assert_eq!(sym.name.as_str(), "x");
            // Verify symbol_at_name_ast also works
            let gsym = symbol_at_name_ast(&db, unit, *name_ast).expect("should resolve via query");
            assert_eq!(gsym.file, lyra_source::FileId(0));
            assert_eq!(gsym.local, *local);
        }
        other => panic!("expected CoreResolution::Pkg, got {other:?}"),
    }
}

#[test]
fn def_resolves_to_def_variant() {
    let db = LyraDatabase::default();
    let file_a = new_file(&db, 0, "module adder; endmodule");
    let file_b = new_file(&db, 1, "module top; adder u1(); endmodule");
    let unit = new_compilation_unit(&db, vec![file_a, file_b]);

    let core = resolve_core_file(&db, file_b, unit);
    let def_b = def_index_file(&db, file_b);

    // Find the use-site for the 'adder' instantiation type name
    let adder_use_idx = def_b
        .use_sites
        .iter()
        .position(|us| us.path.display_name() == "adder")
        .expect("should have a use-site for 'adder'");

    let result = &core.resolutions[adder_use_idx];
    match result {
        CoreResolveResult::Resolved(CoreResolution::Def { def }) => {
            let anchor = def.ast_id();
            // The def anchor should be in file_a
            assert_eq!(
                anchor.file(),
                lyra_source::FileId(0),
                "def anchor should point to file_a"
            );
            // Verify symbol_at_name_ast resolves this
            let gsym = symbol_at_name_ast(&db, unit, anchor).expect("should resolve def anchor");
            assert_eq!(gsym.file, lyra_source::FileId(0));
            let def_a = def_index_file(&db, file_a);
            let sym = def_a.symbols.get(gsym.local);
            assert_eq!(sym.name.as_str(), "adder");
            assert_eq!(sym.kind, lyra_semantic::symbols::SymbolKind::Module);
        }
        other => panic!("expected CoreResolution::Def, got {other:?}"),
    }
}
