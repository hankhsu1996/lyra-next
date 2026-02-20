use super::*;

#[test]
fn sig_ansi_ports_with_directions() {
    let src =
        "module adder(input logic [7:0] a, input logic [7:0] b, output logic [8:0] sum); endmodule";
    let ports = sig_ports(src, "adder");
    assert_eq!(ports.len(), 3);
    assert!(ports[0].starts_with("a:"), "got: {}", ports[0]);
    assert!(ports[1].starts_with("b:"), "got: {}", ports[1]);
    assert!(ports[2].starts_with("sum:"), "got: {}", ports[2]);
}

#[test]
fn sig_preserves_source_order() {
    let src = "module m(input logic z, input logic a, output logic m_out); endmodule";
    let ports = sig_ports(src, "m");
    assert_eq!(ports[0].split(':').next(), Some("z"));
    assert_eq!(ports[1].split(':').next(), Some("a"));
    assert_eq!(ports[2].split(':').next(), Some("m_out"));
}

#[test]
fn sig_params() {
    let db = LyraDatabase::default();
    let src =
        "module m #(parameter int W = 8, parameter int D = 4)(input logic [W-1:0] a); endmodule";
    let file = new_file(&db, 0, src);
    let unit = single_file_unit(&db, file);
    let global = crate::semantic::global_def_index(&db, unit);
    let def_id = global.resolve_module("m").expect("module not found");
    let mref = crate::elab_queries::DesignUnitRef::new(&db, unit, def_id);
    let sig = design_unit_signature(&db, mref);
    assert_eq!(sig.params.len(), 2);
    assert_eq!(sig.params[0].name, "W");
    assert_eq!(sig.params[1].name, "D");
    assert!(sig.params[0].has_default);
}
