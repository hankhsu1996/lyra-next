use super::common::single;
use lyra_lexer::SyntaxKind;

#[test]
fn keywords_vs_identifiers() {
    assert_eq!(single("module").0, SyntaxKind::ModuleKw);
    assert_eq!(single("Module").0, SyntaxKind::Ident);
}

#[test]
fn all_original_keywords() {
    let cases = [
        ("module", SyntaxKind::ModuleKw),
        ("endmodule", SyntaxKind::EndmoduleKw),
        ("input", SyntaxKind::InputKw),
        ("output", SyntaxKind::OutputKw),
        ("inout", SyntaxKind::InoutKw),
        ("wire", SyntaxKind::WireKw),
        ("reg", SyntaxKind::RegKw),
        ("logic", SyntaxKind::LogicKw),
        ("assign", SyntaxKind::AssignKw),
        ("always", SyntaxKind::AlwaysKw),
        ("initial", SyntaxKind::InitialKw),
        ("begin", SyntaxKind::BeginKw),
        ("end", SyntaxKind::EndKw),
        ("if", SyntaxKind::IfKw),
        ("else", SyntaxKind::ElseKw),
        ("parameter", SyntaxKind::ParameterKw),
    ];
    for (src, expected) in cases {
        assert_eq!(single(src).0, expected, "failed for {src:?}");
    }
}

#[test]
fn new_keyword_samples() {
    let cases = [
        ("always_comb", SyntaxKind::AlwaysCombKw),
        ("always_ff", SyntaxKind::AlwaysFfKw),
        ("always_latch", SyntaxKind::AlwaysLatchKw),
        ("class", SyntaxKind::ClassKw),
        ("interface", SyntaxKind::InterfaceKw),
        ("package", SyntaxKind::PackageKw),
        ("function", SyntaxKind::FunctionKw),
        ("task", SyntaxKind::TaskKw),
        ("typedef", SyntaxKind::TypedefKw),
        ("enum", SyntaxKind::EnumKw),
        ("struct", SyntaxKind::StructKw),
        ("union", SyntaxKind::UnionKw),
        ("constraint", SyntaxKind::ConstraintKw),
        ("virtual", SyntaxKind::VirtualKw),
        ("static", SyntaxKind::StaticKw),
        ("automatic", SyntaxKind::AutomaticKw),
        ("generate", SyntaxKind::GenerateKw),
        ("localparam", SyntaxKind::LocalparamKw),
        ("foreach", SyntaxKind::ForeachKw),
        ("forever", SyntaxKind::ForeverKw),
        ("shortint", SyntaxKind::ShortintKw),
        ("longint", SyntaxKind::LongintKw),
        ("shortreal", SyntaxKind::ShortRealKw),
        ("chandle", SyntaxKind::ChandleKw),
        ("string", SyntaxKind::StringKw),
        ("null", SyntaxKind::NullKw),
        ("this", SyntaxKind::ThisKw),
        ("super", SyntaxKind::SuperKw),
        ("import", SyntaxKind::ImportKw),
        ("export", SyntaxKind::ExportKw),
    ];
    for (src, expected) in cases {
        assert_eq!(single(src).0, expected, "failed for {src:?}");
    }
}
