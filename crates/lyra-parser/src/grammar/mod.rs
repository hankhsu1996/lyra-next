mod declarations;
mod expressions;
mod items;
mod ports;
mod statements;

use lyra_lexer::SyntaxKind;

use crate::parser::Parser;

pub(crate) fn source_file(p: &mut Parser) {
    let m = p.start();
    while !p.at_end() {
        if p.at(SyntaxKind::ModuleKw) {
            items::module_decl(p);
        } else if p.at(SyntaxKind::PackageKw) {
            items::package_decl(p);
        } else if p.at(SyntaxKind::InterfaceKw) {
            items::interface_decl(p);
        } else if p.at(SyntaxKind::ProgramKw) {
            items::program_decl(p);
        } else if p.at(SyntaxKind::PrimitiveKw) {
            items::primitive_decl(p);
        } else if p.at(SyntaxKind::ConfigKw) {
            items::config_decl(p);
        } else {
            p.error_bump("expected top-level declaration");
        }
    }
    // Trailing trivia must be inside the root node for a valid rowan tree.
    p.eat_remaining_trivia();
    m.complete(p, SyntaxKind::SourceFile);
}
