/// Canonical anchor extraction for semantic structs.
///
/// `Site` is a transparent alias for `ErasedAstId`. All semantic code
/// (outside builder modules) obtains `Site` values exclusively through
/// the helpers in this module. No other non-builder semantic file should
/// import `HasSyntax` or call `AstIdMap::id_of` directly.
use lyra_ast::{AstIdMap, HasSyntax};

pub type Site = lyra_ast::ErasedAstId;

/// Get a stable site anchor. Returns `None` on error-recovered trees.
pub(crate) fn opt_site_of<T: HasSyntax>(map: &AstIdMap, node: &T) -> Option<Site> {
    map.id_of(node)
}

/// Get a stable site anchor, falling back on failure.
pub(crate) fn site_of<T: HasSyntax>(map: &AstIdMap, node: &T, fallback: Site) -> Site {
    map.id_of(node).unwrap_or(fallback)
}

#[cfg(test)]
mod tests {
    use super::*;
    use lyra_ast::{AstNode, ContinuousAssign};
    use lyra_source::FileId;

    fn parse_and_map(src: &str) -> (lyra_parser::Parse, AstIdMap) {
        let tokens = lyra_lexer::lex(src);
        let parse = lyra_parser::parse(&tokens, src);
        let map = AstIdMap::from_root(FileId(0), &parse.syntax());
        (parse, map)
    }

    #[test]
    fn peeled_expr_anchor_differs_from_wrapper() {
        let src = "module m; assign x = ((a + b)); endmodule";
        let (parse, map) = parse_and_map(src);
        let root = parse.syntax();

        let ca = root
            .descendants()
            .find_map(ContinuousAssign::cast)
            .expect("ContinuousAssign");
        let rhs = ca.rhs().expect("rhs expr");

        let wrapper_site = opt_site_of(&map, &rhs);
        let peeled = rhs.peeled().expect("peeled");
        let inner_site = opt_site_of(&map, &peeled);

        assert!(wrapper_site.is_some());
        assert!(inner_site.is_some());
        assert_ne!(
            wrapper_site, inner_site,
            "peeled site should differ from wrapper"
        );
    }

    #[test]
    fn non_expr_typed_node_anchor() {
        let src = "module m; assign x = 1; endmodule";
        let (parse, map) = parse_and_map(src);
        let root = parse.syntax();

        let ca = root
            .descendants()
            .find_map(ContinuousAssign::cast)
            .expect("ContinuousAssign");

        let site = opt_site_of(&map, &ca);
        assert!(site.is_some(), "ContinuousAssign should have a site");
    }
}
