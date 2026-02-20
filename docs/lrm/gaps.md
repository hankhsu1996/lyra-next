# LRM Gaps

Known gaps between LRM requirements and current engine capabilities. This is the work queue for LRM signoff. See `docs/lrm-signoff.md` for methodology.

When you discover a gap during `/lrm-add`, add an entry here. When you fix the gap and add the passing test, remove the entry. Both changes land in the same PR.

## Chapter 26 -- Packages

### Qualified type in variable declaration (26.3)

`Pkg::type_t x;` as a variable declaration in module body causes parse errors. The parser does not recognize qualified names as types in that context. Qualified names work in expressions (e.g., `localparam int N = Pkg::SIZE;`) but not as the type in a declaration.

- **Blocked by:** parser (qualified name as type in variable/net declaration)
- **Tests to add:** `lrm/ch26/qualified_access` -- add qualified type declaration cases

### Header-level package import (26.4)

`module M import P::*; (...)` syntax is not parsed. The parser does not support `package_import_declaration` between the module name and port list.

- **Blocked by:** parser (import in module/interface/program header)
- **Tests to add:** `lrm/ch26/header_import` -- add actual header import syntax

### Package export declarations (26.6)

`export` declarations (`export P::*;`, `export *::*;`, `export P::name;`) are not parsed or analyzed. Imported declarations are not re-exported through subsequent imports.

- **Blocked by:** parser + semantic (export declaration parsing and transitive import visibility)
- **Tests to add:** `lrm/ch26/export_declarations`

### std built-in package (26.7)

The `std::` built-in package is not implemented. No implicit wildcard import of `std` into compilation units.

- **Blocked by:** semantic (built-in package registration and implicit import)
- **Tests to add:** `lrm/ch26/std_package`

