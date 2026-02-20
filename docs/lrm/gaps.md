# LRM Gaps

Known gaps between LRM requirements and current engine capabilities. This is the work queue for LRM signoff. See `docs/lrm-signoff.md` for methodology.

When you discover a gap during `/lrm-add`, add an entry here. When you fix the gap and add the passing test, remove the entry. Both changes land in the same PR.

## Chapter 6 -- Data Types

### 6.19: Enum base type resolution

Named base types (`enum logic [2:0] { ... }`, `enum my_base_t { ... }`) are stored as `TypeRef` but not resolved to a semantic type. The base type affects variant width and signedness. Blocked by: enum semantic query. Test: `lrm/ch06/enum_named_base`.

### 6.19.3: Enum ranges

`name[N]` and `name[N:M]` range syntax for auto-generating variant names is not supported by the parser. Test: `lrm/ch06/enum_ranges`.

### 6.19.4: Enum methods

`.first()`, `.last()`, `.next()`, `.prev()`, `.num()`, `.name()` are not implemented. Blocked by: method resolution infrastructure. Test: `lrm/ch06/enum_methods`.

### 6.19.5: Enum type compatibility

Assignment between different enum types, enum-to-integral casting rules are not checked. Test: `lrm/ch06/enum_type_compat`.

### 6.19: Enum member visibility vs local declaration ordering

Enum member names are injected into the enclosing scope unconditionally. The LRM's ordering/visibility rules for when a name becomes visible vs later local declarations are not modeled. Collisions between enum members and other declarations are diagnosed as duplicates under the current model. This is the same class of problem as Ch26 wildcard import ordering. Test: `lrm/ch06/enum_member_ordering`.

## Chapter 7 -- Structures and Unions

### 7.3.1: Soft packed unions

The `soft` qualifier is not recognized by the parser. `SoftKw` is lexed but not handled in `struct_type()`. Blocked by: parser extension. Test: `lrm/ch07/soft_packed_union`.

### 7.3.1: Packed union width/shape constraints

Packed union members must satisfy LRM compatibility rules for packed widths. No width validation is performed. Blocked by: width computation on record types. Test: `lrm/ch07/packed_union_width`.

### 7.3.2: Tagged unions

The `tagged` qualifier is parsed and diagnosed as unsupported; the type resolves to error. Full support requires tagged expressions (11.9), pattern matching (12.6), tag tracking, and void members. `RecordKind::TaggedUnion` exists but is never constructed. Test: `lrm/ch07/tagged_union`.

## Chapter 26 -- Packages

### 26.3: Enum literal import semantics

Importing an enum type does not import its enumeration literals (LRM 26.3 teeth_t example). Importing `teeth_t` from package `q` does not make `FALSE` visible -- a bare reference to `FALSE` still resolves to `p::FALSE` via a wildcard import of `p`. Not tested. Blocked by: semantic (enum literal visibility tracking). Test: `lrm/ch26/enum_literal_import`.

### 26.3: Wildcard import scoping across generate blocks

LRM 26.3 Examples 1-4 define complex lexical scoping rules for wildcard imports: imports in outer scopes affect inner generate blocks, positional (before/after) matters, and function/task calls search to end of scope. Not tested, likely not implemented. Blocked by: semantic (positional wildcard import resolution). Tests: `lrm/ch26/wildcard_scope_blocks`.

### 26.3: Later local declaration conflicts with wildcard-imported name

"If a wildcard imported symbol is made locally visible in a scope, any later locally visible declaration of the same name in that scope shall be illegal" (LRM 26.3 Example 1 line 5). Error case not tested. Blocked by: semantic (conflict detection between wildcard import and later local decl). Test: `lrm/ch26/wildcard_local_conflict`.

### 26.7: std package contents (Annex G)

The std built-in package should contain process, mailbox, and semaphore classes per Annex G. Not tested. Blocked by: class support (Ch 8). Test: deferred until class support lands.
