# LRM Gaps

Known gaps between LRM requirements and current engine capabilities. This is the work queue for LRM signoff. See `docs/lrm-signoff.md` for methodology.

When you discover a gap during `/lrm-add`, add an entry here. When you fix the gap and add the passing test, remove the entry. Both changes land in the same PR.

## Chapter 6 -- Data Types

### 6.19: Enum named base type

User-defined type names as enum base types (`enum my_base_t { ... }`) are not parsed -- the parser only recognizes keyword types (`int`, `logic`, `byte`, etc.) as enum base types. Keyword base types with dimensions and signing (`enum logic [2:0] { ... }`, `enum int unsigned { ... }`) parse and are stored correctly. Blocked by: parser extension for `type_identifier` in `enum_base_type`. Test: `lrm/ch06/enum_named_base`.

### 6.19.3: Enum ranges

`name[N]` and `name[N:M]` range syntax for auto-generating variant names is not supported by the parser. Test: `lrm/ch06/enum_ranges`.

### 6.19.4: Enum methods

`.first()`, `.last()`, `.next()`, `.prev()`, `.num()`, `.name()` are not implemented. Blocked by: method resolution infrastructure. Test: `lrm/ch06/enum_methods`.

### 6.19.5: Enum type compatibility

Assignment between different enum types, enum-to-integral casting rules are not checked. Test: `lrm/ch06/enum_type_compat`.

### 6.19: Enum member visibility vs local declaration ordering

Enum member names are injected into the enclosing scope unconditionally. The LRM's ordering/visibility rules for when a name becomes visible vs later local declarations are not modeled. Collisions between enum members and other declarations are diagnosed as duplicates under the current model. This is the same class of problem as Ch26 wildcard import ordering. Test: `lrm/ch06/enum_member_ordering`.

## Chapter 7 -- Aggregate Types

### 7.4: Dynamic arrays, queues, and associative arrays

`UnpackedDim` only represents fixed-size dimensions. No dynamic (`[]`), queue (`[$]`), or associative (`[string]`) dims. Blocked by: unpacked dim model extension. Tests: `lrm/ch07/dynamic_array`, `lrm/ch07/queue`, `lrm/ch07/assoc_array`.

### 7.4.6: Array slicing

`a[i:j]` on unpacked arrays is not handled. Part-select currently applies only to packed integral types. Blocked by: unpacked array slice semantics. Test: `lrm/ch07/array_slice`.

## Chapter 7 -- Structures and Unions

### 7.3.1: Soft packed unions

The `soft` qualifier is not recognized by the parser. `SoftKw` is lexed but not handled in `struct_type()`. Blocked by: parser extension. Test: `lrm/ch07/soft_packed_union`.

### 7.3.1: Packed union width/shape constraints

Packed union members must satisfy LRM compatibility rules for packed widths. No width validation is performed. Blocked by: width computation on record types. Test: `lrm/ch07/packed_union_width`.

### 7.3.2: Tagged unions

The `tagged` qualifier is parsed and diagnosed as unsupported; the type resolves to error. Full support requires tagged expressions (11.9), pattern matching (12.6), tag tracking, and void members. `RecordKind::TaggedUnion` exists but is never constructed. Test: `lrm/ch07/tagged_union`.

## Chapter 11 -- Operators and Expressions

### 11.4.14: Streaming operators

`{>>{ }}` and `{<<{ }}` are not parsed or typed. Blocked by: parser and type inference extension. Test: `lrm/ch11/streaming_operators`.

### 11.5.1: Fixed part-select with non-constant bounds

Engine currently requires constant bounds for `[hi:lo]` fixed part-select. Supporting non-constant bounds needs symbolic/dynamic width representation. Blocked by: dynamic width model. Test: `lrm/ch11/part_select_nonconstant`.

### 11.5.1: Bit-select and part-select signedness

Bit-select and part-select currently return unsigned (engine policy). LRM signedness rules for these in different contexts need verification. Blocked by: signedness rule audit. Test: `lrm/ch11/select_signedness`.

## Chapter 25 -- Interfaces

### 25.5.4: Modport type extraction to dedicated module

Modport types (`ModportDef`, `ModportDefIdx`, `ModportId`, etc.) are co-located with struct/union types in `record.rs`. For consistency with the `enum_def.rs` extraction, modport types should be moved to a dedicated `modport_def.rs` module. Blocked by: refactoring (no functional gap). Test: N/A (internal code organization).

## Chapter 26 -- Packages

### 26.3: Enum literal import semantics

Importing an enum type does not import its enumeration literals (LRM 26.3 teeth_t example). Importing `teeth_t` from package `q` does not make `FALSE` visible -- a bare reference to `FALSE` still resolves to `p::FALSE` via a wildcard import of `p`. Not tested. Blocked by: semantic (enum literal visibility tracking). Test: `lrm/ch26/enum_literal_import`.

### 26.7: std package contents (Annex G)

The std built-in package should contain process, mailbox, and semaphore classes per Annex G. Not tested. Blocked by: class support (Ch 8). Test: deferred until class support lands.
