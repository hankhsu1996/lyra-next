# LRM Gaps

Known gaps between LRM requirements and current engine capabilities. This is the work queue for LRM signoff. See `docs/lrm-signoff.md` for methodology.

When you discover a gap during `/lrm-add`, add an entry here. When you fix the gap and add the passing test, remove the entry. Both changes land in the same PR.

## Chapter 6 -- Data Types

### 6.19.4: Enum methods

`.first()`, `.last()`, `.next()`, `.prev()`, `.num()`, `.name()` are not implemented. Blocked by: method resolution infrastructure. Test: `lrm/ch06/enum_methods`.

### 6.19.5: Enum type compatibility -- casts and value-set membership

Enum assignment requires identical enum type on LHS/RHS (integral-to-enum and cross-enum assignments are diagnosed). Missing: explicit casts (`Colors'(5)`) making assignment legal (cast expressions not parsed); value-set membership / range checking (needs const-eval). Test: `lrm/ch06/enum_type_compat`.

### 6.19: Enum member visibility vs local declaration ordering

Enum member names are injected into the enclosing scope unconditionally. The LRM's ordering/visibility rules for when a name becomes visible vs later local declarations are not modeled. Collisions between enum members and other declarations are diagnosed as duplicates under the current model. This is the same class of problem as Ch26 wildcard import ordering. Test: `lrm/ch06/enum_member_ordering`.

## Chapter 7 -- Aggregate Types

### 7.4: Dynamic arrays, queues, and associative arrays -- behavioral rules

Type representation now handles all unpacked dimension forms (dynamic `[]`, queue `[$]`/`[$:N]`, associative `[*]`/`[string]`). Remaining gaps: assignment compatibility between array types, built-in array methods (`.size()`, `.delete()`, `.push_back()`, etc.), `new[]` constructor, `foreach` iteration, and array query system functions. Tests for type representation: `crates/lyra-db/src/tests/type_of/dims.rs`.

### 7.4.6: Array slicing

`a[i:j]` on unpacked arrays is not handled. Part-select currently applies only to packed integral types. Blocked by: unpacked array slice semantics. Test: `lrm/ch07/array_slice`.

## Chapter 7 -- Structures and Unions

### 7.3.1: Soft packed union layout semantics

Width computation (`bit_width_total`) handles packed records internally (struct=sum, union=max). No observable SV-level behavior depends on it yet -- blocked by `$bits` const-eval (gap 20.6.2). Right-justified member placement and layout mapping are not modeled. Test: `lrm/ch07/soft_packed_union_layout`.

### 7.3.2: Tagged unions

The `tagged` qualifier is parsed and diagnosed as unsupported; the type resolves to error. Full support requires tagged expressions (11.9), pattern matching (12.6), tag tracking, and void members. `RecordKind::TaggedUnion` exists but is never constructed. Test: `lrm/ch07/tagged_union`.

## Chapter 11 -- Operators and Expressions

### 11.4.14: Streaming `with` array range

The `with [array_range_expression]` construct on stream expressions is not parsed. Blocked by: parser extension + dynamic array support (7.4). Test: `lrm/ch11/streaming_with`.

### 11.4.14: Streaming assignment target (unpack)

Streaming operators on the LHS of assignments (unpack semantics) are not handled. The parser accepts the syntax but semantic checking (width validation, dynamic resizing) is not implemented. Blocked by: assignment target analysis. Test: `lrm/ch11/streaming_unpack`.

### 11.4.14.1: Non-integral operand bitstream conversion

Streaming of arrays, structs, unions, and strings follows recursive bitstream conversion rules (LRM 11.4.14.1). Only integral operands are supported. Blocked by: bitstream type conversion. Test: `lrm/ch11/streaming_aggregate`.

### 11.4.14: Streaming slice_size const validation

Parser accepts any expression as `slice_size`. The LRM requires it to be a constant expression. Constantness is not enforced. Blocked by: general constant expression validation. Test: N/A (part of const-eval signoff).

### 11.5.1: Fixed part-select with non-constant bounds

Engine currently requires constant bounds for `[hi:lo]` fixed part-select. Supporting non-constant bounds needs symbolic/dynamic width representation. Blocked by: dynamic width model. Test: `lrm/ch11/part_select_nonconstant`.

### 11.5.1: Bit-select and part-select signedness

Bit-select and part-select currently return unsigned (engine policy). LRM signedness rules for these in different contexts need verification. Blocked by: signedness rule audit. Test: `lrm/ch11/select_signedness`.

## Chapter 20 -- System Functions

### 20.6.2: $bits constant evaluation

`$bits` returns `int` for typing but the actual value is not computed at const-eval time. `bit_width_total` query exists (v1: Integral and Real only). Extending it to walk Record fields, Enum base types, and Array dimensions requires recursing via `record_sem` and enum queries. Blocked by: const_eval extension for compound types. Test: `lrm/ch20/bits_consteval`.

### 20.7: Array query functions

`$size`, `$left`, `$right`, `$low`, `$high`, `$increment`, `$dimensions`, `$unpacked_dimensions` not implemented. Need dimension navigation on `Ty` and optional second argument. Test: `lrm/ch20/array_query`.

## Chapter 25 -- Interfaces

### 25.3.3: Generic interface reference

Module ports declared with bare `interface` keyword as type (generic interface). Parser and semantic layer do not recognize this syntax. Test: `lrm/ch25/generic_interface`.

### 25.5: Modport direction enforcement

Accessing modport members should enforce direction. Writing to an `input` modport member or reading an `output`-only member is not diagnosed. Test: `lrm/ch25/modport_direction_check`.

### 25.5: Modport conflict detection

When modport is specified at both instantiation and module header, they must be identical. Not checked. Test: `lrm/ch25/modport_conflict`.

### 25.5.4: Modport expressions

`.port_identifier(expression)` syntax in modport declarations. Parser does not handle this form. Test: `lrm/ch25/modport_expressions`.

### 25.5.5: Clocking blocks in modports

`modport_clocking_declaration` syntax. No clocking block support. Test: `lrm/ch25/modport_clocking`.

### 25.6: Interfaces and specify blocks

Specify block terminal rules for interface ports. No specify block support. Test: `lrm/ch25/specify_interface`.

### 25.7: Tasks and functions in interfaces

Import/export of subroutines through modports. Neither parsing nor semantic analysis handles `modport_tf_ports_declaration`. Test: `lrm/ch25/interface_methods`.

### 25.7.4: Extern forkjoin tasks

`extern forkjoin task` declaration in interfaces. Not supported. Test: `lrm/ch25/extern_forkjoin`.

### 25.8: Parameterized interfaces

Interface parameter override at instantiation site. Parser/elaboration may partially work but untested in LRM corpus. Test: `lrm/ch25/parameterized_interface`.

### 25.9: Virtual interfaces

`virtual interface` type declaration, assignment, and access. Not supported at any layer. Test: `lrm/ch25/virtual_interface`.

### 25.10: Type and param access through modport

Types and localparams not listed in modport should remain accessible through port reference. Not tested. Test: `lrm/ch25/modport_type_access`.

## Chapter 26 -- Packages

### 26.7: std package contents (Annex G)

The std built-in package should contain process, mailbox, and semaphore classes per Annex G. Not tested. Blocked by: class support (Ch 8). Test: deferred until class support lands.
