# LRM Gaps

Known gaps between LRM requirements and current engine capabilities. This is the work queue for LRM signoff. See `docs/lrm-signoff.md` for methodology.

When you discover a gap during `/lrm-add`, add an entry here. When you fix the gap and add the passing test, remove the entry. Both changes land in the same PR.

## Chapter 5 -- Lexical Conventions

### 5.6.4: Compiler directives -- parameterized macros and non-conditional directive semantics

Object-like macro expansion at use sites is implemented (recursive expansion with depth limit, token-bodied storage, undefined macro diagnostics). Remaining gaps: parameterized macros (`` `FOO(a, b) ``), line continuation in macro bodies, stringification/concatenation operators, non-conditional directive semantics via event consumption (`` `timescale ``, `` `default_nettype ``, etc.). Blocked by: parameterized macro parsing, directive event consumers. Test: `lrm/ch05/5.6.4_compiler_directives`.

## Chapter 6 -- Data Types

### 6.3.2: Strengths -- charge strength and drive strength

Drive strength (`(strong0, pull1)`, etc.) and charge strength (`small`, `medium`, `large`) on net declarations are not parsed. Keywords are recognized by the lexer but no grammar rules exist. Valid SV code using strength specifications produces parse errors. Blocked by: parser grammar for strength specifications, AST representation, type-level strength tracking. Test: `lrm/ch06/6.3.2_strengths`.

### 6.6.7: User-defined nettypes

`nettype` declarations (`nettype <type> <name> [with <resolve_function>]`) are not parsed. The keyword is in the lexer but no parser or semantic support exists. Blocked by: parser grammar, AST nodes, semantic representation for custom net resolution functions. Test: `lrm/ch06/6.6.7_user_defined_nettypes`.

### 6.6.8: Generic interconnect

`interconnect` net type is not parsed. The keyword is in the lexer but no parser or semantic support exists. Blocked by: parser grammar, interconnect type representation, automatic type resolution semantics. Test: `lrm/ch06/6.6.8_generic_interconnect`.

### 6.10: Implicit declarations

Undeclared identifiers used in net contexts should implicitly declare a 1-bit wire (or the type set by `` `default_nettype ``). No implicit net creation exists. The preprocessor recognizes the `default_nettype` directive keyword but does not consume it. Blocked by: `default_nettype` directive event consumption (Ch 5.6.4 gap), name resolution special-case for implicit nets. Test: `lrm/ch06/6.10_implicit_declarations`.

### 6.13: Void data type

`Ty::Void` exists in the type system and `void` parses as a type specifier. Void-in-expression detection works for built-in method calls. Missing: enforcement that `void` cannot be used as a variable type (only valid as function/task return type), void function call semantics in expression vs statement context beyond built-in methods. Blocked by: declaration-site type validation, broader statement/expression context checking. Test: `lrm/ch06/6.13_void_data_type`.

### 6.14: Chandle data type

`Ty::Chandle` exists in the type system and `chandle` parses as a type specifier. Expressions of chandle type produce `UnsupportedExprKind`. Missing: chandle variable declarations with `null` initialization, `null` comparison, passing chandle to/from DPI-C functions, chandle as argument/return type in foreign function declarations. Blocked by: DPI-C support (Ch 35), null literal handling. Test: `lrm/ch06/6.14_chandle_data_type`.

### 6.15: Class

Class declarations (`class`/`endclass`) are not parsed. Keywords are in the lexer but no parser grammar, AST nodes, or semantic representation exists. This is a large feature covering inheritance, virtual methods, polymorphism, constructors, static members, parameterized classes, and more (Ch 8). Blocked by: parser grammar for class declarations, full OOP semantic model. Test: deferred until class support lands (Ch 8).

### 6.17: Event data type

`Ty::Event` exists in the type system and `event` parses as a type specifier. Expressions of event type produce `UnsupportedExprKind`. Missing: event trigger operator (`->`), event wait (`@event_var`), event `or` composition, `null` assignment, event variable in sensitivity lists. Blocked by: event trigger/wait parsing and semantic checking, process/timing control support. Test: `lrm/ch06/6.17_event_data_type`.

### 6.20.3: Type parameters

`parameter type` declarations are parsed and detected but diagnosed as unsupported (`TypeParameterUnsupported`). Type parameters resolve to error. Blocked by: parameterized type representation in the type system, elaboration-time type substitution. Test: `lrm/ch06/6.20.3_type_parameters`.

### 6.20.5: Specify parameters

`specparam` declarations have no semantic tracking. No specify block support exists in the parser. Blocked by: specify block parsing (Ch 32), specparam scope and constant model. Test: `lrm/ch06/6.20.5_specify_parameters`.

### 6.20.6: Const constants

The `const` qualifier on variable declarations is not parsed. The keyword is in the lexer but no parser grammar handles `const var` or `const` in data declarations. Blocked by: parser grammar for const-qualified declarations, immutability tracking in the semantic layer. Test: `lrm/ch06/6.20.6_const_constants`.

### 6.21: Scope and lifetime

`automatic` and `static` lifetime qualifiers are parsed on module/task/function declarations but not tracked in the semantic layer. No symbol-level lifetime storage or queries exist. Lifetime inference rules (default static in modules, default automatic in classes) are not enforced. Blocked by: lifetime attribute on symbols, lifetime inference logic. Test: `lrm/ch06/6.21_scope_and_lifetime`.

### 6.22: Type compatibility -- formal matching and equivalence rules

Basic assignment compatibility checking exists (width truncation, enum-to-enum, array structural matching). The formal LRM type compatibility classifications (matching types 6.22.1, equivalent types 6.22.2, assignment-compatible 6.22.3, cast-compatible 6.22.4) are not implemented as distinct queries. Blocked by: formal type identity and equivalence predicates, typedef-through resolution for equivalence. Test: `lrm/ch06/6.22_type_compatibility`.

### 6.23: Type operator

The `type()` operator (used to reference the type of an expression or data object) is not supported at any layer. No parser grammar, AST node, or semantic handling exists. Blocked by: parser grammar for type reference expressions, type query in the semantic layer. Test: `lrm/ch06/6.23_type_operator`.

### 6.24.2: $cast dynamic casting

The `$cast` system function for runtime dynamic type checking and conversion is not implemented. Not present in the builtin function list. Blocked by: class hierarchy support (for polymorphic downcasting), runtime cast semantics. Test: `lrm/ch06/6.24.2_cast_dynamic`.

### 6.24.3: Bit-stream casting

Casting between bit-stream compatible types (aggregates, arrays, strings cast to/from integral types) is not implemented. Related gaps exist for streaming operator bitstream conversion (11.4.14.1). Blocked by: bitstream type size computation, recursive packing/unpacking rules. Test: `lrm/ch06/6.24.3_bitstream_casting`.

### 6.25: Parameterized data types

Parameterized data types (class or type specialization via `#(type T = int)`) are not supported. Type parameters are diagnosed as unsupported (see 6.20.3). Blocked by: type parameter support, elaboration-time type substitution, parameterized class/interface infrastructure. Test: `lrm/ch06/6.25_parameterized_data_types`.

## Chapter 7 -- Aggregate Data Types

### 7.2.2: Assigning to structures

Packed structs can be assigned from matching-width integral values. Struct-to-struct assignment (copying all fields) and aggregate assignment patterns (struct literals) are not type-checked. No assignment compatibility checking exists for `Record` types in `type_check.rs`. Blocked by: struct assignment compatibility rules, aggregate literal/pattern expression support. Test: `lrm/ch07/7.2.2_assigning_to_structures`.

### 7.3.1: Soft packed union layout semantics

Width computation (`bit_width_total`) handles packed records internally (struct=sum, union=max). `$bits` const-eval now exposes these widths at the SV level. Right-justified member placement and layout mapping are not modeled. Test: `lrm/ch07/7.3.1_packed_unions/cases/soft_packed_union_layout`.

### 7.3.2: Tagged unions

The `tagged` qualifier is parsed and diagnosed as unsupported; the type resolves to error. Full support requires tagged expressions (11.9), pattern matching (12.6), tag tracking, and void members. `RecordKind::TaggedUnion` exists but is never constructed. Test: `lrm/ch07/7.3.2_tagged_unions`.

### 7.4: Dynamic arrays, queues, and associative arrays -- foreach iteration

Type representation handles all unpacked dimension forms. Built-in array methods are implemented with receiver classification, arity/arg-type checking, and void-in-expression detection. `new[]` constructor with contextual typing, size/init validation, and structural array assignment compatibility are implemented. Remaining gap: `foreach` iteration (Ch12 loop statements). Tests: `lrm/ch07/7.5.0_dynamic_array_declaration/cases/`.

### 7.7: Arrays as arguments to subroutines

Callable signature infrastructure stores port types and call-site argument checking exists. Specific LRM rules for passing arrays (by reference vs by value, open array parameters, compatibility rules for dynamic/associative/queue arguments) are not validated. Blocked by: array argument passing semantics, open array parameter support. Test: `lrm/ch07/7.7_arrays_as_arguments`.

### 7.8.3: Associative array with class index

Associative arrays with class-type keys (`int aa[SomeClass]`) cannot be declared because class support is absent. `AssocIndex::Typed(Ty)` representation exists but no class types can be constructed. Blocked by: class support (Ch 8). Test: deferred until class support lands.

### 7.9.11: Associative array literals

Associative array literal syntax (`'{key1: val1, key2: val2}`) is not supported. Aggregate literal/pattern expressions are not parsed. Blocked by: aggregate literal expression parsing and type checking. Test: `lrm/ch07/7.9.11_associative_array_literals`.

### 7.10.1: Queue operators

Queue concatenation (`{q1, q2}`, `{q, item}`) and queue slicing are not supported. The concatenation operator currently requires integral operands only. Queue-specific concatenation and assignment patterns (e.g., `q = {q[0:1], item, q[3:$]}`) are not type-checked. Blocked by: non-integral concatenation support, queue slice semantics. Test: `lrm/ch07/7.10.1_queue_operators`.

### 7.10.5: Bounded queues -- runtime enforcement

Bounded queue declarations (`int q[$:255]`) are parsed and the bound is stored in the type system (`UnpackedDim::Queue { bound }`). Runtime enforcement of the maximum size constraint is not modeled. Diagnostics for statically detectable bound violations (e.g., initializing with more elements than the bound) are not emitted. Blocked by: queue bound validation at assignment/method-call sites. Test: `lrm/ch07/7.10.5_bounded_queues`.

### 7.12: Array manipulation methods

Locator methods (`find`, `find_index`, `find_first`, `find_first_index`, `find_last`, `find_last_index`), ordering methods (`sort`, `rsort`, `reverse`, `shuffle`), reduction methods (`sum`, `product`, `and`, `or`, `xor`), and iterator index querying are not implemented. None of these are in the `ArrayMethodKind` enum. These methods use `with (expression)` clauses that introduce an iterator variable, requiring special scoping. Blocked by: method enum extension, `with` clause parsing and iterator variable scoping, return type computation (locators return queues). Test: `lrm/ch07/7.12_array_manipulation_methods`.

## Chapter 11 -- Operators and Expressions

### 11.4.14: Streaming `with` array range -- unpack and dynamic arrays

Parsing and pack-width semantics for const ranges on fixed-size arrays with integral/enum elements implemented. Unpack (LHS) semantics and dynamic array resize pending (blocked by streaming assignment target gap and dynamic array support). Test: `lrm/ch11/11.4.14.4_streaming_with`.

### 11.4.14: Streaming assignment target (unpack)

Streaming operators on the LHS of assignments (unpack semantics) are not handled. The parser accepts the syntax but semantic checking (width validation, dynamic resizing) is not implemented. Blocked by: assignment target analysis. Test: `lrm/ch11/11.4.14.0_streaming_operators/cases/streaming_unpack`.

### 11.4.14.1: Non-integral operand bitstream conversion

Streaming of arrays, structs, unions, and strings follows recursive bitstream conversion rules (LRM 11.4.14.1). Only integral operands are supported. Blocked by: bitstream type conversion. Test: `lrm/ch11/11.4.14.1_streaming_aggregate`.

### 11.4.14: Streaming slice_size const validation

Parser accepts any expression as `slice_size`. The LRM requires it to be a constant expression. Constantness is not enforced. Blocked by: general constant expression validation. Test: N/A (part of const-eval signoff).

### 11.5.1: Fixed part-select with non-constant bounds

Engine currently requires constant bounds for `[hi:lo]` fixed part-select. Supporting non-constant bounds needs symbolic/dynamic width representation. Blocked by: dynamic width model. Test: `lrm/ch11/11.5.1_bit_select_and_part_select/cases/part_select_nonconstant`.

## Chapter 12 -- Procedural Programming Statements

### 12.7.3: Foreach -- package-qualified postfix headers

Simple indexed (`a[0][j]`), field-access (`s.arr[i]`), and multi-dim partial-index (`m[0][i,j]`) foreach headers are supported. Remaining gap: package-qualified base with postfix selects (`pkg::obj.field[i]`). Blocked by: cross-package field access typing. Test: `lrm/ch12/12.7.3_foreach_loop_statements/cases/indexed_array_ref`.

## Chapter 20 -- System Functions

### 20.6.2: $bits constant evaluation -- unsupported type categories

`$bits` const-eval handles packed integral types (all packed dims), real types, enums (base type width), packed records (struct=sum, union=max), unpacked records (bit-stream sum/max including per-field declarator dims), and fixed-size unpacked arrays (recursive element width * length). Strings and dynamic arrays return const-eval error (not yet surfaced as user-visible diagnostic). Unsupported: chandle, event, void, and interfaces (implementation-defined or not applicable per LRM). Test: `lrm/ch20/20.6.2_expression_size_system_function`.

### 20.7: Array query functions -- runtime evaluation

Typing (arity validation, return type) and const-eval for fixed packed/unpacked dimensions implemented for `$left`, `$right`, `$low`, `$high`, `$size`, `$increment`, `$dimensions`, `$unpacked_dimensions`. Remaining gaps: runtime evaluation (`$left`/`$right`/`$size` on dynamic arrays, queues, and associative arrays with current state) and type-form with dynamically-sized type identifiers. Test: `lrm/ch20/20.7_array_querying_system_functions`.

## Chapter 25 -- Interfaces

### 25.3.3: Generic interface reference

Module ports declared with bare `interface` keyword as type (generic interface). Parser and semantic layer do not recognize this syntax. Test: `lrm/ch25/25.3.3_generic_interface`.

### 25.5.5: Clocking blocks in modports

`modport_clocking_declaration` syntax. No clocking block support. Test: `lrm/ch25/25.5.5_modport_clocking`.

### 25.6: Interfaces and specify blocks

Specify block terminal rules for interface ports. No specify block support. Test: `lrm/ch25/25.6_interfaces_and_specify_blocks`.

### 25.7: Tasks and functions in interfaces

Import/export of subroutines through modports. Neither parsing nor semantic analysis handles `modport_tf_ports_declaration`. Test: `lrm/ch25/25.7_interface_methods`.

### 25.7.4: Extern forkjoin tasks

`extern forkjoin task` declaration in interfaces. Not supported. Test: `lrm/ch25/25.7.4_extern_forkjoin`.

### 25.8: Parameterized interfaces

Interface parameter override at instantiation site. Parser/elaboration may partially work but untested in LRM corpus. Test: `lrm/ch25/25.8_parameterized_interfaces`.

### 25.9: Virtual interfaces

`virtual interface` type declaration, assignment, and access. Not supported at any layer. Test: `lrm/ch25/25.9_virtual_interfaces`.

### 25.10: Type and param access through modport

Types and localparams not listed in modport should remain accessible through port reference. Not tested. Test: `lrm/ch25/25.10_modport_type_access`.

## Chapter 26 -- Packages

### 26.7: std package contents (Annex G)

The std built-in package should contain process, mailbox, and semaphore classes per Annex G. Not tested. Blocked by: class support (Ch 8). Test: deferred until class support lands.
