# LRM Gaps

Known gaps between LRM requirements and current engine capabilities. This is the work queue for LRM signoff. See `docs/lrm-signoff.md` for methodology. For cross-cutting architectural limitations that are not specific to an LRM section, see `docs/architecture-gaps.md`.

When you discover a gap during `/lrm-add`, add an entry here. When you fix the gap and add the passing test, remove the entry. Both changes land in the same PR.

## Chapter 5 -- Lexical Conventions

### 5.6.4: Compiler directives -- non-conditional directive semantics

Macro expansion supports object-like and function-like macros, argument substitution, nested expansion, line continuation, and LRM 22.5.1 stringification/concatenation operators (`` `" `` stringify, `` `""" `` triple-quote stringify, `` `` `` concat, `` `\`" `` escaped quote). `` `default_nettype `` directive semantics are implemented with per-file policy summaries and position-aware lookup. Include path resolution with `-I`/`--incdir` is implemented. Remaining gaps: `` `begin_keywords ``/`` `end_keywords `` (keyword set selection). Blocked by: keyword set infrastructure. Test: `lrm/ch05/5.6.4_compiler_directives`.

## Chapter 6 -- Data Types

### 6.3.2: Strengths -- type-level strength tracking

Drive strength and charge strength syntax is parsed. Illegal both-highz drive strength combinations are diagnosed on net declarations and continuous assigns. Remaining: type-level strength tracking and propagation into the typed net representation. Blocked by: typed net strength model. Test: `lrm/ch06/6.3.2.1_charge_strength`, `lrm/ch06/6.3.2.2_drive_strength`.

### 6.6.7: User-defined nettypes -- resolve function and multi-driver semantics

`nettype` declarations parse, collect as type-namespace symbols, and resolve through the alias expansion chain (define form, alias form, cross-package use). Remaining: resolve function signature validation, multi-driver semantics, net-declaration-specific legality rules. Blocked by: callable signature matching for resolve functions, driver/receiver model. Test: `lrm/ch06/6.6.7_user_defined_nettypes`.

### 6.6.8: Generic interconnect -- elaboration semantics

`interconnect` declarations parse, produce `Net` symbols with `NetKind::Interconnect`, reject strength specifications at parse time, and display honestly (no fabricated base type). Remaining: connection-determined type resolution at elaboration time, driver/receiver semantics. Blocked by: elaboration-time port binding model. Test: `lrm/ch06/6.6.8_generic_interconnect`.

### 6.14: Chandle data type

`Ty::Chandle` exists in the type system and `chandle` parses as a type specifier. Expressions of chandle type produce `UnsupportedExprKind`. Missing: chandle variable declarations with `null` initialization, `null` comparison, passing chandle to/from DPI-C functions, chandle as argument/return type in foreign function declarations. Blocked by: DPI-C support (Ch 35), null literal handling. Test: `lrm/ch06/6.14_chandle_data_type`.

### 6.15: Class

Class declarations (`class`/`endclass`) are not parsed. Keywords are in the lexer but no parser grammar, AST nodes, or semantic representation exists. This is a large feature covering inheritance, virtual methods, polymorphism, constructors, static members, parameterized classes, and more (Ch 8). Blocked by: parser grammar for class declarations, full OOP semantic model. Test: deferred until class support lands (Ch 8).

### 6.17: Event data type

`Ty::Event` exists in the type system and `event` parses as a type specifier. Expressions of event type produce `UnsupportedExprKind`. Missing: event trigger operator (`->`), event wait (`@event_var`), event `or` composition, `null` assignment, event variable in sensitivity lists. Blocked by: event trigger/wait parsing and semantic checking, process/timing control support. Test: `lrm/ch06/6.17_event_data_type`.

### 6.20.5: Specify parameters

`specparam` declarations have no semantic tracking. No specify block support exists in the parser. Blocked by: specify block parsing (Ch 32), specparam scope and constant model. Test: `lrm/ch06/6.20.5_specify_parameters`.

### 6.21: Scope and lifetime

Callable-local lifetime inheritance, container-level callable defaults (`module automatic`, etc.), nested block inheritance of surrounding procedural lifetime context, and loop-variable forced-automatic are implemented. Remaining gaps: class-context defaults (blocked on class support, Ch 8), static-context initialized-variable intent diagnostic. Test: `lrm/ch06/6.21_scope_and_lifetime`.

### 6.22: Type compatibility -- formal matching and equivalence rules

Basic assignment compatibility checking exists (width truncation, enum-to-enum, array structural matching). The formal LRM type compatibility classifications (matching types 6.22.1, equivalent types 6.22.2, assignment-compatible 6.22.3, cast-compatible 6.22.4) are not implemented as distinct queries. Blocked by: formal type identity and equivalence predicates, typedef-through resolution for equivalence. Test: `lrm/ch06/6.22_type_compatibility`.

### 6.23: Type operator -- deferred features

`type(data_type)` and `type(expr)` implemented for declarations and casts. Remaining: `type(this)`, generate `case(type(x))` matching, type equality/inequality semantics. Tests: `lrm/ch06/6.23_type_operator`.

### 6.24.2: $cast dynamic casting

The `$cast` system function for runtime dynamic type checking and conversion is not implemented. Not present in the builtin function list. Blocked by: class hierarchy support (for polymorphic downcasting), runtime cast semantics. Test: `lrm/ch06/6.24.2_cast_dynamic`.

### 6.24.3: Bit-stream casting

Casting between bit-stream compatible types (aggregates, arrays, strings cast to/from integral types) is not implemented. Related gaps exist for streaming operator bitstream conversion (11.4.14.1). Blocked by: bitstream type size computation, recursive packing/unpacking rules. Test: `lrm/ch06/6.24.3_bitstream_casting`.

### 6.25: Parameterized data types

Type parameter declarations and default resolution are implemented (6.20.3). Remaining: elaboration-time type substitution when instantiating modules with type parameter overrides, parameterized class/interface infrastructure. Blocked by: instantiation-time override substitution, parameterized class support (Ch 8). Test: `lrm/ch06/6.25_parameterized_data_types`.

## Chapter 7 -- Aggregate Data Types

### 7.2.2: Assigning to structures

Record-to-record identity mismatch is diagnosed. Packed/softpacked record to/from integral assignment is allowed with truncation warnings; unpacked record to/from integral is an error. Remaining gaps: pattern/aggregate assignment and fieldwise forms, coercion/cast diagnostics in record context, enum-to-record assignment compatibility. Blocked by: aggregate literal/pattern expression support, enum-record compat rules. Test: `lrm/ch07/7.2.2_assigning_to_structures`.

### 7.3.2: Tagged unions

Tagged union declarations are parsed with void and typed members. Tagged expression constructors (LRM 11.9) are context-typed in assignment, variable-initialization, and call-argument positions via a first-class call-argument semantic product (`CallArgCheck`). The expected type determines the record, void-vs-payload arity is validated, and operand type compatibility is checked via the central assignment compatibility path. Constructor structural errors (unknown member, void member with operand, payload member without operand, non-tagged-union target) produce user-visible diagnostics in all contexts. Pattern matching syntax (12.6.1, 12.6.2) has dedicated pattern AST nodes, a dedicated conditional-predicate parser separate from the plain expression parser, and semantic collection of pattern-embedded expressions. `matches` and `&&&` are accepted only in `cond_predicate` call sites (`if()` conditions, `case matches` guards) and are not reachable from plain expression parsing or parenthesized expressions. Remaining gaps: runtime tag-safety work, conditional-expression predicate test (12.6.3), pattern/type compatibility checking, bind-variable scope introduction and name resolution, exhaustiveness/refutability rules. Blocked by: semantic pattern analysis. Test: `lrm/ch07/7.3.2_tagged_unions`.

### 7.4: Dynamic arrays, queues, and associative arrays -- foreach iteration

Type representation handles all unpacked dimension forms. Built-in array methods are implemented with receiver classification, arity/arg-type checking, and void-in-expression detection. `new[]` constructor with contextual typing, size/init validation, and structural array assignment compatibility are implemented. Remaining gap: `foreach` iteration (Ch12 loop statements). Tests: `lrm/ch07/7.5.0_dynamic_array_declaration/cases/`.

### 7.7: Arrays as arguments to subroutines

Callable signature infrastructure stores port types and call-site argument checking exists. Specific LRM rules for passing arrays (by reference vs by value, open array parameters, compatibility rules for dynamic/associative/queue arguments) are not validated. Blocked by: array argument passing semantics, open array parameter support. Test: `lrm/ch07/7.7_arrays_as_arguments`.

### 7.8.3: Associative array with class index

Associative arrays with class-type keys (`int aa[SomeClass]`) cannot be declared because class support is absent. `AssocIndex::Typed(Ty)` representation exists but no class types can be constructed. Blocked by: class support (Ch 8). Test: deferred until class support lands.

### 7.9.11: Associative array literals

Associative array literal syntax (`'{key1: val1, key2: val2}`) is not supported. Aggregate literal/pattern expressions are not parsed. Blocked by: aggregate literal expression parsing and type checking. Test: `lrm/ch07/7.9.11_associative_array_literals`.


## Chapter 11 -- Operators and Expressions

### 11.4.14.1: Non-integral operand bitstream conversion

Streaming of arrays, structs, unions, and strings follows recursive bitstream conversion rules (LRM 11.4.14.1). Only integral operands are supported. Blocked by: bitstream type conversion. Test: `lrm/ch11/11.4.14.1_streaming_aggregate`.

## Chapter 12 -- Procedural Programming Statements

### 12.5.4: Set membership case statement (`case inside`) -- runtime semantics

`case (expr) inside` parses, semantic traversal covers value-range expressions and branch bodies, and `casex`/`casez` combined with `inside` is diagnosed as illegal. Remaining: `inside` operator runtime value-range matching semantics (11.4.13), wildcard bit handling. Blocked by: `inside` operator runtime evaluation. Test: `lrm/ch12/12.5.4_set_membership_case`.

### 12.6.1: Pattern matching in case statements

Pattern syntax is parsed with dedicated AST nodes (tagged, wildcard, struct, bind, constant, paren patterns). `case ... matches` with pattern items and `&&&` guard parsing is supported. Semantic collection walks pattern-embedded expressions. `casex`/`casez` combined with `matches` is diagnosed as illegal. `matches` returns a 1-bit predicate type. Remaining gaps: pattern/type compatibility checking, bind-variable scope introduction and name resolution, exhaustiveness/refutability rules. Blocked by: semantic pattern analysis. Tests: `lrm/ch12/12.6.1_pattern_matching_case/cases/`.

### 12.6.2: Pattern matching in if statements

`if (expr matches pattern)` uses a dedicated `cond_predicate` parser separate from `expr`. `matches` and `&&&` are accepted only in `cond_predicate` call sites (`if()` conditions, `case matches` guards) and are not reachable from the plain expression parser or parenthesized expressions. `MatchesExpr` and optional `&&& guard` via `CondPredicate` produce structured AST nodes. Semantic collection and type inference walk predicate contents (head expression, guards). Remaining gaps: bind-variable scope introduction, pattern/type compatibility. Blocked by: semantic pattern analysis. Tests: `lrm/ch12/12.6.2_pattern_matching_if/cases/`.

### 12.6.3: Pattern matching in conditional expressions

LRM `conditional_expression ::= cond_predicate ? expression : expression` allows predicate syntax (`matches`, `&&&`) in the ternary test position. The plain expression parser does not support this; predicate syntax is restricted to `if()` conditions and `case matches` guards. Ternary with predicate test (`val matches tagged Valid .n ? n : 0`) requires a dedicated conditional-expression parser that uses `cond_predicate` in the test position while keeping predicate syntax out of general expression context. Blocked by: conditional-expression parser layer. Test: deferred until conditional-expression support lands.

## Chapter 20 -- System Functions

### 20.6.2: $bits constant evaluation -- unsupported type categories

`$bits` const-eval handles packed integral types (all packed dims), real types, enums (base type width), packed records (struct=sum, union=max), unpacked records (bit-stream sum/max including per-field declarator dims), and fixed-size unpacked arrays (recursive element width * length). Strings and dynamic arrays return const-eval error (not yet surfaced as user-visible diagnostic). Unsupported: chandle, event, void, and interfaces (implementation-defined or not applicable per LRM). Test: `lrm/ch20/20.6.2_expression_size_system_function`.

### 20.7: Array query functions -- runtime evaluation and type-form on dynamic types

Typing (arity validation, return type) and const-eval for fixed packed/unpacked dimensions implemented for all eight array query functions. Compile-time legality check for dimension-by-number selecting a variable-sized dimension is implemented and integration-tested. Type-form legality check for range queries on dynamically-sized types is implemented in the type checker and unit-tested, but cannot be integration-tested because typedef names do not resolve in system call arguments (pre-existing limitation shared with `$bits`; blocked on system-call type-arg resolution). Remaining gaps: runtime state-dependent evaluation (simulator scope), type-form integration test coverage (blocked on typedef resolution). Tests: `lrm/ch07/7.11_array_query_functions/cases/`.

## Chapter 25 -- Interfaces

### 25.5.5: Clocking blocks in modports

`modport_clocking_declaration` syntax. No clocking block support. Test: `lrm/ch25/25.5.5_modport_clocking`.

### 25.6: Interfaces and specify blocks

Specify block terminal rules for interface ports. No specify block support. Test: `lrm/ch25/25.6_interfaces_and_specify_blocks`.

### 25.7: Export binding semantics

`export` with `task iface.Name` syntax binds a module-defined task into the interface. Parsing of export TF entries is implemented, but the binding semantics that wire a module's task body into the interface scope are not. Blocked by: module-to-interface task binding model. Test: `lrm/ch25/25.7.3_exporting_tasks`.

### 25.7.4: Extern forkjoin tasks

`extern forkjoin task` declaration in interfaces. Not supported. Test: `lrm/ch25/25.7.4_extern_forkjoin`.

### 25.9: Virtual interfaces

`virtual interface` type declaration, assignment, and access. Not supported at any layer. Test: `lrm/ch25/25.9_virtual_interfaces`.

## Chapter 26 -- Packages

### 26.7: std package contents (Annex G)

The std built-in package should contain process, mailbox, and semaphore classes per Annex G. Not tested. Blocked by: class support (Ch 8). Test: deferred until class support lands.

## Chapter 22 -- Compiler Directives
