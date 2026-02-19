# LRM Gaps

Known gaps between LRM requirements and current engine capabilities. This is the work queue for LRM signoff. See `docs/lrm-signoff.md` for methodology.

When you discover a gap during `/lrm-add`, add an entry here. When you fix the gap and add the passing test, remove the entry. Both changes land in the same PR.

## Chapter 5: Lexical conventions

### 5.7 Spaced sized literals

The LRM allows whitespace between the size, base, and value tokens in sized literals (e.g., `5 'd 3`). The lexer correctly produces separate `IntLiteral` and `BasedLiteral` tokens with trivia between them, but the parser only binds them when they are immediately adjacent (no trivia). Blocked by: parser change to allow trivia-separated sized literals.

Test to add: spaced sized literal case in `lrm_ch05_numbers`

### 5.7 Unbased unsized literal width

Unbased unsized literals (`'0`, `'1`, `'x`, `'z`) should expand to the width of their assignment context per LRM 5.7.1. The engine currently treats them as 1-bit, causing spurious width-mismatch warnings when assigned to wider targets. Blocked by: semantic support for context-dependent literal width.

Test to add: unbased unsized literals assigned to multi-bit targets in `lrm_ch05_numbers`

### 5.10 Keyed and default assignment patterns

Keyed assignment patterns (`'{a:0, b:1}`) and default patterns (`'{default:0}`) in structure and array literals cause a parser panic (rowan builder assertion failure). The parser's `ConcatExpr` handler does not recognize the `key : value` form inside `'{...}`. Blocked by: parser support for keyed assignment pattern syntax.

Test to add: keyed and default forms in `lrm_ch05_structure_literals` and `lrm_ch05_array_literals`

### 5.11 Replication in assignment patterns

Replication inside assignment patterns (`'{3{4}}`) fails to parse. The parser's replication handler does not work inside `'{...}` context. Blocked by: parser support for replication within assignment patterns.

Test to add: replication form in `lrm_ch05_array_literals`

### 5.13 Dynamic array declarations

Dynamic array declarations (`int arr[]`) cause a parser panic. The parser does not recognize the unsized dimension `[]` syntax for dynamic arrays. Blocked by: parser support for dynamic array type syntax.

Test to add: dynamic array method calls in `lrm_ch05_builtin_methods`
