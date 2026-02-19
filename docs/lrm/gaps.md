# LRM Gaps

Known gaps between LRM requirements and current engine capabilities. This is the work queue for LRM signoff. See `docs/lrm-signoff.md` for methodology.

When you discover a gap during `/lrm-add`, add an entry here. When you fix the gap and add the passing test, remove the entry. Both changes land in the same PR.

## Chapter 5: Lexical conventions

### 5.11 Replication in assignment patterns

Replication inside assignment patterns (`'{3{4}}`) fails to parse. The parser's replication handler does not work inside `'{...}` context. Blocked by: parser support for replication within assignment patterns.

Test to add: replication form in `lrm/ch05/array_literals`

### 5.13 Dynamic array declarations

Dynamic array declarations (`int arr[]`) cause a parser panic. The parser does not recognize the unsized dimension `[]` syntax for dynamic arrays. Blocked by: parser support for dynamic array type syntax.

Test to add: dynamic array method calls in `lrm/ch05/builtin_methods`
