# Parser Design

Design invariants, decisions, and known improvement areas for `lyra-parser`.

## Architecture

Event-based recursive descent with Pratt precedence for expressions.
Parser emits `Event` stream, `replay()` converts to rowan green tree.

```
tokens -> Parser -> Vec<Event> -> replay() -> GreenNode
```

### Event Model

Three event types: `Start { kind, forward_parent }`, `Finish`, `Token { n_raw_tokens }`.

`forward_parent` enables left-recursive wrapping (Pratt expressions) without
reordering the event stream. A `Start` event can point forward to another
`Start` that becomes its parent. Replay walks these chains and opens nodes
from outermost to innermost.

Invariants:
- Every `Start` has exactly one matching `Finish`
- `forward_parent` chains are acyclic and within bounds
- Every consumed token produces exactly one `Token` event in stream order
- Replay consumes all tokens exactly once (no duplication or loss)

### Trivia Handling

All trivia (whitespace, comments) is **leading**: attached to the next
significant token.

- `current()` and `nth()` skip trivia
- `bump()` emits leading trivia tokens, then the significant token, as one
  `Token` event with `n_raw_tokens` counting all of them
- `eat_remaining_trivia()` emits trailing trivia individually before closing
  the root node

Consequences:
- Leading comments before a module end up inside the `ModuleDecl` node
- EOF-preceding trivia is inside `SourceFile` (via `eat_remaining_trivia`)
- Roundtrip fidelity: green tree text always equals original source

### ExprMode

`<=` is both relational operator and nonblocking assignment. The parser
disambiguates at parse time using `ExprMode`:

- `Normal`: `<=` is relational (inside parens, brackets, braces, conditions)
- `StmtLhs`: `<=` stops the expression (statement context handles as NBA)

`expr()` uses Normal. `expr_for_stmt()` uses StmtLhs. Assignment operators
(`=`, `<=`, `+=`, ...) are handled in `expr_stmt` and `for_stmt`, not in
the Pratt table.

### Error Recovery

On unexpected token:
1. Emit `ParseError` at current position
2. `error_bump()` wraps skipped token in `ErrorNode`
3. Fuel guard (256) prevents infinite loops -- returns `Eof` when exhausted

Sync sets are implicit in grammar structure (while loops check for closing
tokens like `endmodule`, `end`, `endcase`, `)`, `]`).

### ParseError Boundary

`ParseError { range, message }` is parser-local. `lyra-db` converts to
`Diagnostic` with attached `FileId`. The parser is FileId-agnostic.

## Known Improvements

### Based Literal Lexing

The lexer splits `2'b00` into `IntLiteral(2)` + `BasedLiteral('b00)`.
The parser cannot combine them, causing case items with sized based
literals to misparse. Fix belongs in `lyra-lexer`: sized based literals
should be a single token.

### ParseError Richness

Current errors are `String` messages. Future improvement: add structured
fields like `expected: Vec<SyntaxKind>`, error kind enum, recovery context.
This enables better IDE diagnostics and error rendering.

### Event Debug Tooling

The event replay is hard to debug when it breaks. Useful future additions:
- Event stream dump utility (text visualization)
- Invariant assertions in debug builds (balanced start/finish, chain validity)
- Tree diff tool for snapshot debugging

### Sync Set Hardening

Current recovery is minimal -- `error_bump` skips one token. Future work:
explicit sync sets per grammar context (statement-level, item-level,
delimiter-level) to skip to the next viable parse point instead of
one-token-at-a-time recovery.

### Trivia Invariant Documentation

The "all trivia is leading" rule needs formal specification before
formatter or incremental reparse work. Key questions to pin down:
- How trailing comments on a line attach (to next token, not current)
- How blank lines between items are attributed
- Whether incremental reparse can rely on trivia boundaries as edit points
