// LRM 5.4: Comments
//
// SystemVerilog has two comment forms:
//   - Line comments: // ... newline
//   - Block comments: /* ... */
// Block comments do not nest.

// Line comment
module comments_test;

  /* Block comment */
  logic a;

  /* Multi-line
     block comment */
  logic b;

  logic /* inline block comment */ c;

  // Line comment with // embedded double slashes
  logic d;

  /* Block comment with // line comment inside */
  logic e;

  // Line comment with /* block comment start inside
  logic f;

endmodule
