// Non-ANSI port expression with implicit net (LRM 6.10).
// `a` in `(a)` gets an implicit wire net; `input a;` gives direction.
module top(a);
  input a;
endmodule
