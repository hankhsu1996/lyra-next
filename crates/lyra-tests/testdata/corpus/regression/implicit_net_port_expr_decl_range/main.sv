// Non-ANSI port with implicit packed-dimension type (LRM 6.10).
// `a` gets an implicit wire net; `input [7:0] a;` gives direction and range.
module top(a);
  input [7:0] a;
endmodule
