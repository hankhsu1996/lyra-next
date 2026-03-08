// Non-ANSI port with signed packed-dimension implicit type (LRM 6.10).
// `a` gets an implicit wire net; `input signed [7:0] a;` gives direction,
// signing, and range.
module top(a);
  input signed [7:0] a;
endmodule
