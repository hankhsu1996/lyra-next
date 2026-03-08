// Non-ANSI port with explicit wire declaration (LRM 6.10).
// `wire a;` provides the net, so no implicit net is created.
`default_nettype none
module top(a);
  input a;
  wire a;
endmodule
