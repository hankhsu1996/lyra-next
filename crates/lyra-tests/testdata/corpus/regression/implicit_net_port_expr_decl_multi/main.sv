// Non-ANSI port with multiple names in one direction declaration (LRM 6.10).
// Both `a` and `b` get implicit wire nets.
module top(a, b);
  input a, b;
endmodule
