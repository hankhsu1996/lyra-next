// LRM 6.12: Real, shortreal, and realtime data types
//
// real is a 64-bit double, shortreal is a 32-bit float,
// realtime is synonymous with real.

module real_types;

  // All three real type keywords
  real      r;
  shortreal sr;
  realtime  rt;

  // Real literal assignments
  assign r  = 3.14;
  assign sr = 1.0;
  assign rt = 100.5;

  // Arithmetic operators on reals
  real sum, diff, prod, quot;
  assign sum  = r + sr;
  assign diff = r - sr;
  assign prod = r * sr;
  assign quot = r / sr;

  // Relational comparison
  logic cmp;
  assign cmp = (r > sr) ? 1'b1 : 1'b0;

  // Unary negation
  real neg;
  assign neg = -r;

endmodule
