// LRM 6.19.5.3: next() method
//
// Returns the Nth next enumeration value (default N=1).
// Wraps around to the first member after the last.
// Return type matches the enum type.

module enum_next;

  typedef enum { A, B, C, D } abcd_t;
  abcd_t x;

  // next() with no argument (default N=1)
  abcd_t n1 = x.next();

  // next(N) with explicit step
  abcd_t n2 = x.next(2);
  abcd_t n3 = x.next(3);

endmodule
