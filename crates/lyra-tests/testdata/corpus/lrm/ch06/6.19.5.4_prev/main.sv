// LRM 6.19.5.4: prev() method
//
// Returns the Nth previous enumeration value (default N=1).
// Wraps around to the last member before the first.
// Return type matches the enum type.

module enum_prev;

  typedef enum { A, B, C, D } abcd_t;
  abcd_t x;

  // prev() with no argument (default N=1)
  abcd_t p1 = x.prev();

  // prev(N) with explicit step
  abcd_t p2 = x.prev(2);
  abcd_t p3 = x.prev(3);

endmodule
