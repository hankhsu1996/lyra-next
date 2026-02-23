// LRM 6.19.5.2: last() method
//
// Returns the value of the last member of the enumeration.
// Return type matches the enum type.

module enum_last;

  typedef enum { A, B, C } abc_t;
  abc_t x;

  // last() returns the enum type
  abc_t l = x.last();

  // Works on enums with explicit values
  typedef enum { P = 10, Q = 20, R = 30 } pqr_t;
  pqr_t y;
  pqr_t pl = y.last();

endmodule
