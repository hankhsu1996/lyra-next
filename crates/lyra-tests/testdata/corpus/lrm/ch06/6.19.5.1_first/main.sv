// LRM 6.19.5.1: first() method
//
// Returns the value of the first member of the enumeration.
// Return type matches the enum type.

module enum_first;

  typedef enum { A, B, C } abc_t;
  abc_t x;

  // first() returns the enum type
  abc_t f = x.first();

  // Works on different enum shapes
  typedef enum { P = 10, Q = 20 } pq_t;
  pq_t y;
  pq_t pf = y.first();

endmodule
