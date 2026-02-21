// LRM 6.19.3: Enum name ranges
//
// Enum members may specify ranges: name[N] (count) or name[N:M] (from-to).
// Range members expand into individual variant names at elaboration time.

module enum_ranges;

  // Count form: A[3] expands to A0, A1, A2
  typedef enum { A[3] } count_t;
  count_t c;
  assign c = A0;
  assign c = A1;
  assign c = A2;

  // From-to ascending: B[2:4] expands to B2, B3, B4
  typedef enum { B[2:4] } ascending_t;
  ascending_t a;
  assign a = B2;
  assign a = B3;
  assign a = B4;

  // From-to descending: C[4:2] expands to C4, C3, C2
  typedef enum { C[4:2] } descending_t;
  descending_t d;
  assign d = C4;
  assign d = C3;
  assign d = C2;

  // Mixed: plain and range members in one enum
  typedef enum { X[2], Y, Z[3] } mixed_t;
  mixed_t m;
  assign m = X0;
  assign m = X1;
  assign m = Y;
  assign m = Z0;
  assign m = Z1;
  assign m = Z2;

endmodule
