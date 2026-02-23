// LRM 6.19.2: Enumerated type ranges
//
// Enum members may use ranges: name[N] generates N members (name0..nameN-1),
// name[N:M] generates members from N to M inclusive.
// An explicit value on a ranged member sets the first expanded name's value;
// subsequent names auto-increment.

module enum_type_ranges;

  // Count form with explicit initial value
  typedef enum { A[3] = 2 } valued_t;
  valued_t v;
  assign v = A0;
  assign v = A1;
  assign v = A2;

  // From-to form with explicit initial value
  typedef enum { B[1:3] = 10 } fromto_t;
  fromto_t ft;
  assign ft = B1;
  assign ft = B2;
  assign ft = B3;

  // Mixed plain and ranged members with values
  typedef enum { START = 0, SEQ[3], STOP = 100 } mixed_t;
  mixed_t m;
  assign m = START;
  assign m = SEQ0;
  assign m = SEQ1;
  assign m = SEQ2;
  assign m = STOP;

endmodule
