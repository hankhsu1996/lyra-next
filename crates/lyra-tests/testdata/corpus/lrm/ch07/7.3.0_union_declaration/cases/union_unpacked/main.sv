// LRM 7.3: Unions -- unpacked union declaration and member access
//
// Tests unpacked union typedef, inline declaration, and field selection.

module union_unpacked;

  // Typedef unpacked union
  typedef union {
    int i;
    shortreal f;
  } num_t;

  num_t n;
  int result_i;
  assign result_i = n.i;

  // Inline unpacked union
  union {
    int a;
    logic [31:0] b;
  } u;

  int result_a;
  assign result_a = u.a;

endmodule
