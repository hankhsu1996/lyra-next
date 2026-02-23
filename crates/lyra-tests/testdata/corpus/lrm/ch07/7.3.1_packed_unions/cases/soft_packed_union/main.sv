// LRM 7.3.1: Soft packed unions
//
// Members need not be the same width. Union width is max of member widths.

module soft_packed_union;

  // Explicit packed: union soft packed
  union soft packed { logic [7:0] a; logic [31:0] b; } u0;

  // Implicit packed: soft implies packed
  union soft { logic [3:0] x; logic [15:0] y; } u1;

endmodule
