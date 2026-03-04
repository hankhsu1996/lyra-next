// LRM 7.2.2: Assigning to structures
//
// Whole-record assignment is allowed when LHS and RHS are the same record
// type (same semantic identity). Different struct/union types are not
// assignment-compatible even if structurally identical.

module assigning_to_structures;

  // Two structurally identical but distinct struct types
  typedef struct packed { logic [7:0] a; logic [7:0] b; } s1_t;
  typedef struct packed { logic [7:0] a; logic [7:0] b; } s2_t;

  // A union type with equal-width members to avoid packed union width error
  typedef union packed { logic [15:0] raw; logic [15:0] alt; } u1_t;

  s1_t x, y;
  s2_t z;
  u1_t w;

  // OK: same struct type
  assign x = y;

  // Error: different struct types (structurally identical but distinct)
  assign x = z;
  //        ^ error[lyra.type.record_assign_incompat]

  // Error: struct vs union
  assign x = w;
  //        ^ error[lyra.type.record_assign_incompat]

  // OK: packed struct from integral (no record-to-record mismatch)
  assign x = 16'hABCD;

  // Packed struct to/from integral (reuse s1_t which is 16-bit packed)
  logic [15:0] v16;
  assign v16 = x;       // OK: packed struct -> integral (same width)
  assign x = v16;       // OK: integral -> packed struct (same width)

  // Truncation: 16-bit packed struct -> 8-bit integral
  logic [7:0] v8;
  assign v8 = x;
  //    ^ warning[lyra.type.width_mismatch]

  // Unpacked struct to/from integral (error)
  typedef struct { logic [7:0] a; logic [7:0] b; } unpacked_t;
  unpacked_t u;
  //           ^ error[lyra.type.unpacked_record_integral_assign]
  assign v16 = u;
  //             ^ error[lyra.type.unpacked_record_integral_assign]
  assign u = v16;

endmodule
