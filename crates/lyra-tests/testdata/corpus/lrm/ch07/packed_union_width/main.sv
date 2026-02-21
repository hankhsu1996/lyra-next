// LRM 7.3.1: Packed union width constraints
//
// Hard packed unions require all members to have equal bit width.
// Soft packed unions allow different-width members.

module packed_union_width;

  // Valid: equal-width members (no diagnostics)
  typedef union packed {
    logic [7:0] a;
    logic [7:0] b;
  } equal_t;

  // Invalid: mismatched width members
  typedef union packed {
    logic [7:0]  c;
    logic [15:0] d;
  //             ^ error[lyra.type[9]]: packed union member `d` is 16 bits, expected 8 bits

  } mismatch_t;

  // Member from packed struct (struct width = sum of fields = 16 bits)
  typedef struct packed {
    logic [7:0] x;
    logic [7:0] y;
  } pair_t;

  typedef union packed {
    pair_t       s;
    logic [15:0] v;
  } struct_member_t;

  // Member from enum with explicit base type (8-bit base)
  typedef enum logic [7:0] { A, B, C } color_t;

  typedef union packed {
    color_t      e;
    logic [7:0]  f;
  } enum_member_t;

  // Soft packed union: different widths allowed (regression guard)
  typedef union soft packed {
    logic [7:0]  g;
    logic [31:0] h;
  } soft_t;

  // Packed array member (bit [3:0][7:0] = 32 bits)
  typedef union packed {
    bit [3:0][7:0] arr;
    bit [31:0]     flat;
  } array_member_t;

  // Descending packed range (logic [0:7] = 8 bits, direction-independent)
  typedef union packed {
    logic [0:7] lo;
    logic [7:0] hi;
  } range_dir_t;

endmodule
