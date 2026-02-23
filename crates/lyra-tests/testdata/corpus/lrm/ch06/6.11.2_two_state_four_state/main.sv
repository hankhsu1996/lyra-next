// LRM 6.11.2: 2-state and 4-state data types
//
// 2-state types hold values 0 and 1 only.
// 4-state types hold values 0, 1, x, and z.
// Types are paired by width: int/integer (32), longint/time (64),
// bit/logic (1-bit scalar or vector).

module two_state_four_state;

  // 32-bit pair: int (2-state) vs integer (4-state)
  int     i2;
  integer i4;

  // 64-bit pair: longint (2-state) vs time (4-state)
  longint li2;
  time    t4;

  // 1-bit pair: bit (2-state) vs logic (4-state)
  bit   b2;
  logic l4;

  // reg is a 4-state synonym for logic
  reg r4;

  // Other 2-state types: byte (8), shortint (16)
  byte     by;
  shortint si;

  // Vectorized 2-state vs 4-state
  bit [7:0]   bv2;
  logic [7:0] lv4;

endmodule
