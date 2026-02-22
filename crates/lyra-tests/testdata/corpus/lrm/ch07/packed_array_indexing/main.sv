// LRM 7.4.1, 7.4.5 -- Packed array indexing and part-select
//
// Tests multi-dimensional packed arrays, bit-select, and part-select.

module packed_array_indexing;

  // 1D packed array
  logic [7:0] byte_val;
  logic one_bit;
  assign one_bit = byte_val[3];

  // Part-select on packed vector
  logic [3:0] nibble;
  assign nibble = byte_val[7:4];

  // 2D packed array: logic [3:0][7:0] = 4 elements of 8-bit vectors
  logic [3:0][7:0] packed_2d;
  logic [7:0] elem;
  assign elem = packed_2d[2];

  // Bit-select on 2D packed array element
  logic single;
  assign single = packed_2d[1][3];

  // Multi-dim packed: $bits
  parameter B = $bits(logic [3:0][7:0]);

endmodule
