// LRM 11.5.1: Bit-select and part-select addressing
module bit_part_select;
  logic [7:0] vec;
  logic [3:0][7:0] packed_2d;
  logic [7:0] mem [3:0];

  // Bit-select
  logic b;
  assign b = vec[3];

  // Fixed part-select
  logic [3:0] slice;
  assign slice = vec[7:4];

  // Indexed part-select
  logic [3:0] islice;
  assign islice = vec[0+:4];

  // Multi-dim unpacked then bit-select
  logic b2;
  assign b2 = mem[0][3];

  // Packed dim peel
  logic [7:0] byte_val;
  assign byte_val = packed_2d[2];
endmodule
