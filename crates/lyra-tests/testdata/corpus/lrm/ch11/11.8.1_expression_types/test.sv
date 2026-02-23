// LRM 11.8.1: bit-select and part-select results are unsigned
// regardless of the signedness of the operand.
module select_signedness;
  logic signed [7:0] sv;
  int si;

  // Bit-select on signed operands -- result is unsigned
  logic b1;
  assign b1 = sv[3];
  logic b2;
  assign b2 = si[3];

  // Fixed part-select on signed operands -- result is unsigned
  logic [3:0] ps1;
  assign ps1 = sv[3:0];
  logic [7:0] ps2;
  assign ps2 = sv[7:0];

  // Indexed part-select on signed operands -- result is unsigned
  logic [3:0] ip1;
  assign ip1 = sv[0+:4];
  logic [3:0] ip2;
  assign ip2 = sv[7-:4];

  // Part-select on 2-state signed type
  logic [15:0] ps3;
  assign ps3 = si[15:0];
endmodule
