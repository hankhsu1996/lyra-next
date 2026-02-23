// LRM 7.4.6: Operations on arrays -- slice addressing
module array_slice;
  bit signed [31:0] busA [7:0];
  int busB [1:0];
  assign busB = busA[7:6];

  parameter int C = 3;
  int src [9:0];
  int dst [2:0];
  assign dst = src[0+:C];

  logic [7:0] mem [3:0];
  logic [7:0] pair [1:0];
  assign pair = mem[3:2];
endmodule
