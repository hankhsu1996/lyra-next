package my_pkg;
  logic x;
endpackage

module top;
  import my_pkg::x;
  import my_pkg::*;
  logic y;
  assign y = x;
endmodule
