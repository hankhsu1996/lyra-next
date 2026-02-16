package pkg;
  logic val;
endpackage

module top;
  logic y;
  assign y = pkg::val;
endmodule
