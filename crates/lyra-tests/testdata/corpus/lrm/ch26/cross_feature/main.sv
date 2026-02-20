// LRM 26: Cross-feature interaction test
// Package typedef used in module port and instantiation

package Defs;
  typedef logic [15:0] addr_t;
  parameter int DEPTH = 256;
endpackage

module memory(
  input logic [15:0] addr,
  input logic we);
  import Defs::*;
  localparam int D = DEPTH;
endmodule

module top;
  logic [15:0] a;
  logic w;
  memory mem0 (.addr(a), .we(w));
endmodule
