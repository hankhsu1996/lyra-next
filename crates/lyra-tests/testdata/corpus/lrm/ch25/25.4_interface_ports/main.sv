// LRM 25.4: Interface with its own port list

interface i1 (input logic clk, input logic rst_n);
  logic [7:0] data;
  logic valid, ready;
endinterface
