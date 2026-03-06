// LRM 25.3.2: Interface as a named bundle of signals.
// Modules reference interface members via the port name.

interface simple_bus;
  logic req, gnt;
  logic [7:0] addr, data;
  logic [1:0] mode;
  logic start, rdy;
endinterface: simple_bus

module memMod(simple_bus a, input logic clk);
  logic avail;
  always @(posedge clk) a.gnt <= a.req & avail;
endmodule

module cpuMod(simple_bus b, input logic clk);
endmodule

module top;
  logic clk = 1'b0;
  simple_bus sb_intf();
  memMod mem(sb_intf, clk);
  cpuMod cpu(.b(sb_intf), .clk(clk));
endmodule
