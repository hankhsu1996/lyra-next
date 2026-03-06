// LRM 25.5.2: Modport name at the module instantiation site.
// Module ports use plain interface type; modport selected at connection.

interface simple_bus (input logic clk);
  logic req, gnt;
  logic [7:0] addr, data;
  logic [1:0] mode;
  logic start, rdy;

  modport target (input req, addr, mode, start, clk,
                  output gnt, rdy,
                  ref data);
  modport initiator(input gnt, rdy, clk,
                    output req, addr, mode, start,
                    ref data);
endinterface: simple_bus

module memMod(simple_bus a);
  logic avail;
  always @(posedge a.clk)
    a.gnt <= a.req & avail;
endmodule

module cpuMod(simple_bus b);
endmodule

module top;
  logic clk = 1'b0;
  simple_bus sb_intf(clk);
  memMod mem(sb_intf.target);
  cpuMod cpu(sb_intf.initiator);
endmodule
