// LRM 25.5.1: Modport name in the module port declaration.
// Module header specifies interface_name.modport_name.

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

module memMod (simple_bus.target a);
  logic avail;
  always @(posedge a.clk)
    a.gnt <= a.req & avail;
endmodule

module cpuMod (simple_bus.initiator b);
endmodule

module top;
  logic clk = 1'b0;
  simple_bus sb_intf(clk);
  memMod mem(.a(sb_intf));
  cpuMod cpu(.b(sb_intf));
endmodule
