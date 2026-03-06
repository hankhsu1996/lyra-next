// LRM 25.5.3: Generic interface port with modport at instantiation.
// Module header uses `interface` keyword; actual interface and modport
// are specified when the module is instantiated.

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

module memMod(interface a);
  logic avail;
  always @(posedge a.clk)
    a.gnt <= a.req & avail;
endmodule

module cpuMod(interface b);
endmodule

module top;
  logic clk = 1'b0;
  simple_bus sb_intf(clk);
  memMod mem(sb_intf.target);
  cpuMod cpu(sb_intf.initiator);
endmodule
