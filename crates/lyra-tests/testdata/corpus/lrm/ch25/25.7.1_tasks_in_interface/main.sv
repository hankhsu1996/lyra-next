// LRM 25.7.1: Tasks and functions declared in an interface are callable
// through a plain (non-modport) interface port.

interface simple_bus;
  logic [7:0] data;
  logic valid;

  task Read(input logic [7:0] addr);
  endtask

  function void Write(input logic [7:0] addr, input logic [7:0] wdata);
  endfunction
endinterface

module consumer(simple_bus bus);
  initial begin
    bus.Read(8'hAA);
    bus.Write(8'h10, 8'hFF);
  end
endmodule
