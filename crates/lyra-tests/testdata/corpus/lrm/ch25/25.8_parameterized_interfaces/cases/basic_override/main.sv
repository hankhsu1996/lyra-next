// LRM 25.8: Parameterized interface with named override

interface simple_bus #(parameter int WIDTH = 8)();
  logic [WIDTH-1:0] data;
endinterface

module top;
  simple_bus #(.WIDTH(16)) sb();
endmodule
