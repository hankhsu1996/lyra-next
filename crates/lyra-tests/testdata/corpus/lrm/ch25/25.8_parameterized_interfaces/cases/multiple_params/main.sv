// LRM 25.8: Multiple parameters with partial named override

interface data_bus #(parameter int WIDTH = 8, parameter int DEPTH = 4)();
  logic [WIDTH-1:0] data;
  logic [DEPTH-1:0] addr;
endinterface

module top;
  data_bus #(.WIDTH(32)) db();
endmodule
