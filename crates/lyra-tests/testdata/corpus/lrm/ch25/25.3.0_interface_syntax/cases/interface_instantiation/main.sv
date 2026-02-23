// LRM 25.3: Interface instantiated in a top module

interface simple_bus;
  logic req, gnt;
endinterface

module top;
  simple_bus sb();
endmodule
