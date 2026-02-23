// LRM 25.3: Module port typed as an interface (no modport qualification)

interface my_bus;
  logic [7:0] data;
  logic valid;
endinterface

module sink(my_bus b);
endmodule

module top;
  my_bus bus_inst();
  sink u1(.b(bus_inst));
endmodule
