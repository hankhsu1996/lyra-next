// LRM 25.5: Modport direction enforcement
// Verifies that accessing modport members respects declared direction

interface my_bus;
  logic req, gnt;
  logic [7:0] data;
  logic enable;
  modport master(input gnt, output req, data, inout enable);
  modport slave(output gnt, input req, data, inout enable);
endinterface

// Legal accesses: no diagnostics expected
module legal_use(my_bus.master bus);
  logic local_gnt;
  logic local_req;
  logic local_en;
  always_comb local_gnt = bus.gnt;
  always_comb bus.req = local_req;
  always_comb bus.data = 8'hFF;
  always_comb local_en = bus.enable;
  always_comb bus.enable = local_en;
endmodule

// Illegal: write to input member
module write_to_input(my_bus.master bus);
  always_comb bus.gnt = 1'b1;
  //              ^ error[lyra.type[10]]: modport member declared 'input' cannot be used in write context
endmodule

// Illegal: read from output member
module read_from_output(my_bus.slave bus);
  logic local_gnt;
  always_comb local_gnt = bus.gnt;
  //                          ^ error[lyra.type[10]]: modport member declared 'output' cannot be used in read context
endmodule

// Compound assignment on input member
module compound_on_input(my_bus.slave bus);
  always_comb bus.data += 8'h01;
  //              ^ error[lyra.type[10]]: modport member declared 'input' cannot be used in write context
endmodule
