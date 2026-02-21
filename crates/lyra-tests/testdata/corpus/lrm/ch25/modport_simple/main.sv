// LRM 25.5: Modport declarations with input/output directions
// Module uses modport-qualified port

interface my_bus;
  logic req, gnt;
  logic [7:0] data;
  modport master(input gnt, output req, data);
  modport slave(output gnt, input req, data);
endinterface

module producer(my_bus.master bus);
endmodule
