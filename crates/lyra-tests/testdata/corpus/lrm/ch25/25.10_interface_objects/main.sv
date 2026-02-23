// LRM 25.10: Accessing interface members through port reference

interface my_bus;
  logic [7:0] data;
  logic valid;
  modport reader(input data, valid);
endinterface

module consumer(my_bus.reader bus);
  logic [7:0] local_data;
  always_comb local_data = bus.data;
endmodule
