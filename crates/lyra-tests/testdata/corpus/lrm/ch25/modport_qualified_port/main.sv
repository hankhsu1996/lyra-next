// LRM 25.5: Module port with modport-qualified interface type

interface my_bus;
  logic req, gnt;
  modport master(output req, input gnt);
endinterface

module producer(my_bus.master bus);
endmodule

module top;
  my_bus b();
  producer p(.bus(b));
endmodule
