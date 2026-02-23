// LRM 25.3: Interface instance names resolve in port connections

interface simple_bus;
  logic req, gnt;
  logic [7:0] data;
endinterface

module producer(simple_bus bus);
endmodule

module consumer(simple_bus bus);
endmodule

module top;
  simple_bus sb();
  producer p(.bus(sb));
  consumer c(.bus(sb));
endmodule
