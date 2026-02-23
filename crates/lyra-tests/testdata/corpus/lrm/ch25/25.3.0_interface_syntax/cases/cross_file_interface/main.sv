// Modules using the interface from defs.sv

module source(shared_bus.producer bus);
endmodule

module sink(shared_bus.consumer bus);
endmodule

module top;
  shared_bus sb();
  source src(.bus(sb));
  sink snk(.bus(sb));
endmodule
