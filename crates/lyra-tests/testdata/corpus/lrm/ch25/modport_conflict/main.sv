// LRM 25.5: Modport conflict detection setup
// Verifies that modport-qualified interface types resolve correctly.
// The actual conflict diagnostics are elaboration-time checks
// tested in the elab test suite.

interface bus;
  logic req, gnt;
  logic [7:0] data;
  modport master(output req, input gnt);
  modport slave(input req, output gnt);
endinterface

// Module that expects a specific modport
module wants_master(bus.master p);
endmodule

// Passing same modport: resolution succeeds
module test_same(bus.master bm);
  wants_master u1(.p(bm));
endmodule

// Passing different modport: resolution still succeeds (conflict is elab-time)
module test_diff(bus.slave bs);
  wants_master u1(.p(bs));
endmodule

// Passing bare interface: resolution succeeds
module test_bare(bus b_port);
  wants_master u1(.p(b_port));
endmodule

// Explicit modport selection via dot: intf.modport resolves correctly
module test_explicit(bus b_port);
  wants_master u1(.p(b_port.slave));
endmodule

// Modport selection matching formal: resolves correctly
module test_match(bus b_port);
  wants_master u1(.p(b_port.master));
endmodule
