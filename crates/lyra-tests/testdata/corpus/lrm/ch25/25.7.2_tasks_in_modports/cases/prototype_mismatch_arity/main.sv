// LRM 25.7: Prototype port count mismatch.
// The prototype declares 2 ports but the actual task has 1.

interface bus_if;
  logic [7:0] data;

  task Read(input logic [7:0] addr);
  endtask

  modport slave(
    output data,
    import task Read(input logic [7:0] addr, input logic [7:0] extra)
// @Read error[lyra.semantic.prototype_mismatch]: prototype signature does not match declaration of `Read`: prototype has 2 ports but declaration has 1
  );
endinterface
