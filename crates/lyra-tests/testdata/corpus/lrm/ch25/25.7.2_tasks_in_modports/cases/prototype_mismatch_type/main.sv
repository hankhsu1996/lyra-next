// LRM 25.7: Prototype port type mismatch.
// The prototype declares `logic [15:0]` but the actual task has `logic [7:0]`.

interface bus_if;
  logic [7:0] data;

  task Read(input logic [7:0] addr);
  endtask

  modport slave(
    output data,
    import task Read(input logic [15:0] addr)
//              ^ error[lyra.semantic.prototype_mismatch]: prototype signature does not match declaration of `Read`: port 1 type mismatch
  );
endinterface
