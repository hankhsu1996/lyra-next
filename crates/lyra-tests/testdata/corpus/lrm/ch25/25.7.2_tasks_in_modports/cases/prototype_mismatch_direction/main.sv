// LRM 25.7: Prototype port direction mismatch.
// The prototype declares `output` but the actual task has `input`.

interface bus_if;
  logic [7:0] data;

  task Read(input logic [7:0] addr);
  endtask

  modport slave(
    output data,
    import task Read(output logic [7:0] addr)
// @Read error[lyra.semantic.prototype_mismatch]: prototype signature does not match declaration of `Read`: port 1 direction mismatch: prototype has `output` but declaration has `input`
  );
endinterface
