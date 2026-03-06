// LRM 25.7: Prototype return type mismatch.
// The prototype declares `int` return but the actual function returns `logic [7:0]`.

interface bus_if;
  logic [7:0] data;

  function logic [7:0] Status(input logic [7:0] addr);
  endfunction

  modport slave(
    output data,
    import function int Status(input logic [7:0] addr)
//                      ^ error[lyra.semantic.prototype_mismatch]: prototype signature does not match declaration of `Status`: return type mismatch
  );
endinterface
