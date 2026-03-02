// LRM 6.20.6: Const variable declarations
//
// A const variable may be declared with an initializer.
// Parsing succeeds and no diagnostics are emitted.

module const_var_decl;

  const int x = 1;
  const var logic [3:0] y = 4'hf;
  const logic z = 1'b0;

endmodule
