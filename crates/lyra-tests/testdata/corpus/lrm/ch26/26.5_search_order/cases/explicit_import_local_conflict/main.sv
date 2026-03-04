// LRM 26.5: Explicit import conflicts with local declaration
package P;
  parameter int C = 42;
endpackage

module m;
  int C;
  import P::C;
  //    ^ error[lyra.semantic.import_conflict]: import of `C` from package `P` conflicts with local declaration
endmodule
