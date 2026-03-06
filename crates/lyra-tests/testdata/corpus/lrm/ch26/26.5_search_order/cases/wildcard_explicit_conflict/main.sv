// LRM 26.5: Wildcard and explicit import conflict
package P;
  parameter int C = 10;
endpackage
package Q;
  parameter int C = 20;
endpackage

module m;
  import Q::*;
  import P::C;
  // @P error[lyra.semantic.import_conflict]: import of `C` from package `P` conflicts with wildcard import from package `Q`
endmodule
