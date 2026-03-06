// Two later locals in same scope after realization -- both illegal.
package p;
  int x;
endpackage

module top;
  import p::*;
  initial x = 1;
  int x;
  // @x error[lyra.semantic.import_conflict]: local declaration of `x` conflicts with wildcard import from package `p`
  wire x;
  // @x error[lyra.semantic.duplicate_definition]: duplicate definition of `x`
  // @x error[lyra.semantic.import_conflict]: local declaration of `x` conflicts with wildcard import from package `p`
endmodule
