// LRM 26.3: realization + later local is illegal.
package p;
  int x;
endpackage

module top;
  import p::*;
  initial x = 1;  // realizes p::x in scope top
  int x;
  // @x error[lyra.semantic.import_conflict]: local declaration of `x` conflicts with wildcard import from package `p`
endmodule
