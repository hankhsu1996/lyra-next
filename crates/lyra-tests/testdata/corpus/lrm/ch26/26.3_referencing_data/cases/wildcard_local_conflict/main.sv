// LRM 26.3: realization + later local is illegal.
package p;
  int x;
endpackage

module top;
  import p::*;
  initial x = 1;  // realizes p::x in scope top
  int x;
  //  ^ error[lyra.semantic[7]]: local declaration of `x` conflicts with wildcard import from package `p`
endmodule
// ALLOW-EXTRA-DIAGS
