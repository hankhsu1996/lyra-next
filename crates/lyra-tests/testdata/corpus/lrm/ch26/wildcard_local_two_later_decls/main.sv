// Two later locals in same scope after realization -- both illegal.
package p;
  int x;
endpackage

module top;
  import p::*;
  initial x = 1;
  int x;
  //  ^ error[lyra.semantic[7]]: local declaration of `x` conflicts with wildcard import from package `p`
  wire x;
  //   ^ error[lyra.semantic[7]]: local declaration of `x` conflicts with wildcard import from package `p`
endmodule
// ALLOW-EXTRA-DIAGS
