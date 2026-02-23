// LRM 26.3: Explicit preceding wildcard -- explicit wins by precedence.
package p;
  int x;
endpackage

package q;
  int x;
endpackage

module top;
  import q::x;
  //    ^ error[lyra.semantic[7]]: import of `x` from package `q` conflicts with wildcard import from package `p`
  import p::*;
  int a = x;
endmodule
