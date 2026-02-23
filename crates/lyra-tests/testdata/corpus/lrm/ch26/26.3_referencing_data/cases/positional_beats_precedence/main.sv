// LRM 26.3: Position matters -- reference before ALL imports sees nothing.
package p;
  int x;
endpackage

package q;
  int x;
endpackage

module top;
  int a = x;
  //     ^ error[lyra.semantic[1]]: unresolved name `x`
  import q::x;
  //    ^ error[lyra.semantic[7]]: import of `x` from package `q` conflicts with wildcard import from package `p`
  import p::*;
  int b = x;
endmodule
