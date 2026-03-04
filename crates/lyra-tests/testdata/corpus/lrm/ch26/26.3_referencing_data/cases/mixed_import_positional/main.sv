// LRM 26.3: Mixed explicit + wildcard ordering.
package p;
  int x;
endpackage

package q;
  int x;
endpackage

module top;
  import p::*;
  int a = x;
  import q::x;
  //    ^ error[lyra.semantic.import_conflict]: import of `x` from package `q` conflicts with wildcard import from package `p`
  int b = x;
endmodule
