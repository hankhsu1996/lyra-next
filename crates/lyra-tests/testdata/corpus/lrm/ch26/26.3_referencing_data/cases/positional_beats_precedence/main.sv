// LRM 26.3: Position matters -- reference before ALL imports sees nothing.
package p;
  int x;
endpackage

package q;
  int x;
endpackage

module top;
  int a = x;
  // @x error[lyra.semantic.unresolved_name]: unresolved name `x`
  import q::x;
  // @q error[lyra.semantic.import_conflict]: import of `x` from package `q` conflicts with wildcard import from package `p`
  import p::*;
  int b = x;
endmodule
