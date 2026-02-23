// LRM 26.3: wildcard imports are only candidates for references
// that appear after the import in source order.
package p;
  int val;
endpackage

module top;
  int x = val;
  //     ^ error[lyra.semantic[1]]: unresolved name `val`
  import p::*;
  int y = val;
endmodule
