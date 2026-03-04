// LRM 26.3: Explicit imports are also positional.
package p;
  int c;
endpackage

module top;
  wire a = c;
  //      ^ error[lyra.semantic.unresolved_name]: unresolved name `c`
  import p::c;
  wire b = c;
endmodule
