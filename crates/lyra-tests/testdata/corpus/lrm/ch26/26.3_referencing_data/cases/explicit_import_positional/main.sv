// LRM 26.3: Explicit imports are also positional.
package p;
  int c;
endpackage

module top;
  wire a = c;
  //      ^ error[lyra.semantic[1]]: unresolved name `c`
  import p::c;
  wire b = c;
endmodule
