// LRM 26.6: Two wildcard imports from packages with independently
// declared symbols of the same name are a conflict.
package P;
  parameter int x = 10;
endpackage
package Q;
  parameter int x = 20;
endpackage

module m;
  import P::*;
  import Q::*;
  parameter int y = x;
  // @x error[lyra.semantic.ambiguous_import]: name `x` is ambiguous: imported from packages `P`, `Q`
endmodule
