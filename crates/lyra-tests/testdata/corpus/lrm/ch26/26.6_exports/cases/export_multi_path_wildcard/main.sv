// LRM 26.6: Two wildcard imports that resolve to the same original
// declaration through different export paths are not a conflict.
package P;
  parameter int x = 1;
endpackage
package Q;
  import P::*;
  export P::*;
endpackage

module m;
  import P::*;
  import Q::*;
  parameter int y = x;
endmodule
