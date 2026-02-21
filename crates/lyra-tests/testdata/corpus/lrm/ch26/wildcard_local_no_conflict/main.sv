// Local declared before any realization -- no conflict.
package p;
  int x;
endpackage

module top;
  import p::*;
  int x;
  initial x = 1;
endmodule
