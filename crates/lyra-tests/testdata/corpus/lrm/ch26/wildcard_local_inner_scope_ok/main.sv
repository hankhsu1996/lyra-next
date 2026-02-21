// Local in inner scope does not conflict with outer realization.
package p;
  int x;
endpackage

module top;
  import p::*;
  initial x = 1;
  if (1) begin : b
    int x;
    initial x = 1;
  end
endmodule
