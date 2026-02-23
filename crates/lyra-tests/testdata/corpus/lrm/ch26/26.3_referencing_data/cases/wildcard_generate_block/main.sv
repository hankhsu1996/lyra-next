// LRM 26.3: Wildcard import in outer scope visible to inner
// generate block (positional: import precedes the block).
package p;
  int val;
endpackage

module top;
  import p::*;
  if (1) begin : blk
    int x = val;
  end
endmodule
