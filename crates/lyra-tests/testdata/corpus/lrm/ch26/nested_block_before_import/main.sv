// LRM 26.3: Reference inside nested block BEFORE outer import --
// confirms order_key comparison works across scope nesting.
package p;
  int val;
endpackage

module top;
  if (1) begin : blk
    int x = val;
    //     ^ error[lyra.semantic[1]]: unresolved name `val`
  end
  import p::*;
endmodule
