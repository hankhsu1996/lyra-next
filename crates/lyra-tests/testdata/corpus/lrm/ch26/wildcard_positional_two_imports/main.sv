// LRM 26.3 Example 4: only the preceding wildcard import is considered.
// Inner generate block sees only p::* (before it), not p2::* (after it).
package p;
  int val;
endpackage

package p2;
  int val;
endpackage

module top;
  import p::*;
  int x;
  if (1) begin : b
    initial x = val;
  end
  import p2::*;
endmodule
