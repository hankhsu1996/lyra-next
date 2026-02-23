// LRM 26.3: explicit import in descendant scope takes precedence
// over wildcard in ancestor -- use-site does not realize wildcard.
package p;
  int x;
endpackage

module top;
  import p::*;
  if (1) begin : b
    import p::x;
    initial x = 1;  // resolves via explicit import, not wildcard
  end
  int x;  // OK: no wildcard realization in scope top
endmodule
