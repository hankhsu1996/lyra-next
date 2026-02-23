// LRM 26.6: export-triggered import surfaces the name for downstream consumers.
// x is never referenced in p4, but export p1::x realizes the wildcard import,
// making x available on p4's exported surface.
package p1;
  parameter int x = 42;
endpackage

package p4;
  import p1::*;
  export p1::x;
endpackage

module m;
  import p4::*;
  parameter int z = x;
endmodule
