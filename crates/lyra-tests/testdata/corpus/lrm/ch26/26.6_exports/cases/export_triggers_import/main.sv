// LRM 26.6: export of unreferenced candidate counts as reference.
// x is never used in p6, but export p1::x realizes the wildcard candidate.
package p1;
  int x;
endpackage

package p6;
  import p1::*;
  export p1::x;
  //    ^ error[lyra.semantic[7]]: import of `x` from package `p1` conflicts with local declaration
  int x;
endpackage
