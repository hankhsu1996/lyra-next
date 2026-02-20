// LRM 26.6: export-triggered promotion and conflict fire independently per namespace.
package p1;
  int x;
  typedef int x_t;
endpackage

package p2;
  import p1::*;
  export p1::x;
  //    ^ error[lyra.semantic[7]]: import of `x` from package `p1` conflicts with local declaration
  export p1::x_t;
  //    ^ error[lyra.semantic[7]]: import of `x_t` from package `p1` conflicts with local declaration
  int x;
  typedef int x_t;
endpackage
