// LRM 26.5b + 26.6: export-triggered promotion conflicts with wildcard from
// a different package. export P::x promotes the wildcard candidate from P,
// which then conflicts with Q's wildcard candidate for the same name.
package P;
  parameter int x = 1;
endpackage

package Q;
  parameter int x = 2;
endpackage

package R;
  import P::*;
  import Q::*;
  export P::x;
  //    ^ error[lyra.semantic[7]]: import of `x` from package `P` conflicts with wildcard import from package `Q`
endpackage
