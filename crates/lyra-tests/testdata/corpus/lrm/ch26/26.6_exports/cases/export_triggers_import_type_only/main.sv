// LRM 26.6: export triggers import only in the namespace where the name exists.
// foo_t exists only in the type namespace (typedef), not value.
package p1;
  typedef int foo_t;
endpackage

package p2;
  import p1::*;
  export p1::foo_t;
  //    ^ error[lyra.semantic[7]]: import of `foo_t` from package `p1` conflicts with local declaration
  typedef logic foo_t;
endpackage
