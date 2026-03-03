// LRM 3.13: Name spaces
//
// The definition namespace (section 3.13(a)) holds modules,
// primitives, programs, and interfaces. These are resolved
// by name for instantiation, independently of value/type scopes.

module inner;
endmodule

module outer;
  inner u0();
endmodule
