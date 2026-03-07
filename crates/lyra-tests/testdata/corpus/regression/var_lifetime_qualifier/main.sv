// LRM 6.21: Variable-level lifetime qualifiers
//
// static and automatic qualifiers on variable declarations.
// Explicit automatic in non-procedural contexts is an error.

module m;
  static int a;
  int b;
  automatic int c;
  // @automatic error[lyra.decl.automatic_var_non_procedural]: automatic variable declarations are only allowed in procedural contexts

  static const int d = 1;

  initial begin
    static int e;
    automatic int f;
    int g;
  end
endmodule

package p;
  automatic int h;
  // @automatic error[lyra.decl.automatic_var_non_procedural]: automatic variable declarations are only allowed in procedural contexts
  static int i;
  int j;
endpackage
