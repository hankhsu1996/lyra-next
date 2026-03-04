// LRM 3.13: Name spaces
//
// Two declarations of the same name in the same namespace
// within the same scope produce a duplicate definition error.

module dup_value;
  logic bar;
  logic bar;
  //    ^ error[lyra.semantic.duplicate_definition]: duplicate definition of `bar`
endmodule

module dup_type;
  typedef logic [7:0] baz;
  typedef logic [3:0] baz;
  //                  ^ error[lyra.semantic.duplicate_definition]: duplicate definition of `baz`
endmodule
