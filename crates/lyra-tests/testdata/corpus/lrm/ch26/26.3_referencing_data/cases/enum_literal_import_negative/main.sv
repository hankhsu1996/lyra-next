// LRM 26.3: Enum literal import semantics (negative cases)

module type_import_no_literals;
  import q::teeth_t;
  teeth_t t;
  assign t = ORIGINAL;
  //        ^ error[lyra.semantic[1]]: unresolved name `ORIGINAL`
endmodule

module wildcard_ambiguity;
  import p::*;
  import q::*;
  logic x;
  assign x = FALSE;
  //        ^ error[lyra.semantic[5]]: name `FALSE` is ambiguous: imported from packages `p`, `q`
endmodule

module wildcard_local_enum_conflict;
  import p::*;
  logic v;
  assign v = FALSE;
  logic FALSE;
  //    ^ error[lyra.semantic[7]]: local declaration of `FALSE` conflicts with wildcard import from package `p`
endmodule
