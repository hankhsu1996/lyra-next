// LRM 26.3: Enum literal import semantics (negative cases)

module type_import_no_literals;
  import q::teeth_t;
  teeth_t t;
  assign t = ORIGINAL;
  // @ORIGINAL error[lyra.semantic.unresolved_name]: unresolved name `ORIGINAL`
endmodule

module wildcard_ambiguity;
  import p::*;
  import q::*;
  logic x;
  assign x = FALSE;
  // @FALSE error[lyra.semantic.ambiguous_import]: name `FALSE` is ambiguous: imported from packages `p`, `q`
endmodule

module wildcard_local_enum_conflict;
  import p::*;
  logic v;
  assign v = FALSE;
  logic FALSE;
  // @FALSE error[lyra.semantic.import_conflict]: local declaration of `FALSE` conflicts with wildcard import from package `p`
endmodule
