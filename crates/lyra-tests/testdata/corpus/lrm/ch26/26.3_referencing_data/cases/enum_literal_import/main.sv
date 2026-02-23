// LRM 26.3: Enum literal import semantics (positive cases)

module wildcard_test;
  import p::*;
  bool_t v;
  assign v = FALSE;
endmodule

module explicit_literal_test;
  import q::teeth_t;
  import q::ORIGINAL;
  teeth_t t;
  assign t = ORIGINAL;
endmodule

module qualified_literal_test_q;
  logic v;
  assign v = q::FALSE;
endmodule

module qualified_literal_test_p;
  logic v;
  assign v = p::TRUE;
endmodule
