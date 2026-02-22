// $bits constant evaluation (LRM 20.6.2)

// Type-form: keyword types
module bits_keyword_types;
  parameter P_INT       = $bits(int);
  parameter P_LOGIC     = $bits(logic);
  parameter P_LOGIC_VEC = $bits(logic [7:0]);
  parameter P_PACKED_2D = $bits(logic [3:0][7:0]);
  parameter P_REAL      = $bits(real);
  parameter P_SHORTREAL = $bits(shortreal);
  parameter P_BYTE      = $bits(byte);
endmodule

// Expr-form: variables and expressions
module bits_expr_form;
  logic [7:0] x;
  parameter int A = 0;

  parameter P_VAR   = $bits(x);
  parameter P_PARAM = $bits(A);
  parameter P_ADD   = $bits(x + 1);
endmodule

// Composition: $bits used in larger expressions
module bits_composition;
  parameter P = $bits(int) + 1;
endmodule

// Parameterized width
module bits_param_dim;
  parameter W = 8;
  logic [W-1:0] x;
  parameter P = $bits(x);
endmodule
