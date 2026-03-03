module type_expr_cast;
  int a;
  logic [31:0] v;
  initial a = type(a)'(v);
endmodule
