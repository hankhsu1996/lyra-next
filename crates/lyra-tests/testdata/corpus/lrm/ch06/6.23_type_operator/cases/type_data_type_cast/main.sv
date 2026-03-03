module type_data_type_cast;
  logic [31:0] v;
  int x;
  initial x = type(int)'(v);
endmodule
