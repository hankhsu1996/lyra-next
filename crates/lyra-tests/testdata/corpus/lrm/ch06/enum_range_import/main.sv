// LRM 6.19.3: Cross-file import of range-generated enum variants

module top;
  import my_pkg::*;

  my_enum_t x;
  assign x = A2;
  assign x = A3;
  assign x = A4;
endmodule
