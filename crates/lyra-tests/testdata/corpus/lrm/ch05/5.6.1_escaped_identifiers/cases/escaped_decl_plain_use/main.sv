// LRM 5.6.1: Escaped identifier declared, plain identifier used.
// \cpu3 and cpu3 denote the same identifier.

module escaped_decl_plain_use;
  int \cpu3 ;
  int \data_bus ;

  initial begin
    cpu3 = 42;
    data_bus = 7;
  end
endmodule
