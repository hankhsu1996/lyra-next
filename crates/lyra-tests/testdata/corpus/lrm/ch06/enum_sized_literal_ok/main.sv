// LRM 6.19: Sized literal width matches base -- no diagnostic expected

module enum_sized_literal_ok;

  typedef enum bit [3:0] { gold=4'h5, platinum=4'b1010 } ok_t;

endmodule
