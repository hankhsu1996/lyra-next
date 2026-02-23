// LRM 6.19: Sized literal width must match enum base type width

module enum_sized_literal_width;

  // 5-bit and 3-bit literals do not match 4-bit base
  typedef enum bit [3:0] { bronze=5'h3, silver=3'h5 } mismatch_t;
  //                      ^ error[lyra.semantic[12]]
  //                                   ^ error[lyra.semantic[12]]

endmodule
