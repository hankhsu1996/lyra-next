// LRM 6.19: Enum value overflow when auto-incrementing past base type range

module enum_autoinc_overflow;

  // 1-bit unsigned: 0,1 are valid; c=2 overflows
  typedef enum bit [0:0] { a, b, c } overflow1_t;
  //                            ^ error[lyra.semantic[11]]

  // 2-bit unsigned: 0-3 are valid; q=4 overflows
  typedef enum bit [1:0] { p=3, q } overflow2_t;
  //                           ^ error[lyra.semantic[11]]

endmodule
