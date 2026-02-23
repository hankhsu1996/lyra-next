// LRM 6.19: Duplicate enum member values

module enum_duplicate_values;

  // Auto-increment collision: c auto-increments to 8, duplicates d=8
  typedef enum { a=0, b=7, c, d=8 } dup_autoinc_t;
  //                         ^ error[lyra.semantic[10]]

  // Explicit duplicate
  typedef enum { x=1, y=1 } dup_explicit_t;
  //                 ^ error[lyra.semantic[10]]

endmodule
