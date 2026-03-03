// LRM 7.11 / 20.7.1 -- Dimension argument selects a variable-sized dim
//
// When the second argument to a range query selects a dimension that
// is variable-sized (dynamic, queue, or associative), it is an error.

module array_query_error_dim_by_number;

  // int a[3][][5]: dim 1 = [3] (fixed), dim 2 = [] (dynamic), dim 3 = [5] (fixed)
  int a [3][][5];
  int r;

  assign r = $size(a, 2);
  //                 ^ error[lyra.type[34]]

  // int b[][5]: dim 1 = [] (dynamic), dim 2 = [5] (fixed)
  int b [][5];

  assign r = $size(b, 1);
  //                 ^ error[lyra.type[34]]

  // Fixed dim: no error
  assign r = $size(a, 1);
  assign r = $size(a, 3);

endmodule
