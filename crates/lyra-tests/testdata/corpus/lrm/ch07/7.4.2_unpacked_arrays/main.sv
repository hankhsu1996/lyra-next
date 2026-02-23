// LRM 7.4.2, 7.4.4 -- Unpacked array declaration and indexing
//
// Tests unpacked array with fixed ranges, fixed sizes, and multi-dim.

module unpacked_array_decl;

  // Fixed-size unpacked array
  int arr_size [8];

  // Fixed-range unpacked array
  int arr_range [0:7];

  // Multi-dimensional unpacked array
  int arr_2d [4][8];

  // Mixed packed + unpacked
  logic [7:0] mixed [4];

  // Indexing into unpacked array
  int elem;
  assign elem = arr_size[3];

  // Multi-dim indexing
  int elem_2d;
  assign elem_2d = arr_2d[1][2];

  // Mixed indexing: unpacked then packed
  logic [7:0] row;
  assign row = mixed[2];

  // $dimensions on mixed
  parameter D = $dimensions(mixed);

endmodule
