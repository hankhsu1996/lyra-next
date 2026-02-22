// LRM 7.11 / 20.7 -- Array querying system functions
//
// Tests $dimensions, $unpacked_dimensions, $left, $right, $low, $high,
// $size, $increment on fixed packed/unpacked types via const-eval.

module array_query_functions;

  // $dimensions on keyword types
  parameter D_INT = $dimensions(int);           // 1 (implicit [31:0])
  parameter D_BYTE = $dimensions(byte);         // 1 (implicit [7:0])
  parameter D_LOGIC = $dimensions(logic);       // 1 (scalar bit)

  // $dimensions on packed types
  parameter D_VEC = $dimensions(logic [7:0]);           // 1
  parameter D_2D  = $dimensions(logic [3:0][7:0]);      // 2

  // $dimensions on variables with unpacked dims
  logic [7:0] arr1 [4];
  parameter D_ARR1 = $dimensions(arr1);         // 2 (1 unpacked + 1 packed)

  logic [3:0][7:0] arr2 [10][5];
  parameter D_ARR2 = $dimensions(arr2);         // 4 (2 unpacked + 2 packed)

  // $unpacked_dimensions
  parameter UD_INT  = $unpacked_dimensions(int);        // 0
  parameter UD_ARR1 = $unpacked_dimensions(arr1);       // 1
  parameter UD_ARR2 = $unpacked_dimensions(arr2);       // 2

  // $size on packed type (dim 1 = only packed dim)
  parameter SZ_VEC = $size(logic [7:0]);        // 8

  // $size on unpacked dim (dim 1 = outermost unpacked)
  int arr3 [10];
  parameter SZ_ARR3 = $size(arr3);              // 10

  // $left/$right/$low/$high/$increment on packed type
  parameter L_VEC = $left(logic [7:0]);         // 7
  parameter R_VEC = $right(logic [7:0]);        // 0
  parameter LO_VEC = $low(logic [7:0]);         // 0
  parameter HI_VEC = $high(logic [7:0]);        // 7
  parameter INC_VEC = $increment(logic [7:0]);  // 1

  // Ascending range
  parameter L_ASC = $left(logic [0:7]);         // 0
  parameter R_ASC = $right(logic [0:7]);        // 7
  parameter INC_ASC = $increment(logic [0:7]);  // -1

  // $size with explicit dim argument
  logic [7:0] arr4 [4];
  parameter SZ_D1 = $size(arr4, 1);             // 4 (unpacked dim)
  parameter SZ_D2 = $size(arr4, 2);             // 8 (packed dim)

  // $left on int (implicit [31:0])
  parameter L_INT = $left(int);                 // 31
  parameter SZ_INT = $size(int);                // 32

endmodule
