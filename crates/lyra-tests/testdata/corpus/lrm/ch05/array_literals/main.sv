// LRM 5.11: Array literals
//
// Array literals use assignment pattern syntax '{...}.

module array_literals_test;

  // 1D array literal
  int a[0:2] = '{10, 20, 30};

  // 2D array literal (nested)
  int b[1:2][1:3] = '{'{0, 1, 2}, '{3, 4, 5}};

  // Keyed array literal
  int c[0:2] = '{0:10, 1:20, 2:30};

  // Default array literal
  int d[0:2] = '{default:0};

endmodule
