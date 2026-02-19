// LRM 5.11: Array literals
//
// Array literals use assignment pattern syntax '{...}.
// Only positional form is tested here; keyed, default, and
// replication forms require parser support for assignment pattern keys.

module array_literals_test;

  // 1D array literal
  int a[0:2] = '{10, 20, 30};

  // 2D array literal (nested)
  int b[1:2][1:3] = '{'{0, 1, 2}, '{3, 4, 5}};

endmodule
