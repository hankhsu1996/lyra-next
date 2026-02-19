// LRM 5.5: Operators

// Arithmetic operators
module arithmetic_ops;
  int a = 3 + 4;
  int b = 5 - 2;
  int c = 3 * 4;
  int d = 8 / 2;
  int e = 7 % 3;
  int f = 2 ** 3;
endmodule

// Bitwise operators
module bitwise_ops;
  int x = 10;
  int y = 12;
  int a = x & y;
  int b = x | y;
  int c = x ^ y;
  int d = ~x;
  int e = x ~^ y;
endmodule

// Logical operators
module logical_ops;
  int x = 5;
  int y = 0;
  logic a = (x && y);
  logic b = (x || y);
  logic c = !x;
endmodule

// Comparison operators
module comparison_ops;
  int x = 5;
  int y = 3;
  logic a = (x == y);
  logic b = (x != y);
  logic c = (x < y);
  logic d = (x <= y);
  logic e = (x > y);
  logic f = (x >= y);
  logic g = (x === y);
  logic h = (x !== y);
endmodule

// Shift operators
module shift_ops;
  int a = 8 >> 1;
  int b = 8 << 2;
  int c = -8 >>> 1;
  int d = 8 <<< 1;
endmodule

// Reduction operators
module reduction_ops;
  logic a = &4'b1111;
  logic b = |4'b0001;
  logic c = ^4'b1010;
  logic d = ~&4'b1110;
  logic e = ~|4'b0000;
  logic f = ~^4'b1010;
endmodule

// Conditional operator
module conditional_op;
  int x = 5;
  int y = 3;
  int max = (x > y) ? x : y;
endmodule
