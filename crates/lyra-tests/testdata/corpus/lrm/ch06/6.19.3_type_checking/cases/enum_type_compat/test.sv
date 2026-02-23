// LRM 6.19.5 -- Enum type compatibility and cast expressions
module enum_type_compat;

  typedef enum logic [1:0] { RED=0, GREEN=1, BLUE=2 } Colors;
  typedef enum logic [1:0] { X=0, Y=1, Z=2 } OtherColors;

  Colors c;
  OtherColors d;
  int i;

  // Cast expression: enum type cast is legal
  initial begin
    c = Colors'(1);
  end

  // Enum to int: integral view (legal)
  assign i = c;

  // Int to enum without cast: error
  assign c = i;
  //        ^ error[lyra.type[7]]

  // Cross-enum assignment: error
  assign c = d;
  //        ^ error[lyra.type[7]]

  // Builtin type cast
  initial begin
    i = int'(c);
  end

  // Out-of-range cast value: warning
  typedef enum int { A=1, B=3 } E;
  E e;
  initial begin
    e = E'(2);
    // ^ warning[lyra.type[12]]
  end

endmodule
