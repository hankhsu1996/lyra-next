// LRM 5.10: Structure literals
//
// Structure literals use assignment pattern syntax '{...}.

module structure_literals_test;

  typedef struct {int a; int b;} ab;
  ab c;

  // Positional assignment pattern
  initial begin
    c = '{0, 1};
  end

  // Keyed assignment pattern
  initial begin
    c = '{a:0, b:1};
    //    ^ error[lyra.semantic[1]]: unresolved name `a`
    //        ^ error[lyra.semantic[1]]: unresolved name `b`
  end

  // Default assignment pattern
  initial begin
    c = '{default:0};
  end

  // Nested: array of structures
  ab arr[1:0];
  initial begin
    arr = '{'{1, 2}, '{3, 4}};
  end

endmodule
