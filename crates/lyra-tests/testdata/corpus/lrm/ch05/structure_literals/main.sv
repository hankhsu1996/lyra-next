// LRM 5.10: Structure literals
//
// Structure literals use assignment pattern syntax '{...}.
// Only positional form is tested here; keyed and default forms
// require parser support for assignment pattern keys.

module structure_literals_test;

  typedef struct {int a; int b;} ab;
  ab c;

  // Positional assignment pattern
  initial begin
    c = '{0, 1};
  end

  // Nested: array of structures
  ab arr[1:0];
  initial begin
    arr = '{'{1, 2}, '{3, 4}};
  end

endmodule
