// LRM 5.13: Built-in methods
//
// Built-in methods use dot notation: object.method().

module builtin_methods_test;

  string s = "hello";
  int len;

  // Dynamic array with new
  int dyn[];

  initial begin
    // Method call with parentheses
    len = s.len();

    // Method call in expression
    $display("length = %0d", s.len());

    // Dynamic array new expression
    dyn = new[3];

    // Dynamic array size method
    len = dyn.size();
  end

endmodule
