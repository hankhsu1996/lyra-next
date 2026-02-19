// LRM 5.13: Built-in methods
//
// Built-in methods use dot notation: object.method().
// Only basic method-call syntax on declared variables is tested here;
// dynamic arrays and other complex types require parser support.

module builtin_methods_test;

  string s = "hello";
  int len;

  initial begin
    // Method call with parentheses
    len = s.len();

    // Method call in expression
    $display("length = %0d", s.len());
  end

endmodule
