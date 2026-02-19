// LRM 5.9: String literals
//
// Tests quoted strings, triple-quoted strings, escape sequences,
// and string usage in expressions.

module string_literals_test;

  // Basic quoted strings
  string s1 = "hello world";
  string s2 = "";
  string s3 = "escape: \n \t \\ \"";

  // Octal and hex escapes
  string s4 = "\101";
  string s5 = "\x41";

  // Triple-quoted strings
  string s6 = """triple quoted""";
  string s7 = """contains "double quotes" inside""";

  // String as packed array
  logic [8*5-1:0] packed_str;

  // String in expressions
  initial begin
    packed_str = "hello";
    $display("message: %s", s1);
    $display("concatenated: %s %s", s1, s3);
  end

endmodule
