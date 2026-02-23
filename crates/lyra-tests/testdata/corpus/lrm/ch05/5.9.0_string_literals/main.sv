// LRM 5.9: String literal syntax, triple-quoted strings, packed array assignment

module string_literals_test;

  // Basic quoted strings
  string s1 = "hello world";
  string s2 = "";

  // Triple-quoted strings
  string s3 = """triple quoted""";
  string s4 = """contains "double quotes" inside""";

  // String as packed array
  logic [8*5-1:0] packed_str;

  // String in expressions
  initial begin
    packed_str = "hello";
    $display("message: %s", s1);
    $display("concatenated: %s %s", s1, s3);
  end

endmodule
