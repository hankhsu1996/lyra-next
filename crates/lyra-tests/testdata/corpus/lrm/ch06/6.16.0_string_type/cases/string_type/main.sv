// LRM 6.16: String data type
//
// The string type holds variable-length character strings.
// Default initial value is "" (empty string, Table 6-7).
// 6.16.1: .len() returns the string length as int.

module string_type;

  // Declaration with and without initializer
  string s1;
  string s2 = "hello";
  string s3 = "world";

  // .len() method (LRM 6.16.1)
  int length;
  initial begin
    length = s2.len();
  end

  // Empty string is the default (Table 6-7)
  string empty;

endmodule
