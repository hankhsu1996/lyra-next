// LRM 5.9.1: Special characters in strings

module string_special_chars;

  // Standard escape sequences
  string s1 = "escape: \n \t \\ \"";

  // Octal escape
  string s2 = "\101";

  // Hex escape
  string s3 = "\x41";

endmodule
