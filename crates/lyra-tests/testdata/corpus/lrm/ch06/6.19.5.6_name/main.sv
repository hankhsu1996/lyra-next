// LRM 6.19.5.6: name() method
//
// Returns the string representation of the enum value's name.
// Return type is string.

module enum_name;

  typedef enum { RED, GREEN, BLUE } color_t;
  color_t c;

  // name() returns string
  string s = c.name();

endmodule
