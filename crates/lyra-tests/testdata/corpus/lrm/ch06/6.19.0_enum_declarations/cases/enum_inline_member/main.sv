// LRM 6.19: Enumerations -- inline enum member resolution
//
// Enum members from an inline (non-typedef) enum declaration
// are also injected into the enclosing scope.

module enum_inline_member;

  enum { RED, GREEN, BLUE } color;

  assign color = GREEN;

endmodule
