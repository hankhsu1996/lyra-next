// LRM 5.3: White space
//
// White space (spaces, tabs, newlines, formfeeds) shall be ignored except
// when they serve to separate other lexical tokens.

module	white_space_test;

  // Tab-separated declarations
  logic	a;
  logic		b;

  // Multiple spaces between tokens
  logic   [7:0]   c;

  // Newlines between tokens (free format)
  logic
    [3:0]
    d;

  // Formfeed as separator (ASCII 0x0C)
  logic e;

  // Mixed whitespace in expressions
  assign c = a  +	b;

endmodule
