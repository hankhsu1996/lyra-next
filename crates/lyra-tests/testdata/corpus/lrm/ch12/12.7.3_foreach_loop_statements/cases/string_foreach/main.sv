// LRM 12.7.3 -- foreach over a string
//
// A string has one iterable dimension with int index type.
// No diagnostics expected.

module string_foreach;

  string s;

  initial begin
    foreach (s[i]) begin
    end
  end

endmodule
