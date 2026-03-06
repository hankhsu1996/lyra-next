// LRM 7.8.1 -- Wildcard associative array cannot be used in foreach
//
// A wildcard associative array shall not be used in a foreach loop.

module foreach_illegal;

  int aa[*];

  initial foreach (aa[i]) begin
  // @foreach error[lyra.semantic.foreach_wildcard_assoc]: foreach loop cannot iterate over wildcard associative array
  end

endmodule
