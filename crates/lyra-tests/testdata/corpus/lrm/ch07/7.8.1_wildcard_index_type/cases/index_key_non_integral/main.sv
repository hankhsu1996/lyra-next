// LRM 7.8.1 -- Wildcard associative array index must be integral
//
// Index expressions on wildcard associative arrays must be integral.
// A non-integral index (e.g. string) is an error.
// Tests both assignment and non-assignment (if condition) contexts.

module index_key_non_integral;

  int aa[*];
  string s;
  int r;

  assign r = aa[s];
  // @s error[lyra.type.index_key_not_integral]: index expression must be integral for wildcard associative array

  initial begin
    if (aa[s]) begin end
    // @s error[lyra.type.index_key_not_integral]: index expression must be integral for wildcard associative array
  end

endmodule
