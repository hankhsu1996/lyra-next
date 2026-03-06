// LRM 7.8.1 -- Wildcard associative array method restrictions
//
// first, last, next, prev require a concrete index type and are
// illegal on wildcard associative arrays.

module method_restriction;

  int aa[*];
  int k;
  int r;

  initial begin
    r = aa.first(k);
    // @first error[lyra.type.method_call_error]
    r = aa.last(k);
    // @last error[lyra.type.method_call_error]
  end

endmodule
