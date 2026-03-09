// LRM 12.6.2 -- basic matches in if statement
// ALLOW-EXTRA-DIAGS

module basic_matches_if;
  typedef union tagged {
    void Invalid;
    int Valid;
  } maybe_int_t;

  maybe_int_t val;
  int result;

  always_comb begin
    if (val matches tagged Valid .n)
      result = n;
    else
      result = 0;
  end
endmodule
