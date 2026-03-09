// LRM 12.6.2 -- matches with &&& guard in if statement
// ALLOW-EXTRA-DIAGS

module guarded_matches_if;
  typedef union tagged {
    void Invalid;
    int Valid;
  } maybe_int_t;

  maybe_int_t val;
  int result;

  always_comb begin
    if (val matches tagged Valid .n &&& n > 0)
      result = n;
    else
      result = 0;
  end
endmodule
