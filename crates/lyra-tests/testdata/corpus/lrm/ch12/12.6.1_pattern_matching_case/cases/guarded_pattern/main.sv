// LRM 12.6.1 -- guarded pattern with &&& in case matches
// ALLOW-EXTRA-DIAGS

module guarded_pattern;
  typedef union tagged {
    void Invalid;
    int Valid;
  } maybe_int_t;

  maybe_int_t val;
  int x;
  int result;

  always_comb begin
    case (val) matches
      tagged Valid .n &&& x > 0: result = 1;
      tagged Valid .n: result = 0;
      tagged Invalid: result = -1;
    endcase
  end
endmodule
