// LRM 12.6.1 -- basic tagged pattern in case matches
// ALLOW-EXTRA-DIAGS

module basic_tagged_pattern;
  typedef union tagged {
    void Invalid;
    int Valid;
  } maybe_int_t;

  maybe_int_t val;
  int result;

  always_comb begin
    case (val) matches
      tagged Valid .n: result = 1;
      tagged Invalid: result = 0;
    endcase
  end
endmodule
