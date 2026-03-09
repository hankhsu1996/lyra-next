// LRM 12.6.1 -- struct pattern in case matches
// ALLOW-EXTRA-DIAGS

module struct_pattern;
  typedef struct {
    int a;
    int b;
  } pair_t;

  pair_t p;
  int result;

  always_comb begin
    case (p) matches
      '{a: 0, b: 0}: result = 0;
      '{a: 1, b: .*}: result = 1;
      default: result = -1;
    endcase
  end
endmodule
