// LRM 6.20.6: Assignment to const variable
//
// A const variable is not assignable.

module assign_to_const;

  const int x = 1;
  initial begin
    x = 2;
// @x error[lyra.type.assign_to_const]: cannot assign to const variable `x`
  end

endmodule
