// LRM 7.5: Dynamic array new[] constructor
//
// Tests new[] constructor typing and diagnostics.

module new_constructor;

  // Happy path: new[] on dynamic array
  int dyn_arr [];
  int init_src [3];
  initial begin
    dyn_arr = new[10];
    dyn_arr = new[3](init_src);
  end

  // Happy path: var init with new[]
  int dyn_init [] = new[5];

  // Error: new[] on fixed array target
  int fixed_arr [5];
  initial begin
    fixed_arr = new[5];
  //           ^ error[lyra.type[19]]
  end

  // Error: negative size
  initial begin
    dyn_arr = new[-1];
  //         ^ error[lyra.type[22]]
  end

endmodule
