// LRM 12.7.3 -- foreach loop variables are read-only
//
// Assigning to a loop variable is illegal.

module assign_to_loop_var;

  int arr [4];

  initial begin
    foreach (arr[i]) begin
      i = 0;
//    ^ error[lyra.semantic[23]]: cannot assign to foreach loop variable `i`
    end
  end

endmodule
