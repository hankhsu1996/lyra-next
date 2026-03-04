// LRM 12.7.3 -- more loop variables than dimensions
//
// A 1D array iterated with two variables is an error.

module too_many_vars;

  int arr [4];

  initial begin
    foreach (arr[i, j]) begin
//                  ^ error[lyra.semantic[25]]: foreach has 2 loop variables but iterated expression has only 1 dimensions
    end
  end

endmodule
