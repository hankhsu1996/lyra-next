// LRM 12.7.3 -- loop variable same name as iterated array
//
// A loop variable sharing the name of the iterated array is flagged.

module var_same_name_as_array;

  int arr [4];

  initial begin
    foreach (arr[arr]) begin
//               ^ error[lyra.semantic[24]]: foreach loop variable has same name as iterated array `arr`
    end
  end

endmodule
