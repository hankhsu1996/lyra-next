// LRM 12.7.3 -- Basic foreach over a fixed-size array
//
// Loop variable `i` iterates over the single unpacked dimension.
// No diagnostics expected.

module basic_fixed_array;

  int arr [4];

  initial begin
    foreach (arr[i]) begin
      arr[i] = i;
    end
  end

endmodule
