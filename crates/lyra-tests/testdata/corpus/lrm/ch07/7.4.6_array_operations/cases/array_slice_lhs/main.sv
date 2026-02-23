// LRM 7.4.6: Array slice as assignment target
module array_slice_lhs;
  int arr [7:0];
  int pair [1:0];
  int trio [2:0];
  initial begin
    arr[3:2] = pair;
    arr[1+:3] = trio;
    arr[7-:3] = trio;
  end
endmodule
