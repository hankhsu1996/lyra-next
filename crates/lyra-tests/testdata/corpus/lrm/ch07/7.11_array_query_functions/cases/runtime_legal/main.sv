// LRM 7.11 / 20.7 -- Legal runtime array queries
//
// Value-form array queries on dynamic types are legal at runtime
// (they return the current dimension state). No diagnostics expected.

module array_query_runtime_legal;

  int dyn_arr [];
  int queue_var [$];
  int fixed_arr [8];

  initial begin
    int n;
    n = $size(dyn_arr);
    n = $size(queue_var);
    n = $left(dyn_arr);
    n = $right(queue_var);
    n = $low(dyn_arr);
    n = $high(queue_var);
    n = $increment(dyn_arr);
    n = $dimensions(dyn_arr);
    n = $unpacked_dimensions(queue_var);
    n = $size(fixed_arr, 1);
  end

endmodule
