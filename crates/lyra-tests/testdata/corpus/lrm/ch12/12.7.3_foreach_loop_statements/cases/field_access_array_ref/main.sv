// LRM 12.7.3 -- Foreach with field access array reference
//
// The struct member `s.arr` is an array. The var list `[i]` iterates
// over its dimension. No diagnostics expected.

module field_access_array_ref;

  typedef struct { int arr [3]; } foo_t;
  foo_t s;

  initial begin
    foreach (s.arr[i]) begin
      s.arr[i] = i;
    end
  end

endmodule
