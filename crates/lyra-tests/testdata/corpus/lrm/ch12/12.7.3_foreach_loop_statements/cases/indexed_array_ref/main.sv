// LRM 12.7.3 -- Foreach with indexed array reference
//
// The array reference `a[0]` selects a sub-array. The var list `[j]`
// iterates over the remaining dimension. No diagnostics expected.

module indexed_array_ref;

  int a [3][4];

  initial begin
    foreach (a[0][j]) begin
      a[0][j] = j;
    end
  end

endmodule
