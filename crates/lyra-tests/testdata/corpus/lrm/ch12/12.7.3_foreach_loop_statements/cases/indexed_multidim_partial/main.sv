// LRM 12.7.3 -- Foreach with indexed multidimensional partial select
//
// The array reference `m[0]` selects a 2D sub-array from a 3D array.
// The var list `[i,j]` iterates the two remaining dimensions.
// No diagnostics expected.

module indexed_multidim_partial;

  int m [2][3][4];

  initial begin
    foreach (m[0][i,j]) begin
      m[0][i][j] = i * 4 + j;
    end
  end

endmodule
