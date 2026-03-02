// LRM 12.7.3 -- Foreach over multidimensional fixed-size array
//
// Both `i` and `j` iterate over their respective dimensions.
// No diagnostics expected.

module multidim_array;

  int matrix [3][4];

  initial begin
    foreach (matrix[i,j]) begin
      matrix[i][j] = i * 4 + j;
    end
  end

endmodule
