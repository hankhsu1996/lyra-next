// LRM 12.7.3 -- Foreach with skipped dimension slots
//
// The middle dimension is skipped (empty slot). `i` binds to dim 0,
// `k` binds to dim 2. No diagnostics expected.

module skip_slot_preserved;

  int cube [2][3][4];

  initial begin
    foreach (cube[i,,k]) begin
      cube[i][0][k] = i + k;
    end
  end

endmodule
