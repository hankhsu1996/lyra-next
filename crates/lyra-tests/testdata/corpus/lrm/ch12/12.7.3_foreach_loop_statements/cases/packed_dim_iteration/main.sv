// LRM 12.7.3 -- foreach over packed dimensions
//
// A packed vector has one iterable packed dimension.
// No diagnostics expected.

module packed_dim_iteration;

  logic [7:0] v;

  initial begin
    foreach (v[i]) begin
    end
  end

endmodule
