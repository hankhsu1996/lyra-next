// LRM 12.7.3 -- Foreach over associative array with string index
//
// Loop variable `key` takes the associative index type (string).
// No diagnostics expected.

module associative_string_index;

  int aa [string];

  initial begin
    foreach (aa[key]) begin
      aa[key] = 1;
    end
  end

endmodule
