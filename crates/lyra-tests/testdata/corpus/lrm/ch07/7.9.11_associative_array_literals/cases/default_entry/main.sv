// LRM 7.9.11 -- Valid associative array literal with default entry

module default_entry;

  int aa [string];

  initial begin
    aa = '{"a": 1, default: 0};
  end

endmodule
