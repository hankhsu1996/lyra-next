// LRM 7.9.11 -- Valid associative array literals with keyed entries

module keyed_entries;

  int aa_int [int];
  int aa_str [string];

  initial begin
    aa_int = '{0: 10, 1: 20, 2: 30};
    aa_str = '{"foo": 1, "bar": 2};
  end

endmodule
