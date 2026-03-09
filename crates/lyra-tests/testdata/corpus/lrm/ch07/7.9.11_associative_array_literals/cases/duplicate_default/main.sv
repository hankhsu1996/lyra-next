// LRM 7.9.11 -- Duplicate default entry rejected

module duplicate_default;

  int aa [int];

  initial begin
    aa = '{default: 1, default: 2};
    // @2:default error[lyra.type.assign_pattern_duplicate_default]: duplicate `default` entry in assignment pattern
  end

endmodule
