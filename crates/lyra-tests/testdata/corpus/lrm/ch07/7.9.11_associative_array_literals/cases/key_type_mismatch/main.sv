// LRM 7.9.11 -- Key type mismatch in associative array pattern

module key_type_mismatch;

  int aa [string];

  initial begin
    aa = '{42: 1};
    // @[42] error[lyra.type.assign_pattern_key_type_mismatch]: assignment pattern key type `bit signed [31:0]` does not match index type `string`
  end

endmodule
