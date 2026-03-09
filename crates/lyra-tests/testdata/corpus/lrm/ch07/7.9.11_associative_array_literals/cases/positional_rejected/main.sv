// LRM 7.9.11 -- Positional entries rejected in associative array pattern

module positional_rejected;

  int aa [string];

  initial begin
    aa = '{42};
    // @[42] error[lyra.type.assign_pattern_positional_in_assoc]: positional entry is not allowed in associative array assignment pattern
  end

endmodule
