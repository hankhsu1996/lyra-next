// LRM 6.3.2.2: Illegal drive strength -- both high-impedance
//
// The combination (highz0, highz1) or (highz1, highz0) is illegal per the
// LRM grammar (both strengths cannot be high-impedance).

module illegal_both_highz;

  wire a, b;

  // Net declarations with both-highz drive strength
  wire (highz0, highz1) w;
  //  ^ error[lyra.type[35]]: drive strength (highz0, highz1) is not allowed
  wire (highz1, highz0) w2;
  //  ^ error[lyra.type[35]]: drive strength (highz0, highz1) is not allowed

  // Continuous assigns with both-highz drive strength
  assign (highz0, highz1) w = a;
  //    ^ error[lyra.type[35]]: drive strength (highz0, highz1) is not allowed
  assign (highz1, highz0) w2 = b;
  //    ^ error[lyra.type[35]]: drive strength (highz0, highz1) is not allowed

endmodule
