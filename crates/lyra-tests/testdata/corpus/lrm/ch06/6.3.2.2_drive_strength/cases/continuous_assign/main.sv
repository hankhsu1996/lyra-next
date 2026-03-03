// LRM 6.3.2.2: Drive strength on continuous assign
//
// Continuous assigns accept an optional drive strength after the assign keyword.

module drive_strength_assign;

  wire c, d, e;

  assign (strong0, strong1) c = d & e;
  assign (pull0, pull1) d = e;

endmodule
