// LRM 5.8: Time literals
//
// time_literal ::= unsigned_number time_unit
//                | fixed_point_number time_unit
// time_unit ::= s | ms | us | ns | ps | fs

module time_literals;
  // Integer form -- all 6 units
  realtime t1 = 2ns;
  realtime t2 = 40ps;
  realtime t3 = 1us;
  realtime t4 = 100ms;
  realtime t5 = 3s;
  realtime t6 = 10fs;

  // Fixed-point form
  realtime t7 = 2.1ns;

  // Exponent form
  realtime t8 = 1.5e2ns;

  // Underscore separators
  realtime t9 = 100_000fs;

  // Zero
  realtime t10 = 0ns;
endmodule
