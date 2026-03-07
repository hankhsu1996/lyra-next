// LRM 5.6.4: Compiler directives
//
// Tests `default_nettype directive parsing and policy summary.
// Implicit net creation behavior (6.10) is not tested here.

`default_nettype wire

module default_nettype_basic;
  logic a;
endmodule

`default_nettype none

module default_nettype_none;
  logic b;
endmodule

`resetall

module after_resetall;
  logic c;
endmodule
