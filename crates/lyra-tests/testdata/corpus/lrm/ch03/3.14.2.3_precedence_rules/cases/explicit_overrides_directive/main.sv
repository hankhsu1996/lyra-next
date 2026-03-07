// LRM 3.14.2.3: explicit timeunit overrides `timescale directive
`timescale 1ns / 1ps
module m;
  timeunit 10us;
endmodule
