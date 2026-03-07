// LRM 3.14.2.1: multiple `timescale directives in one file
`timescale 1ns / 1ps
module m1; endmodule

`timescale 10us / 100ns
module m2; endmodule
