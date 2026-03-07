// LRM 3.14.2.1: precision must not exceed time unit
`timescale 1ps / 1ns
// @[`timescale] error[lyra.preprocess.timescale_precision_exceeds_unit]: `timescale precision (1ns) must not exceed time unit (1ps)
module m;
endmodule
