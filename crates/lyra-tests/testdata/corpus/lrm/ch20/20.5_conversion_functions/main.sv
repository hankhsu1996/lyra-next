// LRM 20.5: Real conversion functions

module real_conversion;
  int i;
  real r;
  shortreal sr;
  bit [63:0] bits64;
  bit [31:0] bits32;
  bit [15:0] bits16;

  // Valid conversions
  assign r = $itor(i);
  assign i = $rtoi(r);
  assign bits64 = $realtobits(r);
  assign r = $bitstoreal(bits64);
  assign bits32 = $shortrealtobits(sr);
  assign sr = $bitstoshortreal(bits32);

  // $shortrealtobits accepts real (implicit narrowing)
  assign bits32 = $shortrealtobits(r);

  // Wrong width for $bitstoreal (expects 64-bit, got 16-bit)
  assign r = $bitstoreal(bits16);
  //                     ^ error[lyra.type[8]]: $bitstoreal requires 64-bit argument, got 16-bit

  // Wrong width for $bitstoshortreal (expects 32-bit, got 64-bit)
  assign sr = $bitstoshortreal(bits64);
  //                           ^ error[lyra.type[8]]: $bitstoshortreal requires 32-bit argument, got 64-bit

  // Category mismatch: $bitstoreal with real arg
  assign r = $bitstoreal(r);
  //                     ^ error[lyra.type[8]]: $bitstoreal requires integral argument

  // Category mismatch: $realtobits with integral arg
  assign bits64 = $realtobits(bits64);
  //                          ^ error[lyra.type[8]]: $realtobits requires real argument
endmodule
