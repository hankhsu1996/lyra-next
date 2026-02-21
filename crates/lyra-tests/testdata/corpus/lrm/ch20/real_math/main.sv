// LRM 20.8.2: Real math system functions
//
// All take real arguments and return real.

module real_math;
  real x, y, r;

  // 1-arg functions
  assign r = $ln(x);
  assign r = $log10(x);
  assign r = $exp(x);
  assign r = $sqrt(x);
  assign r = $floor(x);
  assign r = $ceil(x);
  assign r = $sin(x);
  assign r = $cos(x);
  assign r = $tan(x);
  assign r = $asin(x);
  assign r = $acos(x);
  assign r = $atan(x);
  assign r = $sinh(x);
  assign r = $cosh(x);
  assign r = $tanh(x);
  assign r = $asinh(x);
  assign r = $acosh(x);
  assign r = $atanh(x);

  // 2-arg functions
  assign r = $pow(x, y);
  assign r = $atan2(x, y);
  assign r = $hypot(x, y);
endmodule
