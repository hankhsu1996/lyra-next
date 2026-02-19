// LRM 5.6-5.9: Identifiers, numbers, and string literals

// 5.6.1 Escaped identifiers
module escaped_identifiers;
  int \busa+index ;
  int \-clock ;
  int \***error-condition*** ;
  int \net1/\net2 ;
  int \{a,b} ;
  int \a*(b+c) ;
endmodule

// 5.7.1 Integer literal constants
module integer_literals;
  // Unsized decimal
  int a = 659;
  // Based literals (unsized -- width inherited from context)
  int b = 'h837FF;
  int c = 'o7460;
  // Sized literals (all 32-bit to match int)
  int d = 32'b1001;
  int e = 32'd3;
  int f = 32'b01x;
  int g = 32'hx;
  int h = 32'hz;
  // Signed based
  int i = 32'shf;
  // Underscores for readability
  int j = 27_195_000;
  int k = 32'b0000_0000_0000_0000_0011_0101_0001_1111;
  int l = 32'h12ab_f001;
  // Unbased unsized literals
  bit m = '0;
  bit n = '1;
endmodule

// 5.7.2 Real literal constants
module real_literals;
  real r1 = 1.2;
  real r2 = 0.1;
  real r3 = 2394.26331;
  real r4 = 1.2E12;
  real r5 = 1.30e-2;
  real r6 = 23E10;
endmodule

// 5.9 String literals
module string_literals;
  string s1 = "hello world";
  string s2 = "line1\nline2";
  string s3 = "tab\there";
  string s4 = "quote\"inside";
  string s5 = "backslash\\end";
endmodule
