module based_lit_test;
  // Compact forms
  parameter A = 8'hFF;
  parameter B = 4'b1010;

  // Spaced: size separated from prefix
  parameter C = 8 'hFF;

  // Spaced: prefix separated from digits
  parameter D = 'h FF;

  // Fully spaced: size, prefix, and digits all separated
  parameter E = 32 'h 12ab_f001;

  // Block comment between prefix and digits
  parameter F = 'h/*c*/FF;

  // Block comment between size, prefix, and digits
  parameter G = 8 'h/*c*/FF;

  // Line comment between prefix and digits
  parameter H = 8 'h // c
    FF;

  // Malformed: prefix without digits
  parameter I = 'h ;
endmodule
