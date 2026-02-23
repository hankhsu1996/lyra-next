// LRM 5.7.1: Integer literal constants

module integer_literals_test;

  // Unsized decimal integers
  int a = 659;
  int b = 27_195_000;

  // Sized based literals (binary, octal, decimal, hex)
  logic [3:0] bin = 4'b1001;
  logic [4:0] dec = 5'd3;
  logic [7:0] oct = 8'o77;
  logic [11:0] hex = 12'hABC;

  // Sized based with x and z
  logic [11:0] xval = 12'hx;
  logic [15:0] zval = 16'hz;
  logic [2:0] xbin = 3'b01x;

  // Signed based literals
  logic [3:0] shex = 4'shf;

  // Unsized based literals
  int uhex = 'h837FF;
  int uoct = 'o7460;

  // Unbased unsized literals (used in same-width context to avoid warnings)
  logic all0 = '0;
  logic all1 = '1;
  logic allx = 'x;
  logic allz = 'z;

  // Unbased unsized with wider targets (context-determined width, LRM 5.7.1)
  logic [7:0]  wide_all0 = '0;
  logic [7:0]  wide_all1 = '1;
  logic [15:0] wide_allx = 'x;
  logic [31:0] wide_allz = 'z;

  // Underscore separators
  logic [15:0] uscore = 16'b0011_0101_0001_1111;
  int hex_uscore = 32'h12ab_f001;

  // ? as z alternative (LRM 5.7.1)
  logic [3:0] qbin = 4'b10?1;
  logic [7:0] qhex = 8'h?F;

  // Negative numbers (LRM 5.7.1 -- unary minus before sized literal)
  int neg = -8'd6;
  int neg2 = -4'sd1;

  // Spaced sized literals (LRM 5.7.1 -- trivia between size and base)
  logic [7:0]  spaced_hex = 8 'hFF;
  logic [3:0]  spaced_bin = 4 'b1010;

endmodule
