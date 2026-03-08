// LRM 5.6.1: Escaped keywords used as user identifiers.
// \begin, \module etc. are valid user identifiers, not keywords.

module escaped_keyword_ident;
  int \begin ;
  int \module ;
  int \wire ;

  initial begin
    \begin = 1;
    \module = 2;
    \wire = 3;
  end
endmodule
