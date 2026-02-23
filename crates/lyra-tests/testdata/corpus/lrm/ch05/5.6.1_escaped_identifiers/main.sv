// LRM 5.6.1: Escaped identifiers

module escaped_identifiers;
  int \busa+index ;
  int \-clock ;
  int \***error-condition*** ;
  int \net1/\net2 ;
  int \{a,b} ;
  int \a*(b+c) ;
  // Escaped keyword treated as user identifier (LRM 5.6.1)
  int \module ;
  int \endmodule ;
  int \wire ;
endmodule
