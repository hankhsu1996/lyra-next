// LRM 6.19.5.5: num() method
//
// Returns the number of elements in the enumeration as int.

module enum_num;

  typedef enum { A, B, C } three_t;
  three_t x;
  int count3 = x.num();

  typedef enum { SOLO } one_t;
  one_t y;
  int count1 = y.num();

endmodule
