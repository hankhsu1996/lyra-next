// LRM 6.19.4 -- Enum methods
module enum_methods;

  typedef enum { RED, GREEN, BLUE } color_t;
  color_t c;

  // .first() and .last() return the enum type
  color_t f = c.first();
  color_t l = c.last();

  // .next() and .prev() return the enum type
  color_t n0 = c.next();
  color_t n2 = c.next(2);
  color_t p0 = c.prev();
  color_t p1 = c.prev(1);

  // .num() returns int
  int count = c.num();

  // .name() returns string
  string s = c.name();

endmodule
