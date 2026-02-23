// LRM 7.3: Unions -- union containing struct, chained field access
//
// Tests chained member access through a union field that is a struct type.

module union_nested_member;

  typedef struct {
    int x;
    int y;
  } point_t;

  typedef union {
    point_t p;
    int raw;
  } data_t;

  data_t d;
  int result;
  assign result = d.p.x;

endmodule
