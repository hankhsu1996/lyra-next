// LRM 7.2: Structures -- member access via dot notation
//
// Tests struct field access (x.field) for builtin and user-defined field types.

module struct_member_access;

  // Basic struct with int and logic fields
  typedef struct {
    int a;
    logic [7:0] b;
  } basic_t;

  basic_t s1;
  int result_a;
  logic [7:0] result_b;

  assign result_a = s1.a;
  assign result_b = s1.b;

  // Nested struct
  typedef struct {
    int x;
  } inner_t;

  typedef struct {
    inner_t sub;
  } outer_t;

  outer_t s2;
  int result_nested;
  assign result_nested = s2.sub.x;

  // Packed struct
  typedef struct packed {
    bit [7:0] high;
    bit [7:0] low;
  } packed_t;

  packed_t s3;
  bit [7:0] result_high;
  assign result_high = s3.high;

endmodule
