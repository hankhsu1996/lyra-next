// LRM 7.2.1 / 7.3.1 -- Non-integral members in packed records
//
// Packed structs and packed unions shall contain only integral data types.

module packed_record_non_integral_member;

  // -- Valid: integral members in packed records --

  typedef struct packed {
    logic [7:0] a;
    bit [3:0]   b;
  } packed_struct_ok_t;

  typedef union packed {
    logic [7:0] x;
    logic [7:0] y;
  } packed_union_ok_t;

  // Enum member in packed union (integral)
  typedef enum logic [7:0] { RED, GREEN, BLUE } color_t;

  typedef union packed {
    color_t     c;
    logic [7:0] raw;
  } packed_union_enum_t;

  // Packed struct containing packed struct member (integral)
  typedef union packed {
    packed_struct_ok_t s;
    logic [11:0]       flat;
  } packed_union_nested_t;

  // Soft packed union with integral members
  typedef union soft packed {
    logic [7:0]  lo;
    logic [31:0] hi;
  } soft_packed_ok_t;

  // -- Invalid: non-integral members in packed records --

  typedef union packed {
    real r;
// @r error[lyra.semantic.non_integral_packed_member]: union member `r` is not an integral type (real) in packed union
    logic [63:0] bits;
  } packed_union_real_t;

  typedef union packed {
    string s;
// @s error[lyra.semantic.non_integral_packed_member]: union member `s` is not an integral type (string) in packed union
    logic [7:0] bits;
  } packed_union_string_t;

  typedef struct packed {
    real r;
// @r error[lyra.semantic.non_integral_packed_member]: struct member `r` is not an integral type (real) in packed struct
    logic [7:0] b;
  } packed_struct_real_t;

  typedef struct {
    int x;
    int y;
  } unpacked_struct_t;

  typedef union packed {
    unpacked_struct_t us;
// @us error[lyra.semantic.non_integral_packed_member]: union member `us` is not an integral type (unpacked struct) in packed union
    logic [63:0] bits;
  } packed_union_unpacked_struct_t;

  typedef struct packed {
    logic [7:0] arr [4];
// @arr error[lyra.semantic.non_integral_packed_member]: struct member `arr` is not an integral type (unpacked array) in packed struct
  } packed_struct_array_t;

  typedef union soft packed {
    real r;
// @r error[lyra.semantic.non_integral_packed_member]: union member `r` is not an integral type (real) in soft packed union
    logic [63:0] bits;
  } soft_packed_real_t;

endmodule
