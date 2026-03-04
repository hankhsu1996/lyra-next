// LRM 7.3.2: Tagged unions
//
// Tests tagged union declaration, void members, and restriction that
// void members are only allowed inside tagged unions.

module tagged_unions;

  // Valid: tagged union with void and typed members
  typedef union tagged {
    void Invalid;
    int Valid;
  } maybe_int_t;

  maybe_int_t mi;

  // Valid: tagged union with 3+ members (exercises tag bits)
  typedef union tagged {
    void None;
    int ByteVal;
    int HalfVal;
  } payload_t;

  payload_t p;

  // Invalid: void member in a struct
  typedef struct packed {
    void bad_field;
    //  ^ error[lyra.semantic.void_member_non_tagged]
    logic [7:0] data;
  } bad_struct_t;

  // Invalid: void member in an untagged union
  typedef union packed {
    void bad_field;
    //  ^ error[lyra.semantic.void_member_non_tagged]
    logic [7:0] data;
  } bad_union_t;

endmodule
