// LRM 7.3.2: Tagged unions
//
// Tests tagged union declaration, void members, tagged expression
// constructors (LRM 11.9), and constructor error detection.

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

  // Tagged expression constructors (LRM 11.9): valid cases
  int arr[3];
  typedef struct packed {
    int val;
  } inner_t;
  inner_t obj;

  initial begin
    mi = tagged Valid 42;
    mi = tagged Valid (23 + 34);
    mi = tagged Invalid;
    p = tagged None;
    p = tagged ByteVal 8;

    // Postfix primary operands (field access, indexing)
    mi = tagged Valid obj.val;
    mi = tagged Valid arr[0];
  end

  // Tagged expression constructors: negative cases
  typedef struct packed {
    logic [7:0] data;
  } my_struct_t;

  my_struct_t s;

  initial begin
    // Unknown member name
    mi = tagged Foo 1;
    // @tagged error[lyra.type.tagged_expr_error]

    // Void member given an operand
    mi = tagged Invalid 1;
    // @tagged error[lyra.type.tagged_expr_error]

    // Payload member without operand
    mi = tagged Valid;
    // @tagged error[lyra.type.tagged_expr_error]

    // Non-tagged-union expected type (struct)
    s = tagged Valid 42;
    // @tagged error[lyra.type.tagged_expr_error]
  end

  // Tagged expression in call-argument context (LRM 11.9)
  function void accept_mi(maybe_int_t x);
  endfunction

  function void accept_payload(payload_t x);
  endfunction

  initial begin
    // Valid: tagged expression as call argument
    accept_mi(tagged Valid 42);
    accept_mi(tagged Invalid);
    accept_payload(tagged None);
    accept_payload(tagged ByteVal 8);
  end

  // Call-argument negative cases
  initial begin
    // Unknown member in call argument
    accept_mi(tagged Foo 1);
    // @tagged error[lyra.type.tagged_expr_error]

    // Void member with operand in call argument
    accept_mi(tagged Invalid 1);
    // @tagged error[lyra.type.tagged_expr_error]

    // Payload member without operand in call argument
    accept_mi(tagged Valid);
    // @tagged error[lyra.type.tagged_expr_error]
  end

  // Invalid: void member in a struct
  typedef struct packed {
    void bad_field;
    // @bad_field error[lyra.semantic.void_member_non_tagged]
    logic [7:0] other;
  } bad_struct_t;

  // Invalid: void member in an untagged union
  typedef union packed {
    void bad_field;
    // @bad_field error[lyra.semantic.void_member_non_tagged]
    logic [7:0] data;
  } bad_union_t;

endmodule
