// LRM 7.2.1 -- Packed structures
//
// Tests packed struct declaration, assignment, and field access.

module packed_struct_assign;

  typedef struct packed {
    logic [3:0] upper;
    logic [3:0] lower;
  } nibble_pair_t;

  nibble_pair_t np;
  logic [7:0] flat;

  // Packed struct can be assigned from integral of matching width
  assign np = 8'hAB;
  assign flat = np;

  // Field access on packed struct
  logic [3:0] u;
  assign u = np.upper;

  // Nested packed struct
  typedef struct packed {
    nibble_pair_t first;
    nibble_pair_t second;
  } pair_of_pairs_t;

  pair_of_pairs_t pp;
  logic [3:0] nested_field;
  assign nested_field = pp.first.upper;

endmodule
