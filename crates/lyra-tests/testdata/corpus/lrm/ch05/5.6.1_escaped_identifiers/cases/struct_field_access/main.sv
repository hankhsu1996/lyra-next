// LRM 5.6.1: Escaped identifiers in struct field declarations and access.
// Escaped field declared, plain access; plain field declared, escaped access.

module struct_field_access;
  typedef struct packed {
    logic [7:0] \data ;
    logic       valid;
  } pkt_t;

  pkt_t p;

  initial begin
    p.data = 8'hFF;
    p.\valid = 1'b1;
  end
endmodule
