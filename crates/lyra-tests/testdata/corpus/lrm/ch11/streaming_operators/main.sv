// LRM 11.4.14: Streaming operators
module streaming_ops;
  logic [31:0] a, b;
  logic [63:0] packed_val;

  initial begin
    // Left-to-right streaming, no slice_size
    packed_val = {>> {a, b}};

    // Right-to-left streaming with type slice_size
    packed_val = {<< int {a, b}};

    // Numeric slice_size
    packed_val = {<< 16 {a, b}};

    // Packed type with dimensions as slice_size
    packed_val = {<< logic [3:0] {a, b}};
  end
endmodule
