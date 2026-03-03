// LRM 11.4.14.4: Streaming with array range
// ALLOW-EXTRA-DIAGS
module streaming_with;
  int arr [8];
  logic [31:0] packed_val;
  logic [7:0] x;

  initial begin
    // Fixed range: const bounds
    packed_val = {<< int {arr with [0:3]}};

    // Indexed ascending with const width
    packed_val = {<< int {arr with [0 +: 4]}};

    // Single element
    packed_val = {<< int {arr with [0]}};

    // Mixed operands: plain integral + array with range
    packed_val = {<< int {x, arr with [0 +: 4]}};

    // with on non-array scalar -- error
    packed_val = {<< int {x with [0]}};
    //                     ^ error[lyra.type[13]]: `with` clause requires an array operand
  end
endmodule
