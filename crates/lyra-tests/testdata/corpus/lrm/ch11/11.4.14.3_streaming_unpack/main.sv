// LRM 11.4.14.3: Streaming assignment target (unpack)
module streaming_unpack;
  logic [7:0] a, b;
  logic [15:0] src16;
  logic [63:0] src64;
  logic [127:0] src128;
  int arr [4];
  logic dyn_arr [];

  initial begin
    // Positive: MSB-first unpack, widths match
    {>> {a, b}} = src16;

    // Positive: LSB-first unpack, widths match
    {<< {a, b}} = src16;

    // Positive: fixed-size unpacked array of integral
    {>> {arr}} = src128;

    // Width mismatch: LHS 8 bits, RHS 64 bits
    {>> {a}} = src64;
    //       ^ error[lyra.type[28]]: streaming unpack target is 8 bits but source is 64 bits

    // Invalid operand: literal is not an lvalue
    {>> {8'd0}} = src16;
    //   ^ error[lyra.type[25]]: streaming unpack operand is not a valid assignment target

    // Unsupported operand: dynamic array has no fixed width
    {>> {dyn_arr}} = src16;
    //   ^ error[lyra.type[26]]: streaming unpack operand type `logic []` has no fixed streaming width

    // with clause on unpack target
    {>> {arr with [0:3]}} = src16;
    //      ^ error[lyra.type[27]]: `with` clause on streaming unpack target is not yet supported
  end

  // Positive: continuous assign with streaming LHS
  assign {>> {a, b}} = src16;
endmodule
