// LRM 11.4.14.3: Streaming assignment target (unpack)
module streaming_unpack;
  logic [7:0] a, b;
  logic [15:0] src16;
  logic [31:0] a32;
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

    // Positive: with clause, fixed range (4 ints x 32b = 128 bits)
    {>> {arr with [0:3]}} = src128;

    // Positive: with clause, single element (1 int = 32 bits)
    {>> {arr with [0]}} = a32;

    // Positive: with clause, indexed-plus (2 ints x 32b = 64 bits)
    {>> {arr with [0 +: 2]}} = src64;

    // Positive: with clause, indexed-minus (2 ints x 32b = 64 bits)
    {>> {arr with [3 -: 2]}} = src64;

    // Width mismatch: LHS 8 bits, RHS 64 bits
    {>> {a}} = src64;
    //       ^ error[lyra.type[28]]: streaming unpack target is 8 bits but source is 64 bits

    // Width mismatch via with: select 2 elements (64 bits) but assign 128-bit source
    {>> {arr with [0:1]}} = src128;
    //                    ^ error[lyra.type[28]]: streaming unpack target is 64 bits but source is 128 bits

    // Invalid operand: literal is not an lvalue
    {>> {8'd0}} = src16;
    //   ^ error[lyra.type[25]]: streaming unpack operand is not a valid assignment target

    // Unsupported operand: dynamic array has no fixed width
    {>> {dyn_arr}} = src16;
    //   ^ error[lyra.type[26]]: streaming unpack operand type `logic []` has no fixed streaming width

    // with clause on non-array scalar LHS
    {>> {a with [0]}} = src16;
    //    ^ error[lyra.type[13]]: `with` clause requires an array operand
  end

  // Positive: continuous assign with streaming LHS
  assign {>> {a, b}} = src16;
endmodule
