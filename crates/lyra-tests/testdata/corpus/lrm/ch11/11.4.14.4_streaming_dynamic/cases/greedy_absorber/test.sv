// LRM 11.4.14.4: Greedy absorber for dynamic arrays and queues
module greedy_absorber;
  logic [127:0] src128;
  logic [63:0] src64;
  logic [31:0] src32;
  logic [7:0] src8;
  logic [31:0] a;
  logic [7:0] b;
  int dyn_arr [];
  int q [$];
  logic dyn_logic [];

  initial begin
    // Positive: single dynamic array absorbs all bits (128 / 32 = 4 elems)
    {>> {dyn_arr}} = src128;

    // Positive: fixed + greedy (32 fixed + 96 remaining, 96 / 32 = 3 elems)
    {>> {a, dyn_arr}} = src128;

    // Positive: queue absorbs all bits (64 / 32 = 2 elems)
    {>> {q}} = src64;

    // Positive: two dynamics, second gets 0 elements (no error)
    {>> {dyn_arr, q}} = src128;

    // Positive: 1-bit element dynamic array (128 / 1 = 128 elems)
    {>> {dyn_logic}} = src128;

    // Positive: fixed + 1-bit element greedy (8 fixed, 120 / 1 = 120 elems)
    {>> {b, dyn_logic}} = src128;

    // Negative: remainder not element-aligned
    // 64 - 8 = 56 remaining, 56 % 32 != 0
    {>> {b, dyn_arr}} = src64;
    //     ^ error[lyra.type[27]]: streaming unpack leaves 56 bits for dynamic target of 32-bit elements

    // Negative: queue remainder not element-aligned
    // 64 - 8 = 56 remaining, 56 % 32 != 0
    {>> {b, q}} = src64;
    //     ^ error[lyra.type[27]]: streaming unpack leaves 56 bits for dynamic target of 32-bit elements

    // Negative: source smaller than fixed portion
    // fixed = 32 (a), source = 8 bits
    {>> {a, dyn_arr}} = src8;
    //                ^ error[lyra.type[28]]: streaming unpack target is 32 bits but source is 8 bits
  end
endmodule
