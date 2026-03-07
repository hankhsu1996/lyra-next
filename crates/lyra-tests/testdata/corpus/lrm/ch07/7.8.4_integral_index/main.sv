// LRM 7.8.4 -- Integral index
//
// Associative arrays with integral keyword index types. The index expression
// is cast to the index type. Ordering is signed or unsigned numerical
// depending on the signedness of the index type.

module integral_index;

  // Various integral keyword index types
  logic [7:0] aa_int [int];
  int bb_integer [integer];
  int cc_byte [byte];
  int dd_shortint [shortint];
  int ee_longint [longint];
  int ff_bit [bit];

  // Typedef name as associative index type
  typedef shortint SNibble;
  int gg_typedef [SNibble];

  // Typedef chain: typedef of typedef
  typedef SNibble MyKey;
  int hh_chain [MyKey];

  // Built-in methods
  int sz;
  assign sz = aa_int.size();

  int n;
  assign n = bb_integer.num();

  // Procedural context: indexing and methods
  initial begin
    aa_int[42] = 8'hFF;
    bb_integer[-1] = 100;
    cc_byte[8'h0A] = 3;
    dd_shortint[16'h1234] = 4;
    ee_longint[0] = 5;

    // Typedef-keyed: indexing, methods, foreach
    gg_typedef[42] = 10;
    hh_chain[-1] = 20;
    gg_typedef.delete(42);
    hh_chain.delete();

    // delete with typed key argument
    aa_int.delete(42);
    aa_int.delete();

    // exists with typed key argument
    if (bb_integer.exists(-1))
      bb_integer[-1] = 0;
  end

  // Foreach iteration: loop variable matches the index type
  initial begin
    foreach (aa_int[idx]) begin
      bb_integer[idx] = 0;
    end
    foreach (gg_typedef[k]) begin
      hh_chain[k] = 0;
    end
  end

  // Error: real expression as index on integral-keyed array
  real r_val;
  initial begin
    aa_int[r_val] = 0;
    // @r_val error[lyra.type.assoc_index_key_mismatch]: associative array index type `real` does not match declared key type `int`
    gg_typedef[r_val] = 0;
    // @r_val error[lyra.type.assoc_index_key_mismatch]: associative array index type `real` does not match declared key type `shortint`
  end

  // Error: string expression as index on integral-keyed array
  string s_val;
  initial begin
    bb_integer[s_val] = 0;
    // @s_val error[lyra.type.assoc_index_key_mismatch]: associative array index type `string` does not match declared key type `integer`
  end

endmodule
