// LRM 7.8.2 -- String index
//
// Associative arrays with string index accept string or string literal keys.
// Empty string "" is a valid index. Tests declaration, indexing, methods,
// and foreach iteration (string-indexed arrays yield string loop variables).

module string_index;

  // Declaration
  int aa [string];
  logic [15:0] bb [string];

  // Built-in methods: size, num
  int sz;
  assign sz = aa.size();

  int n;
  assign n = bb.num();

  // Procedural context: string literal indexing
  initial begin
    aa["hello"] = 1;
    aa["world"] = 2;
    aa[""] = 3;  // empty string is a valid index

    bb["key"] = 16'hCAFE;

    // delete with string argument, delete all
    aa.delete("hello");
    aa.delete();

    // exists with string argument
    if (aa.exists("world"))
      aa["world"] = 0;
  end

  // Foreach iteration: loop variable is string type
  initial begin
    foreach (aa[key]) begin
      bb[key] = 16'h0;
    end
  end

endmodule
