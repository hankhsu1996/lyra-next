// LRM 5.10: Structure literals
//
// Structure literals use assignment pattern syntax '{...}.

module structure_literals_test;

  typedef struct {int a; int b;} ab;
  ab c;

  // Positional assignment pattern
  initial begin
    c = '{0, 1};
  end

  // Keyed assignment pattern
  initial begin
    c = '{a:0, b:1};
  end

  // Default assignment pattern
  initial begin
    c = '{default:0};
  end

  // Nested: array of structures
  ab arr[1:0];
  initial begin
    arr = '{'{1, 2}, '{3, 4}};
  end

  // Keyed pattern in case item (exercises collect_name_refs_from_expr path)
  initial begin
    case (c)
      '{a:0, b:1}: $display("match");
    endcase
  end

endmodule
