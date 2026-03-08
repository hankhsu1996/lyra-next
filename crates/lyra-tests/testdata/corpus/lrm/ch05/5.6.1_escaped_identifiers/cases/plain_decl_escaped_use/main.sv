// LRM 5.6.1: Plain identifier declared, escaped identifier used.
// cpu3 and \cpu3 denote the same identifier.

module plain_decl_escaped_use;
  int cpu3;
  int data_bus;

  initial begin
    \cpu3 = 42;
    \data_bus = 7;
  end
endmodule
