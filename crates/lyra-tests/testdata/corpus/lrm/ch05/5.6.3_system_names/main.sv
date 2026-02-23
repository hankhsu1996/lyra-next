// LRM 5.6.3: System names ($-prefixed identifiers)

// System functions in expressions
module system_functions;
  localparam int W = $clog2(256);
  int arr [8];
  int sz = $size(arr);
endmodule

// System tasks in procedural context
module system_tasks;
  initial begin
    $display("hello");
    $finish;
  end
endmodule

// System functions with type arguments (LRM system_tf_call)
module system_tf_type_args;
  localparam int B1 = $bits(logic [7:0]);
  localparam int B2 = $bits(int);
endmodule
