module top;
  adder #(.WIDTH(16)) u_add (
    .a(in_a),
    .b(in_b),
    .sum(result)
  );
endmodule
