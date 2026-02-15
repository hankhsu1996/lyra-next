module mux(input logic sel, input logic a, input logic b, output logic y);
  always_comb begin
    if (sel)
      y = a;
    else
      y = b;
  end
endmodule
