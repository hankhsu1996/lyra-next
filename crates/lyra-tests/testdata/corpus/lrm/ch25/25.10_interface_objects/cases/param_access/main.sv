// LRM 25.10: Parameters remain accessible through modport-qualified ports

interface iface_with_params;
  parameter int True = 1;
  localparam int False = 0;
  logic [7:0] data;
  modport reader(input data);
endinterface

module consumer(iface_with_params.reader i);
  int val;
  always_comb val = i.True;
  int other;
  always_comb other = i.False;
endmodule
