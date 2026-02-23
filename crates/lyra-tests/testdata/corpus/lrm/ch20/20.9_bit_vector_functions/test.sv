module system_functions;
  logic [7:0] vec;
  int i;
  logic signed [7:0] s;
  assign s = $signed(vec);
  int sz;
  assign sz = $bits(vec);
  int cnt;
  assign cnt = $countones(vec);
  bit hot;
  assign hot = $onehot(vec);
endmodule
