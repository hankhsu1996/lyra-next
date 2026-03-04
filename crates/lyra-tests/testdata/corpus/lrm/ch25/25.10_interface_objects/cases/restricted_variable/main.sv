// LRM 25.10: Non-parameter variables not listed in modport are restricted

interface iface_restricted;
  logic [7:0] data;
  logic valid;
  logic secret;
  modport reader(input data, valid);
endinterface

module consumer(iface_restricted.reader bus);
  logic leaked;
  always_comb leaked = bus.secret;
  //                       ^ error[lyra.type[38]]: member `secret` is not accessible through this modport
endmodule
