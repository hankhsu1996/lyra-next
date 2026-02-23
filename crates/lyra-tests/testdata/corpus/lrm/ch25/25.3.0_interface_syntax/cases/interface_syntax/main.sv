// LRM 25.3: Basic interface declaration with signals and modports

interface simple_bus;
  logic req, gnt;
  logic [7:0] addr, data;
  modport master(input gnt, output req, addr, data);
  modport slave(output gnt, input req, addr, data);
endinterface
