// Interface defined in a separate file

interface shared_bus;
  logic [7:0] data;
  logic valid, ready;
  modport producer(output data, valid, input ready);
  modport consumer(input data, valid, output ready);
endinterface
