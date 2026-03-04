// LRM 25.7.2: Bare-name import of tasks/functions in modports.
// `import Read` makes the interface task `Read` accessible through
// a modport-qualified port.

interface simple_bus;
  logic [7:0] data;

  task Read(input logic [7:0] addr);
  endtask

  function void Write(input logic [7:0] addr, input logic [7:0] wdata);
  endfunction

  modport master(
    input data,
    import Read, Write
  );
endinterface

module consumer(simple_bus.master bus);
  initial begin
    bus.Read(8'hAA);
    bus.Write(8'h10, 8'hFF);
  end
endmodule
