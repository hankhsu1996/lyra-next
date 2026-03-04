// LRM 25.7.2: Prototype-form import of tasks/functions in modports.
// `import task Read(input logic [7:0] addr)` uses the full prototype form.
// Includes a non-void return type to exercise structural name extraction.

interface simple_bus;
  logic [7:0] data;

  task Read(input logic [7:0] addr);
  endtask

  function void Write(input logic [7:0] addr, input logic [7:0] wdata);
  endfunction

  function logic [7:0] Status(input logic [7:0] addr);
  endfunction

  modport slave(
    output data,
    import task Read(input logic [7:0] addr),
    import function void Write(input logic [7:0] addr, input logic [7:0] wdata),
    import function logic [7:0] Status(input logic [7:0] addr)
  );
endinterface

module consumer(simple_bus.slave bus);
  logic [7:0] s;
  initial begin
    bus.Read(8'hAA);
    bus.Write(8'h10, 8'hFF);
    s = bus.Status(8'h00);
  end
endmodule
