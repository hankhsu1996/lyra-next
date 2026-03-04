// LRM 11.4.14: slice_size must be a constant expression
module slice_size_const;
  parameter int P = 8;
  localparam int LP = 16;
  logic [63:0] data;
  logic [63:0] result;
  int x;

  initial begin
    // Passing: literal slice size
    result = {<< 8 {data}};

    // Passing: parameter slice size
    result = {<< P {data}};

    // Passing: localparam slice size
    result = {<< LP {data}};

    // Passing: type-form slice size (not checked for constness)
    result = {<< byte {data}};

    // Failing: variable expression as slice size
    result = {<< (x + 0) {data}};
    //          ^ error[lyra.type[40]]: streaming slice_size must be a constant expression
  end
endmodule
