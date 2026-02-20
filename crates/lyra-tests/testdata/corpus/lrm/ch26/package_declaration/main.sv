// LRM 26.2: Package declarations
//
// Packages provide a mechanism for sharing parameters, data, type, task,
// and function declarations among modules, interfaces, programs, and checkers.

// Basic package with typedef and functions (LRM 26.2 example)
package ComplexPkg;
  typedef struct packed {
    shortreal i, r;
  } Complex;

  function automatic Complex add(Complex a, b);
    add.r = a.r + b.r;
    add.i = a.i + b.i;
  endfunction

  function automatic Complex mul(Complex a, b);
    mul.r = (a.r * b.r) - (a.i * b.i);
    mul.i = (a.r * b.i) + (a.i * b.r);
  endfunction
endpackage : ComplexPkg

// Package with parameters and variables
package ParamPkg;
  parameter int WIDTH = 8;
  localparam int DEPTH = 16;

  typedef logic [WIDTH-1:0] data_t;
endpackage

// Package with enum declaration
package EnumPkg;
  typedef enum logic [1:0] {
    IDLE  = 2'b00,
    RUN   = 2'b01,
    STOP  = 2'b10,
    ERR   = 2'b11
  } state_t;
endpackage

// Empty package (legal per LRM)
package EmptyPkg;
endpackage
