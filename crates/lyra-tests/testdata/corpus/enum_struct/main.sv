// ALLOW-EXTRA-DIAGS
module m;
  typedef enum { A, B, C } abc_t;
  abc_t x;
  typedef struct packed { logic [7:0] data; } pkt_t;
  pkt_t y;
  enum { P, Q } pq;
  struct { int a; int b; } point;
endmodule
