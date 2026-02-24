// LRM 6.19.3: Enum name range error cases

module enum_range_errors;

  // Negative count
  typedef enum { N[-1] } neg_t;
  //             ^ error[lyra.semantic[9]]

  // Collision: range-generated name collides with plain member
  typedef enum { P[3], P1 } collision_t;
  //             ^ error[lyra.semantic[2]]

endmodule
