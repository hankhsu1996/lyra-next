// LRM 6.19: Enumerations -- member name resolution via typedef
//
// Enum member names are injected into the enclosing scope as
// value-namespace constants with the parent enum's type.

module enum_member_resolve;

  typedef enum { IDLE, RUNNING, DONE } state_t;

  state_t current;
  state_t next_state;

  assign current = IDLE;
  assign next_state = RUNNING;

  // Enum member with explicit initializer
  typedef enum { OFF = 0, ON = 1 } toggle_t;

  toggle_t sw;
  assign sw = ON;

endmodule
