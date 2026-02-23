// LRM 6.19.1: Defining new data types as enumerated types
//
// typedef enum creates a named enumerated type that can be
// used as a type for variable declarations. Without typedef,
// the enum is anonymous and members are injected into scope
// but no reusable type name exists.

module enum_type_definition;

  // Named type via typedef
  typedef enum { IDLE, RUNNING, DONE } state_t;
  state_t s1;
  state_t s2;
  assign s1 = IDLE;
  assign s2 = DONE;

  // Named type with explicit base type
  typedef enum logic [1:0] { OFF, LOW, MED, HIGH } level_t;
  level_t lev;
  assign lev = HIGH;

  // Named type with explicit member values
  typedef enum int { ERR = -1, OK = 0, WARN = 1 } status_t;
  status_t st;
  assign st = OK;

  // Anonymous enum -- members injected into scope, no named type
  enum { X, Y, Z } anon;
  assign anon = Y;

endmodule
