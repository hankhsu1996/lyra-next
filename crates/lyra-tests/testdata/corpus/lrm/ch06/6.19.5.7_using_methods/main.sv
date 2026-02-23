// LRM 6.19.5.7: Using enumerated type methods
//
// Enum methods can be called on any expression of the enum type.
// This section demonstrates combined usage patterns.

module using_enum_methods;

  typedef enum { MON, TUE, WED, THU, FRI } weekday_t;
  weekday_t day;

  // All methods on a single enum variable
  weekday_t first_day = day.first();
  weekday_t last_day  = day.last();
  weekday_t next_day  = day.next();
  weekday_t prev_day  = day.prev();
  int day_count       = day.num();
  string day_name     = day.name();

  // Methods on an enum with explicit base type
  typedef enum logic [2:0] { S0, S1, S2, S3 } state_t;
  state_t st;
  state_t sf = st.first();
  state_t sl = st.last();
  int sn     = st.num();

endmodule
