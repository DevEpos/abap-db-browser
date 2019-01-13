INTERFACE zif_dbbr_c_operator
  PUBLIC .
  CONSTANTS equals TYPE voperator VALUE '=' ##no_text.
  CONSTANTS greater_than TYPE voperator VALUE '>' ##no_text.
  CONSTANTS greater_or_equal_to TYPE voperator VALUE '>=' ##no_text.
  CONSTANTS lesser_than TYPE voperator VALUE '<' ##no_text.
  CONSTANTS lesser_or_equal_to TYPE voperator VALUE '<=' ##no_text.
  CONSTANTS not_equals TYPE voperator VALUE '<>' ##no_text.
  CONSTANTS like TYPE voperator VALUE 'LIKE' ##no_text.
  CONSTANTS not_like TYPE voperator VALUE 'NOT LIKE' ##no_text.
  CONSTANTS between TYPE voperator VALUE 'BETWEEN' ##no_text.
ENDINTERFACE.
