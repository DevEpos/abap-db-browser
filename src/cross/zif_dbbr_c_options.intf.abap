INTERFACE zif_dbbr_c_options
  PUBLIC .


  CONSTANTS including TYPE char1 VALUE 'I' ##NO_TEXT.
  CONSTANTS excluding TYPE char1 VALUE 'E' ##NO_TEXT.
  CONSTANTS between TYPE char2 VALUE 'BT' ##NO_TEXT.
  CONSTANTS not_between TYPE char2 VALUE 'NB' ##NO_TEXT.
  CONSTANTS equals TYPE char2 VALUE 'EQ' ##NO_TEXT.
  CONSTANTS not_equals TYPE char2 VALUE 'NE' ##NO_TEXT.
  CONSTANTS greater_than TYPE char2 VALUE 'GT' ##NO_TEXT.
  CONSTANTS greater_equal TYPE char2 VALUE 'GE' ##NO_TEXT.
  CONSTANTS lesser_than TYPE char2 VALUE 'LT' ##NO_TEXT.
  CONSTANTS lesser_equal TYPE char2 VALUE 'LE' ##NO_TEXT.
  CONSTANTS contains_pattern TYPE char2 VALUE 'CP' ##NO_TEXT.
  CONSTANTS not_contains_pattern TYPE char2 VALUE 'NP' ##NO_TEXT.
  CONSTANTS contains_string TYPE char2 VALUE 'CS' ##NO_TEXT.
  CONSTANTS not_constains_string TYPE char2 VALUE 'NS' ##NO_TEXT.
  CONSTANTS is_null TYPE char2 VALUE 'IN' ##no_text.
  CONSTANTS is_not_null TYPE char2 VALUE 'NN' ##no_text.
  CONSTANTS not_in_subquery TYPE char2 VALUE 'S1' ##NO_TEXT.
  CONSTANTS in_subquery TYPE char2 VALUE 'S2' ##NO_TEXT.
  CONSTANTS exists_subquery TYPE char2 VALUE 'S3' ##NO_TEXT.
  CONSTANTS not_exists_subquery TYPE char2 VALUE 'S4' ##NO_TEXT.
ENDINTERFACE.
