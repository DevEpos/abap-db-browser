INTERFACE ZIF_DBBR_stmnt_string_builder
  PUBLIC .
  METHODS build_string
    CHANGING
      cs_statement TYPE ZIF_DBBR_fe_types=>ty_statement.

ENDINTERFACE.
