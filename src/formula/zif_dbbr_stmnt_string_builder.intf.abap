INTERFACE zif_dbbr_stmnt_string_builder
  PUBLIC.
  METHODS build_string
    CHANGING
      cs_statement TYPE zif_dbbr_fe_types=>ty_statement.

ENDINTERFACE.
