INTERFACE zif_dbbr_statement_validator
  PUBLIC.
  METHODS validate
    CHANGING
      cs_statement TYPE zif_dbbr_fe_types=>ty_statement
    RAISING
      zcx_dbbr_fe_stmnt_valid_exc.
ENDINTERFACE.
