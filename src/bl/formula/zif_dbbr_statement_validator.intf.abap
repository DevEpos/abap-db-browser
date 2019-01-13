INTERFACE ZIF_DBBR_statement_validator
  PUBLIC .
  METHODS validate
    CHANGING
      cs_statement TYPE ZIF_DBBR_fe_types=>ty_statement
    RAISING
      ZCX_DBBR_fe_stmnt_valid_exc.
ENDINTERFACE.
